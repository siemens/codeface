## This file is part of prosoda.  prosoda is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.
'''
Module providing a root logger 'log' for the prosoda python package.

On import, initializes the logger 'prosoda' with a console logger
logging to stderr. If stderr is not redirected, the logging messages
are couloured according to their level.

* set_log_level(level_string) changes the log level of the console logger
to the level given as a string (debug, info, warning or error)

* start_logfile(filename, level_string) starts additionally logging to
the file with the given file name. Note that the log file is truncated.

* stop_logfile(filename) stops logging to the given logfile.
'''
import logging
import os
from copy import copy

def set_log_level(level_string):
    '''
    Change the log level of the console logger to the level given in
    level_string, which can be debug, info, warning or error.
    '''
    console_handler.setLevel(loglevel_from_string(level_string)-1)

def start_logfile(filename, level_string):
    '''
    Start logging to the file specified by filename using the log level
    specified in level_string.
    '''
    logfile_handler = get_log_handler(file(filename, 'w'))
    logfile_handler.setLevel(loglevel_from_string(level_string))
    log.info("Opened logfile '{}' with log level '{}'"
            "".format(filename, level_string))
    log.addHandler(logfile_handler)
    logfile_handlers[filename] = logfile_handler

def stop_logfile(filename):
    '''Stop logging to the specified log file'''
    handler = logfile_handlers[filename]
    handler.flush()
    log.removeHandler(handler)
    handler.close()
    log.info("Stopped logging into logfile '{}'".format(filename))

# Internal constants and definitions follow

# These are the ANSI sequences needed to get coloured ouput
RESET_SEQ = "\033[0m"
COLOR_SEQ = "\033[1;%dm"
BOLD_SEQ = "\033[1m"

# ANSI colour number offsets.
# The background is set with 40 plus the number of the colour,
# the foreground with 30 plus the number of the colour.
RED, YELLOW, BLUE, WHITE = 1, 3, 4, 7

# Colours which are used for the respective log levels.
COLORS = {
    'DEBUG' : BLUE,
    'INFO' : WHITE,
    'WARNING' : YELLOW,
    'ERROR' : RED,
    'CRITICAL' : RED,
}

logfile_handlers = {}

def insert_seqs(message):
    '''
    Insert the ANSI reset/bold sequences into a message that has been
    formatted with the $RESET and $BOLD pseudo-variables.
    '''
    return message.replace("$RESET", RESET_SEQ).replace("$BOLD", BOLD_SEQ)

def remove_seqs(message):
    '''Remove the $RESET and $BOLD pseudo-variables from a message.'''
    return message.replace("$RESET", "").replace("$BOLD", "")

class ColoredFormatter(logging.Formatter):
    '''
    Utility class that adds terminal colour codes to log messages
    to improve the visibility of important messages.
    '''
    def format(self, record):
        levelname = record.levelname
        if levelname in COLORS:
            color_seq = COLOR_SEQ % (30 + COLORS[levelname])
            # Make a copy of this record, since other handlers may want to
            # print the record without colour codes
            record = copy(record)
            record.levelname = color_seq + levelname + RESET_SEQ
        return logging.Formatter.format(self, record)

def loglevel_from_string(level_string):
    '''Converts a log level string from e.g. the command line into a number'''
    return getattr(logging, level_string.upper())

def get_log_handler(stream=None):
    '''
    Return a log handler that by default prints to stderr, but can also be
    used to stream to file objects.
    Even if it streams to stderr, we have to check if the receiving stream
    is a TTY, since stderr could have been redirected into a file.
    '''
    handler = logging.StreamHandler(stream=stream)
    FORMAT = "[$BOLD%(name)-20s$RESET][%(levelname)-19s] %(message)s"
    if hasattr(handler.stream, "fileno") and os.isatty(handler.stream.fileno()):
        handler.setFormatter(ColoredFormatter(insert_seqs(FORMAT)))
    else:
        handler.setFormatter(logging.Formatter(remove_seqs(FORMAT)))
    return handler

# Initialize the logger that prints to the console.
# The initial level of DEBUG will be overwritten by the command line parsing
console_handler = get_log_handler()
console_handler.setLevel(logging.DEBUG)

# Initialize root prosoda logger
# Note that the Level is set to 1, so all logging messages will be passed to
# the handlers, which will then apply their log level.
log = logging.getLogger("prosoda")
log.addHandler(console_handler)
log.setLevel(1) # pass all messages to the handlers
