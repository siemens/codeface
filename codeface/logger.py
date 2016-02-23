# This file is part of Codeface. Codeface is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.

"""This module provides a root logger for the codeface package.

On import, this module initializes the logger 'codeface' with a console logger
which logs to stdout.
If stdout is not redirected, the logging messages are couloured according to
their log level.

Note: If there is demand for more elaborate logging (multiple logfiles,
network logging, etc.) the logging.config module should probably be used
instead of the current handcoded configuration.

Methods:
    set_set_log_level: Set log level of console logger.
    start_logfile: Start logging to file.
    stop_logfile: Stop logging to file.

Attributes:
    logfile_handlers (dict): Contains handles of active logfiles.


"""

import logging
import os
from copy import copy

DEVINFO_LEVEL = 15
logging.DEVINFO = DEVINFO_LEVEL
logging.addLevelName(DEVINFO_LEVEL, "DEVINFO")

# Internal constants and definitions
# These are the ANSI sequences needed to get coloured ouput
RESET_SEQ = "\033[0m"
COLOR_SEQ = "\033[1;%dm"
BOLD_SEQ = "\033[1m"

# ANSI colour number offsets.
# The background is set with 40 plus the number of the colour,
# the foreground with 30 plus the number of the colour.
RED, GREEN, YELLOW, BLUE, WHITE = 1, 2, 3, 4, 7

# Colours which are used for the respective log levels.
COLORS = {
    'DEBUG': BLUE,
    'DEVINFO': WHITE,
    'INFO': GREEN,
    'WARNING': YELLOW,
    'ERROR': RED,
    'CRITICAL': RED,
}

logfile_handlers = {}


def set_log_level(level_string):
    """Change the log level of the console logger.

    Args:
        level_string (str): Future log level. Valid options are:
            "debug", "info", "warning" or "error"
    """

    console_handler.setLevel(_loglevel_from_string(level_string) - 1)


def start_logfile(filename, level_string):
    """Start logging to the file with the log level.

    Starts logging to the target file with the specified logging level.
    Valid options for the logging level are:
        debug, info, warning or error

    Args:
        filename (str): Filename of the logfile.
        level_string (str): Log level for the file. Valid options are:
            "debug", "info", "warning" or "error"
    """

    logfile_handler = _get_log_handler(file(filename, 'w'))
    logfile_handler.setLevel(_loglevel_from_string(level_string))
    log.devinfo("Opened logfile '%s' with log level '%s'",
                filename, level_string)
    log.addHandler(logfile_handler)
    logfile_handlers[filename] = logfile_handler


def stop_logfile(filename):
    """Stops logging to logfile.

    Args:
        filename (str): Log file to stop.
    """

    handler = logfile_handlers[filename]
    handler.flush()
    log.removeHandler(handler)
    handler.close()
    log.devinfo("Stopped logging into logfile '%s'", filename)


def _insert_seqs(message):
    """Insert the ANSI reset/bold sequences into a *message* that has been
    formatted with the $RESET and $BOLD pseudo-variables.

    Args:
        message:

    Returns:

    """

    return message.replace("$RESET", RESET_SEQ).replace("$BOLD", BOLD_SEQ)


def _remove_seqs(message):
    """Remove the $RESET and $BOLD pseudo-variables from a message."""

    return message.replace("$RESET", "").replace("$BOLD", "")


class _ColoredFormatter(logging.Formatter):
    """Utility class that adds terminal colour codes to log messages
    to improve the visibility of important messages.
    """

    def format(self, record):
        """

        Args:
            record:

        Returns:

        """

        levelname = record.levelname
        if levelname in COLORS:
            color_seq = COLOR_SEQ % (30 + COLORS[levelname])
            # Make a copy of this record, since other handlers may want to
            # print the record without colour codes
            record = copy(record)
            record.levelname = color_seq + levelname + RESET_SEQ
        return logging.Formatter.format(self, record)


def _loglevel_from_string(level_string):
    """Converts a log level string from e.g. the command line into a number"""

    return getattr(logging, level_string.upper())


def _get_log_handler(stream=None):
    """Return a log handler that by default prints to stderr, but can also be
    used to stream to file objects.
    Even if it streams to stderr, we have to check if the receiving stream
    is a TTY, since stderr could have been redirected into a file.
    """

    handler = logging.StreamHandler(stream=stream)
    format_string = "%(asctime)s [$BOLD%(name)s$RESET] " \
                    "%(processName)s %(levelname)s: %(message)s"
    datefmt = '%Y-%m-%d %H:%M:%S'

    if hasattr(handler.stream, "fileno") and os.isatty(handler.stream.fileno()):
        handler.setFormatter(
            _ColoredFormatter(_insert_seqs(format_string), datefmt=datefmt))
    else:
        handler.setFormatter(
            logging.Formatter(_remove_seqs(format_string), datefmt=datefmt))
    return handler


# Initialize the logger that prints to the console.
# The initial level of DEBUG will be overwritten by the command line parsing
console_handler = _get_log_handler()
console_handler.setLevel(logging.DEBUG)


class DevInfoLogger(logging.getLoggerClass()):
    """Logger with adjustable development levels."""

    def devinfo(self, msg, *args, **kwargs):
        """Initialize root codeface logger
        Note that the Level is set to 1, so all logging messages will be passed
        to the handlers, which will then apply their log level.

        Args:
            msg (str): Message string.
        """

        return self.log(DEVINFO_LEVEL, msg, *args, **kwargs)


# TODO Rework this!  Make it ALWAYS fires not only when the whole
# module is imported
logging.setLoggerClass(DevInfoLogger)
log = logging.getLogger("codeface")
log.addHandler(console_handler)
log.setLevel(1)  # pass all messages to the handlers
