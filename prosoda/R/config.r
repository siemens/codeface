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

suppressPackageStartupMessages(library(optparse))
library(yaml)
library(logging)
basicConfig()


source("db.r")

## Load global and local configuration and apply some sanity checks
## TODO: More sanity checks, better merging of conf objects
load.config <- function(global_file, project_file=NULL) {
  loginfo(paste("Loading global config file '", global_file, "'", sep=""))
  if (!(file.exists(global_file))) {
      stop(paste("Global configuration file", global_file, "does not exist!"))
  }
  conf <- yaml.load_file(global_file)

  if (is.null(conf$dbhost) || is.null(conf$dbname) ||
      is.null(conf$dbuser) || is.null(conf$dbpwd)) {
    stop("Malformed global configuration: Database information is incomplete!\n")
  }

  if (is.null(conf$nodejsHostname)) {
    conf$nodejsHostname <- "127.0.0.1"
  }

  if (is.null(conf$nodejsPort)) {
    conf$nodejsPort <- 8080
  } else {
    conf$nodejsPort <- as.integer(conf$nodejsPort)
  }

  if (is.null(project_file)) {
      return(conf)
  }
  loginfo(paste("Loading project config file '", project_file, "'", sep=""))
  if (!(file.exists(project_file))) {
      stop(paste("Project configuration file", project_file, "does not exist!"))
  }

  # Append project configuration to conf
  conf <- c(conf, yaml.load_file(project_file))

  if (is.null(conf$project) || is.null(conf$repo)) {
    stop("Malformed configuration: Specify project and repository!\n")
  }

  if (conf$tagging != "tag" && conf$tagging != "committer2author" &&
      conf$tagging != "proximity") {
    stop("Malformed configuration: Invalid tagging mode specified!")
  }

  if(length(conf$revisions) < 2) {
    stop("Malformed configuration: Revision list must include at least 2 commits!")
  }

  if (length(conf$rcs) > 0 && (length(conf$revisions) != length(conf$rcs))) {
    stop("Malformed configuration: Revision and rcs lists must have same length!")
  }

  return(conf)
}

## This function parses the command line accepting some default parameters,
## specifically the prosoda and project configuration files. It then creates
## a conf object and amends it with a database connection.
## If positional arguments are given by name in the positional_args list,
## these are stored by name in the conf object.
config.from.args <- function(positional_args=list(), extra_args=list()) {
    option_list <- c(list(
        make_option(c("-l", "--loglevel"), default="info",
                    help="logging level (debug, info, warning or error) [%default]"),
        make_option(c("-f", "--logfile"), help="logfile"),
        make_option(c("-c", "--config"), default="prosoda.conf",
                    help="global prosoda configuration file [%default]"),
        make_option(c("-p", "--project"), help="project configuration file",
                    default=NULL)
    ), extra_args)
    parser <- OptionParser(usage=do.call(paste, c("%prog", positional_args)),
                           option_list=option_list)

    arguments <- parse_args(parser, positional_arguments=(length(positional_args)>0))
    opts = arguments[1]$options
    args = arguments[2]$args
    config.logging(opts$loglevel, opts$logfile)
    if (length(args) != length(positional_args)) {
        logfatal("Wrong number of positional arguments!")
        print_help(parser)
        return(NULL)
    }

    # Load configuration file(s)
    conf <- load.config(opts$config, opts$project)

    # Open up the corresponding database connection
    if(is.null(opts$project)) {
        conf <- init.db.global(conf)
    } else {
        conf <- init.db(conf)
    }

    # Store positional arguments under their names in the conf object
    conf[unlist(positional_args)] = arguments$args
    return(conf)
}

## Setup the logging package given the log level string and an optional logfile
config.logging <- function(level, logfile) {
    logReset()
    setLevel(toupper(level), getLogger())
    addHandler(writeToConsole, level=toupper(level), formatter=config.logging.formatter)
    logdebug(paste("Set log level to '", toString(level), "' == ", loglevels[toupper(level)], sep=""))
    if (!is.null(logfile)) {
        loginfo(paste("Opening log file '", logfile, "'", sep=""))
        addHandler(writeToFile, file=logfile, formatter=config.logging.formatter)
    }
}

config.logging.formatter <- function(record) {
    if (is.null(record$logger) || record$logger == "") {
        from <- "[prosoda.R]"
    } else {
        from <- paste("[prosoda.R.", record$logger, "]", sep="")
    }
    text <- paste(record$timestamp, from, paste(record$levelname, ": ", record$msg, sep=''))
}

logfatal <- function(msg, ..., logger="") { levellog(50, msg, ..., logger) }

## TODO: Turn this into a proper pipeline, or some plugin-based
## analysis mechanism?
if (!interactive()) {
  options(error = quote(dump.frames("error.dump", TRUE)))
} else {
  options(error=recover)
}
