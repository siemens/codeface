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
suppressPackageStartupMessages(library(yaml))
suppressPackageStartupMessages(library(logging))
## set up the basic logging config in case logging is called at source() level
basicConfig()

source("db.r")

logdebug.config <- function(conf) {
  ## Log the contents of a conf object to the debug stream
  logdebug("Configuration options:")
  for (n in names(conf)) {
    logdebug(paste(n, ":", conf[n]))
  }
}

## Load global and local configuration and apply some sanity checks
## TODO: More sanity checks, better merging of conf objects
load.config <- function(global.file, project.file=NULL) {
  logdevinfo(paste("Loading global config file '", global.file, "'", sep=""), logger="config")
  if (!(file.exists(global.file))) {
      stop(paste("Global configuration file", global.file, "does not exist!"))
  }
  conf <- yaml.load_file(global.file)

  if (is.null(conf$dbhost) || is.null(conf$dbname) ||
      is.null(conf$dbuser) || is.null(conf$dbpwd)) {
    stop("Malformed global configuration: Database information is incomplete!\n")
  }

  if (is.null(conf$idServiceHostname)) {
    conf$idServiceHostname <- "127.0.0.1"
  }

  if (is.null(conf$idServicePort)) {
    conf$idServicePort <- 8080
  } else {
    conf$idServicePort <- as.integer(conf$idServicePort)
  }

  if (is.null(project.file)) {
      return(conf)
  }
  logdevinfo(paste("Loading project config file '", project.file, "'", sep=""),
             logger="config")
  if (!(file.exists(project.file))) {
      stop(paste("Project configuration file", project.file, "does not exist!"))
  }

  ## Append project configuration to conf
  conf <- c(conf, yaml.load_file(project.file))

  if (is.null(conf$project) || is.null(conf$repo)) {
    stop("Malformed configuration: Specify project and repository!\n")
  }

  if (conf$tagging != "tag" && conf$tagging != "committer2author" &&
      conf$tagging != "proximity" && conf$tagging != "file") {
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

## Set the default prosoda conf relative to this file
default.prosoda.conf <- normalizePath("../../prosoda.conf")
## Set the default log file
default.prosoda.log <- str_c(normalizePath("../../log"), "/prosoda.log.R.", Sys.getpid())

## This function parses the command line accepting some default parameters,
## specifically the prosoda and project configuration files. It then creates
## a conf object and amends it with a database connection.
## If positional arguments are given by name in the positional.args list,
## these are stored by name in the conf object.
config.from.args <- function(positional.args=list(), extra.args=list(),
                             require.project=TRUE) {
  option_list <- c(list(
    make_option(c("-l", "--loglevel"), default="info",
                help="logging level (debug, devinfo, info, warning or error) [%default]"),
    make_option(c("-f", "--logfile"), help="logfile", default=default.prosoda.log),
    make_option(c("-c", "--config"), default=default.prosoda.conf,
                help="global prosoda configuration file [%default]"),
    make_option(c("-p", "--project"), help="project configuration file",
                default=NULL),
    make_option(c("-j", "--jobs"), type="integer", default=1,
                help="Number of parallel jobs for analysis"),
    make_option("--profile", help="Measure and store profiling data",
                action="store_true", default=FALSE)
  ), extra.args)

  ## Note that positional_arguments=TRUE even if no positional arguments are
  ## required - this is necessary since otherwise the parser output differs
  parser <- OptionParser(usage=do.call(paste, c("%prog", positional.args)),
                         option_list=option_list)
  arguments <- parse_args(parser, positional_arguments=TRUE)
  opts = arguments[1]$options
  args = arguments[2]$args

  ## Set up logging handlers/logfiles
  config.logging(opts$loglevel, opts$logfile)

  ## Check options for correctness
  if (length(args) != length(positional.args)) {
    print_help(parser)
    stop("Wrong number of positional arguments!")
  }
  if (require.project & is.null(opts$project)) {
    stop("No project configuration file specified!")
  }

  ## Load configuration file(s)
  conf <- load.config(opts$config, opts$project)

  ## Open up the corresponding database connection
  if(is.null(opts$project)) {
    conf <- init.db.global(conf)
  } else {
    conf <- init.db(conf)
  }

  ## Store positional arguments under their names in the conf object
  conf[unlist(positional.args)] <- args
  for (n in extra.args) {
    conf[n@dest] <- opts[n@dest]
  }

  ## Store other options that need to be propagated upwards
  conf$profile <- opts$profile
  conf$jobs <- opts$jobs

  logdebug.config(conf)
  return(conf)
}

## Setup the logging package given the log level string and an optional logfile
config.logging <- function(level, logfile) {
  logReset()
  setLevel(loglevels["DEBUG"], getLogger())
  addHandler(writeToConsole, level=loglevels[toupper(level)], formatter=config.logging.formatter)
  logdebug(paste("Set log level to '", toString(level), "' == ", loglevels[toupper(level)], sep=""),
           logger="config")
  if (!is.null(logfile)) {
    logdevinfo(paste("Opening log file '", logfile, "'", sep=""), logger="config")
    addHandler(writeToFile, level=loglevels["DEBUG"], file=logfile, formatter=config.logging.formatter)
  }
}

loglevels["DEVINFO"] <- 15
loglevels["FATAL"] <- 50

## A new logging formatter that is similar to the python formatter
config.logging.formatter <- function(record) {
  if (is.null(record$logger) || record$logger == "") {
    from <- "[prosoda.R]"
  } else {
    from <- paste("[prosoda.R.", record$logger, "]", sep="")
  }
  if (record$level == loglevels["DEVINFO"]) {
    record$levelname <- "DEVINFO"
  }
  text <- paste(record$timestamp, from, paste(record$levelname, ": ", record$msg, sep=''))
}

## Add own functions that set the default log level to DEBUG, so that the handlers can filter out messages
logdebug <- function(msg, ..., logger="") { mylevellog(loglevels["DEBUG"], msg, ..., logger=logger) }
logdevinfo <- function(msg, ..., logger="") { mylevellog(loglevels["DEVINFO"], msg, ..., logger=logger) }
loginfo <- function(msg, ..., logger="") { mylevellog(loglevels["INFO"], msg, ..., logger=logger) }
logwarning <- function(msg, ..., logger="") { mylevellog(loglevels["WARNING"], msg, ..., logger=logger) }
logerror <- function(msg, ..., logger="") { mylevellog(loglevels["ERROR"], msg, ..., logger=logger) }
logfatal <- function(msg, ..., logger="") { mylevellog(loglevels["FATAL"], msg, ..., logger=logger) }
mylevellog <- function(lvl, msg, ..., logger="") {
  setLevel(loglevels["DEBUG"], getLogger(logger))
  levellog(lvl, msg, ..., logger=logger)
}

## Run a script in a tryCatch environment that catches errors and either terminates
## the script with an error code, or in an interactive environment calls stop() again
config.script.run <- function(expr) {
  ## Some notes on the muffleWarning restart:
  ## http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg21676.html
  withCallingHandlers(expr,
    error=function(e) {
      ## In a noninteractive shell, try to give as much useful information
      ## about the error as possible to ease error reporting
      if (!interactive()) {
        ## Extract information from the frames
        ## (see also code of R builtin debugger function)
        dump.frames("error.dump")
        n <- length(error.dump)
        calls <- names(error.dump)
        trace <- "Traceback:\n"
        for (i in 2L:n-2) {
          trace <- paste(trace, formatC(i, width=3), ": ",
                         gsub("\n","\n     ", calls[i]), "\n",
                         sep="")
        }
        loginfo(trace)
        logfatal(e$message)
        ## Save the dump to file for later analysis
        save("error.dump", file="error.dump.rda")
        loginfo("Error dump was written to 'error.dump.rda'.")
        loginfo("To debug, launch R and run 'load(\"error.dump.rda\"); debugger(error.dump)'")
        quit(save="no", status=1)
      } else {
        recover()
      }
    },
    warning=function(w) {
      logdebug(w$message)
      invokeRestart("muffleWarning")
    }
  )
}

