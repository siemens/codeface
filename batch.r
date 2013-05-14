#! /usr/bin/env Rscript
# -*- R -*-
## This file is part of prosoda.mail.  prosoda is free software: you can
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
# NOTE: The curious source()s from ../prosoda will vanish once
# the code bases are merged together
source("../prosoda/config.r")
source("../prosoda/db.r")
source("local.r")

## TODO: Filter out spam. There's an incredible amount in some gmane archives
## TODO: (this should also include filtering out non-english messages)
## The easiest thing would be to let SpamAssassin run over the mbox
## file, and then delete all messages marked as SPAM.

######################### Dispatcher ###################################
option_list <- list(
                 make_option(c("", "--use_db"), action="store_true",
                             default=FALSE,
                             help="Use precomputed results"),
                 make_option(c("", "--basedir"), type="character", default="./",
                             help="Base directory for prosoda.nntp"),
                 make_option(c("-n", "--nodes"), type="integer", default=1,
                             help=paste("Number of nodes for cluster analysis",
                               "(1 means local processing)"))
                 )
parser <- OptionParser(usage = "%prog [options] <resdir> <mldir> <config>",
                       option_list=option_list)
arguments <- parse_args(parser, positional_arguments = TRUE)
opts <- arguments$options

if (length(arguments$args) != 3) {
  print_help(parser)

  cat("Mandatory positional arguments:\n")
  cat("   <resdir>: Base directory for result outputs\n")
  cat("   <mldir>: Base directory for mailing list inputs\n")
  cat("   <config>: Project-specific configuration file\n\n")
  stop()
} else {
  resdir <- arguments$args[1]
  repo.path <- arguments$args[2]
  config.file <- arguments$args[3]
}

conf <- load.config(config.file)
# NOTE: Loading the global config via three corners will go away once
# the code bases are merged
global.conf <- load.global.config("../prosoda/prosoda.conf")

conf <- init.db(conf, global.conf)

if (is.null(conf$ml)) {
  cat("No mailing list repository available for project, skipping analysis\n")
  stop()
}

resdir <- file.path(resdir, conf$project, "ml")
gen.dir(resdir)

## Provide a frame dump in case of failure
options(error = quote(dump.frames("error.dump", TRUE)))

## NOTE: This is _temporary_. After a proper package has been
## created, we can get rid of loading the files directly (includes.r
## load snatm via snatm.path)
snatm.path <- "../src.nntp/snatm/pkg/R"
source("includes.r")
set.seed(19101978) ## Fix the seed to make results of random algorithms reproducible

## TODO: Make log file configurable
if (opts$nodes > 1) {
  tm_startCluster(numCPUs=opts$nodes, outfile="/dev/tty")
}

dispatch.all(conf, repo.path, resdir, doCompute=!opts$use_db)

if (opts$nodes > 1) {
  tm_stopCluster()
}


