#! /usr/bin/env Rscript
# -*- R -*-
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

## NOTE: The dispatcher is supposed to be called from the main prosoda
## directory; otherwise, we run into trouble with source()ed filed

s <- suppressPackageStartupMessages
source("config.r")
source("db.r")
source("ml/ml_utils.r")
s(source("ml/analysis.r"))
s(source("ml/project.spec.r"))
s(source("ml/keyword.list.r"))

s(library(tm))
s(library(parallel))
s(library(tm.plugin.mail))
s(library(sna))
s(library(ggplot2))
s(library(igraph))
s(library(lsa))
s(library(Rgraphviz)) # Available on bioconductor
s(library(lubridate))
s(library(optparse))
s(library(xtable))
s(library(reshape))
s(library(plyr))
rm(s)

## NOTE: This is _temporary_. After the changes to snatm are upstreamed,
## we can get rid of loading the files directly.
snatm.path <- "../src.nntp/snatm/pkg/R"
s(source.files(snatm.path))


## TODO: Filter out spam. There's an incredible amount in some gmane archives
## TODO: (this should also include filtering out non-english messages)
## The easiest thing would be to let SpamAssassin run over the mbox
## file, and then delete all messages marked as SPAM.

######################### Dispatcher ###################################
option_list <- list(
                 make_option(c("", "--basedir"), type="character", default="./",
                             help="Base directory for prosoda"),
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

if (!(file.exists("prosoda.conf"))) {
  stop("prosoda.conf not found in current directory!")
}
global.conf <- load.global.config("prosoda.conf")

conf <- init.db(conf, global.conf)

if (is.null(conf$ml)) {
  cat("No mailing list repository available for project, skipping analysis\n")
  stop()
}

resdir <- file.path(resdir, conf$project, "ml")
gen.dir(resdir)

## Provide a frame dump in case of failure
if (!interactive()) {
  options(error = quote(dump.frames("error.dump", TRUE)))
} else {
  options(error=recover)
}

set.seed(19101978) ## Fix the seed to make results of random algorithms reproducible

if (packageVersion("tm") < "0.5.9") {
  stop("tm needs to be available in version >= 0.5.9, please update.")
}
if (packageVersion("tm.plugin.mail") < "0.0.6") {
  stop("tm.plugin.mail needs to be available in version >= 0.0.6, please update.")
}
if (packageVersion("plyr") < "1.8.0") {
  stop("plyr needs to be available in version >= 1.8.0, please update.")
}

if (Sys.getenv("http_proxy") != "") {
  cat("WARNING: http_proxy is set!\n")
  cat("This is often unintended for local communication with the ID service.\n")
  cat("Are you shure this setup is correct? Continuing nevertheless.\n")
}

if (opts$nodes > 1) {
  options(mc.cores=opts$nodes)
} else {
  ## Setting mc.cores to 1 makes sure that a regular lapply is used.
  options(mc.cores=1)
}

dispatch.all(conf, repo.path, resdir)


