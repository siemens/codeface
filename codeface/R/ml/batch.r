#! /usr/bin/env Rscript
# -*- R -*-
## This file is part of Codeface. Codeface is free software: you can
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

## NOTE: The dispatcher is supposed to be called from the main codeface
## directory; otherwise, we run into trouble with source()ed filed

s <- suppressPackageStartupMessages
source("../config.r", chdir=TRUE)
source("../db.r", chdir=TRUE)
source("../mc_helpers.r", chdir=TRUE)
source("ml_utils.r")
s(source("analysis.r"))
s(source("project.spec.r"))
s(source("keyword.list.r"))

s(library(logging))
s(library(tm))
s(library(tm.plugin.mail))
s(library(sna))
s(library(snatm))
s(library(ggplot2))
s(library(igraph))
s(library(lsa))
s(library(Rgraphviz)) # Available on bioconductor
s(library(lubridate))
s(library(optparse))
s(library(xtable))
s(library(reshape))
s(library(plyr))
source("../page.rank.r", chdir=TRUE)
rm(s)

## TODO: Filter out spam. There's an incredible amount in some gmane archives
## TODO: (this should also include filtering out non-english messages)
## The easiest thing would be to let SpamAssassin run over the mbox
## file, and then delete all messages marked as SPAM.

######################### Dispatcher ###################################
config.script.run({
    positional.args <- list("resdir", "mldir", "listname")

    conf <- config.from.args(positional.args=positional.args)

    gen.dir(conf$resdir)

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
    if (packageVersion("snatm") < "1.2.0") {
      stop("plyr needs to be available in version >= 1.2.0, please update.")
    }

    if (Sys.getenv("http_proxy") != "") {
      lw <- function(msg) logwarn(msg, logger="ml.batch")
      lw("WARNING: http_proxy is set!")
      lw("This is often unintended for local communication with the ID service.")
      lw("Are you sure this setup is correct? Continuing nevertheless.")
    }

    ## Email parser relies on locale and should be set to english
    Sys.setlocale(category="LC_ALL", locale ="en_US.UTF-8")

    conf <- init.mc(conf)
    dispatch.all(conf, conf$mldir, conf$resdir)
})
