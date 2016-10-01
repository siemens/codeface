#! /usr/bin/env Rscript
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
## Copyright 2016, Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## All Rights Reserved.

## Support routines for handling DSMs produced by the Titan toolset

s <- suppressPackageStartupMessages
s(library(igraph))

load.sdsm <- function(sdsm.filename, relavent.files) {
    sdsm.size <- read.table(sdsm.filename, skip=1, nrows=1)[[1]]
    sdsm.filenames <- read.table(sdsm.filename, skip=2+sdsm.size)
    sdsm.binary <- read.table(sdsm.filename, skip=2, nrows=sdsm.size)
    sdsm.binary[sdsm.binary > 0] <- 1
    sdsm.binary <- as.matrix(sdsm.binary)
    colnames(sdsm.binary) <- as.vector(sdsm.filenames)[[1]]
    rownames(sdsm.binary) <- as.vector(sdsm.filenames)[[1]]
    sdsm.graph <- graph.adjacency(sdsm.binary, mode="directed")
    sdsm.edgelist <- as.data.frame(get.edgelist(sdsm.graph))

    return(sdsm.edgelist)
}
