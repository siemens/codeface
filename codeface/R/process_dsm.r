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

load.sdsm <- function(sdsm.filename) {
    sdsm.size <- NULL
    tryCatch({
        sdsm.size <- read.table(sdsm.filename, skip=1, nrows=1)[[1]]
    }, error=function(e) {
        ## If the file only has content "null", then Titan did not
        ## produce valid data
        sdsm.size <<- NULL
    })
    if (is.null(sdsm.size)) { return(NULL) }

    sdsm.filenames <- read.table(sdsm.filename, skip=2+sdsm.size)

    ## Titan stores absolute filenames, and also replaces slashes (/) with dots (.).
    ## Since we run Titan on a checked out state of the repository in the
    ## temporary location /tmp/Rtmpxxxxxx/code/, this gives prefixes like
    ## .tmp.RtmpZ11Ycl.code. that need to be eliminated from the filename
    sdsm.filenames$V1 <- gsub("\\.tmp\\.Rtmp\\w\\w\\w\\w\\w\\w\\.code\\.", "",
                              sdsm.filenames$V1, perl=TRUE)

    sdsm.binary <- read.table(sdsm.filename, skip=2, nrows=sdsm.size)
    sdsm.binary[sdsm.binary > 0] <- 1
    sdsm.binary <- as.matrix(sdsm.binary)
    colnames(sdsm.binary) <- as.vector(sdsm.filenames)[[1]]
    rownames(sdsm.binary) <- as.vector(sdsm.filenames)[[1]]
    sdsm.graph <- graph.adjacency(sdsm.binary, mode="directed")
    sdsm.edgelist <- as.data.frame(get.edgelist(sdsm.graph))

    return(sdsm.edgelist)
}
