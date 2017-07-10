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
## Copyright 2017, Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

## Support routines for handling DSMs produced by understand

s <- suppressPackageStartupMessages
s(library(igraph))

load.dsm <- function(dsm.filename) {
    if (is.na(file.info(dsm.filename)$size) || file.info(dsm.filename)$size==0) {
        return(NULL)
    }

    dat <- read.csv(dsm.filename)

    ## The code is checked out into a temporary directory, and understand
    ## stores absolute filenames. Make these relative to the project root.
    dat$From.File <- gsub("/tmp/Rtmp\\w\\w\\w\\w\\w\\w/code/", "",
                          dat$From.File, perl=TRUE)
    dat$To.File <- gsub("/tmp/Rtmp\\w\\w\\w\\w\\w\\w/code/", "",
                        dat$To.File, perl=TRUE)

    ## All dependency data frames use the same format: V1 and V2 for
    ## source and destination, and weight for edge weights. In our case.
    ## From.File, To.File and References fulfil these roles for us.
    dat <- dat[,c("From.File", "To.File", "References")]
    colnames(dat) <- c("V1", "V2", "weight")

    return(dat)
}
