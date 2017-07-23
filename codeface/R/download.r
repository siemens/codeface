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
## Copyright 2017 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

## Download mailing list archive from mod_mbox sources
s <- suppressPackageStartupMessages
s(library(RCurl))
s(library(lubridate))
s(library(stringr))
s(library(optparse))
rm(s)

## TODO: From stackoverflow to prevent Unicode problems with erroneous encodings.
## Determine license and comply with license information
myGetURL <- function(...) {
    rcurlEnv <- getNamespace("RCurl")
    mapUnicodeEscapes <- get("mapUnicodeEscapes", rcurlEnv)
    unlockBinding("mapUnicodeEscapes", rcurlEnv)
    assign("mapUnicodeEscapes", function(str) str, rcurlEnv)
    on.exit({
        assign("mapUnicodeEscapes", mapUnicodeEscapes, rcurlEnv)
        lockBinding("mapUnicodeEscapes", rcurlEnv)
    }, add = TRUE)
    return(getURL(...))
}

## Iterate over all admissible months for a given project, download mbox
## files, concatenate them and save then to outdir
get.mboxes <- function(dat, base.url, outdir) {
    file.out <- file.path(outdir, str_c(dat$ml.id, ".mbox"))
    if (file.exists(file.out)) {
        print(str_c("NOTE: Erasing existing output file ", file.out))
    }

    res <- do.call(rbind, lapply(seq(dat$start.year, dat$end.year), function (year) {
       return(data.frame(month=1:12, year=year))
    }))

    res <- res[!(res$year==dat$start.year & res$month < dat$start.month),]
    res <- res[!(res$year==dat$end.year & res$month > dat$end.month),]

    dummy <- lapply(1:nrow(res), function(i) {
        url <- str_c(base.url, "/", dat$ml.id, "/", res$year[i], str_pad(res$month[i], 2, "left", 0), ".mbox")
        print(str_c("  -> downloading ", url))
        mbox <- myGetURL(url)
        write(mbox, file.out, append=TRUE)
    })
}

download.mbox <- function(base.url, inv.file, outdir) {
    dat <- read.table(inv.file, header=FALSE)
    colnames(dat) <- c("project", "ml.id", "start.month", "start.year", "end.month", "end.year")

    lapply(1:nrow(dat), function(i) {
        print(str_c("\n\nProcessing ", dat$project[i]))
        get.mboxes(dat[i,], base.url, outdir)
    })

}

## #################################################################################
parser <- OptionParser(usage = "%prog <base.url> <inventory file> <outdir>")
arguments <- parse_args(parser, positional_arguments = TRUE)
opts <- arguments$options

if (length(arguments$args) != 3) {
  print_help(parser)

  cat("Mandatory positional arguments:\n")
  cat("   <url>: base url\n")
  ## For instance: http://mail-archives.apache.org/mod_mbox/
  cat("   <inventory file>: description of mailbox sources\n")
  cat("   <outdir>: output directory\n")
  stop()
} else {
  base.url <- arguments$args[1]
  inv.file <- arguments$args[2]
  outdir <- arguments$args[3]
}

download.mbox(base.url, inv.file, outdir)
