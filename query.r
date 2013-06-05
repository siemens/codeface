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

## Helper functions to query data from the database. All routines
## in this file should only use a-priori knowledge that is available
## in the configuration object, not any other state.

suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(stringr))

## Obtain the series.merged object constructed in do.ts.analysis
query.series.merged <- function(conf, subset=NULL) {
  res <- lapply(c("Averaged (small window)", "Averaged (large window)",
                  "Cumulative"),
                function(type) {
                  plot.id <- get.plot.id(conf, str_c("Progress TS [", type, "]"))
                  query <- str_c("SELECT time, value, value_scaled ",
                                 "FROM timeseries where plotId=",
                                 plot.id)
                  if (!is.null(subset)) {
                    ## TODO: Handle the case of subset selection
                    ## by modifying query appropriately
                  }

                  dat <- dbGetQuery(conf$con, query)
                  dat$type <- type
                  dat$time <- ymd_hms(dat$time, quiet=T)
                  colnames(dat)[3] <- "value.scaled"

                  return(dat)
                })

  return(do.call(rbind, res))
}

get.commits.by.ranges <- function(conf, subset=NULL, FUN=NULL) {
  ts <- vector("list", length(conf$revisions)-1)
  tstamps <- conf$tstamps.release

  for (i in 1:(dim(tstamps)[1]-1)) {
    range.id <- get.range.id(conf, tstamps$tag[i], tstamps$tag[i+1])
    dat <- dbGetQuery(conf$con, str_c("SELECT * FROM commit where projectId=",
                                      conf$pid, " AND releaseRangeId=", range.id))

    if (!is.null(FUN)) {
      dat <- FUN(dat, subset)
    }

    if (dim(dat)[1] == 0) {
      cat("Skipping empty cycle", tstamps$tag[i], "..", tstamps$tag[i+1])
      next
    }

    ts[[i]] <- cbind(data.frame(revision=tstamps$tag[[i+1]],
                                date=tstamps$date[[i+1]]), dat)
  }

  return(ts)
}

### General SQL helper functions
## Test if a table is empty (returns false) or not (returns true)
table.has.entries <- function(conf, table) {
  dat <- dbGetQuery(conf$con, str_c("SELECT * from ", table))

  return (dim(dat)[1] > 0)
}
