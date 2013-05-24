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

