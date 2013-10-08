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

## Some helper routine for multi-core issues
suppressPackageStartupMessages(library(parallel))

init.mc <- function(conf) {
  if (!is.null(conf$jobs) && conf$jobs > 1) {
    options(mc.cores=conf$jobs)
  } else {
    ## Setting mc.cores to 1 makes sure that a regular lapply is used
    ## even is mclapply is called.
    options(mc.cores=1)
  }
}

## When computations that access the database are run in parallel,
## we need to make sure that every thread get an own DB connection object.
## To simplify the mechanics, the following wrapper is provided.
mclapply.db <- function(conf, X, FUN, ...) {
  res <- mclapply(X, function(i) {
    ## A database connection is required for every worker thread.
    conf <- init.db.global(conf)
    res.local <- FUN(conf, i)
    dbDisconnect(conf$con)

    return(res.local)
  })

  ## Check for errors in mclapply
  for (r in res) {
    if (inherits(r, "try-error")) {
      stop(r)
    }
  }

  return(res)
}
