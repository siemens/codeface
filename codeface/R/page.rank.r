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
## Copyright 2010, 2011 by Wolfgang Mauerer <wm@linux-kernel.net>
## Copyright 2012, 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## NOTE: This file needs to be sourced _after_ the igraph
## library has been loaded

## Hacky fix for problems with page rank that are (presumably) caused by
## problems in the FORTRAN implementation of igraphdneupd (error 1: Schur form
## computed by LAPACK routine dlahqr). Once this is properly fixed in the
## FORTRAN code, this work-around needs to go away.
## TODO: Acutally, I'm not sure if copying the graph for each try or re-setting
## the RNG is respoinsible for the "fix".

page.rank.orig <- page.rank

MAX.RETRIES <- 10
page.rank <- function(g, ...) {
  success <- FALSE
  res <- NULL
  i <- 1

  repeat {
    tryCatch({
      g.work <- g
      res <- page.rank.orig(g.work, ...)
      success <- TRUE
    }, error=function(e) {
      logwarning("Numerical problems with page.rank encountered, trying to fix up",
                 logger="page.rank")      
    })

    if (success) {
      break
    } else {
      ## Try with a different random seed
      i <- 1 + 1
      set.seed(i)
    }

    if (i >= MAX.RETRIES) {
      break
    }
  }

  if (is.null(res)) {
    stop("Internal error: Fix for page.rank failed!")
  }
  return(res)
}
