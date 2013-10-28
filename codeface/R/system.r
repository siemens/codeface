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
## Copyright 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

library(stringr)

## Given a finished command line in cmd, execute it
do.system.raw <- function(cmd) {
  logdevinfo(str_c("Calling command ", cmd), logger="system")

  tryCatch(res <- system(cmd, ignore.stderr=TRUE, intern=TRUE),
           error = function(e) {
             stop("Could not execute ", cmd, "\n")
           })

  return(res)
}

## Given the name of an executable and arguments, construct a
## command line and execute it.
do.system <- function(cmd, args) {
  cmd.which <- Sys.which(cmd)
  if (cmd.which == "") {
    stop("Internal error: Required command ", cmd, " not found, aborting.\n")
  }

  cmd <- str_c(cmd.which, " ", args)

  return(do.system.raw(cmd))
}
