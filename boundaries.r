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

## Helper functions for constructing release boundary information

get.boundaries <- function(i, conf) {
  tstamps.release <- conf$tstamps.all[conf$tstamps.all$type=="release",]
  tstamps.rc <- conf$tstamps.all[conf$tstamps.all$type=="rc",]
  tag <- conf$revisions[conf$revisions==tstamps.release[i+1,]$tag]
  tag.start <- conf$revisions[conf$revisions==tstamps.release[i,]$tag]

  rc <- NA
  rc <- tstamps.rc[i,]$date

  return(data.frame(date.start=tstamps.release[i,]$date,
                    date.end=tstamps.release[i+1,]$date,
                    date.rc_start=rc,
                    tag=tag,
                    cycle=paste(tag.start, tag, sep="-")))
}

prepare.release.boundaries <- function(conf) {
  ## TODO: The information should be constructed from an sql query,
  ## and not be constructed with the above magic.
  len <- dim(conf$tstamps.all[conf$tstamps.all$type=="release",])[1]-1
  res <- lapply(1:len, function(i) {
    return(get.boundaries(i, conf))
  })

  res <- do.call(rbind, res)
  return(res)
}

