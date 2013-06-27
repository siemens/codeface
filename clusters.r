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

## Definitions for clustering

cluster.methods <- c("Spin Glass Community", "Random Walk Community")
pagerank.types <- c(0, 1) ## 0 mean regular, 1 means transposed adjacency matrix

cluster.method.valid <- function(cluster.method) {
  return(any(cluster.method %in% cluster.methods))
}

pagerank.type.valid <- function(pr.type) {
  return(any(pr.method %in% pr.type))
}
