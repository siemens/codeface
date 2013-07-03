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

## Helper functions to process/visualise initiation-response structures stored
## in the database

## ir needs to be obtained by query.initiate.response
## Assign colours to persons depending on their relative
## importance in the network as measured by their degree
prepare.initiate.response <- function(ir, threshold1=0.25, threshold2=0.5) {
  ir$col <- "Low"
  ir$col[ir$deg > threshold1 & ir$deg < threshold2] <- "Medium"
  ir$col[ir$deg >= threshold2] <- "High"

  return(ir)
}

plot.init.response <- function(ir) {
  g1 <- ggplot(ir, aes(x=initiations, y=responses)) +
    geom_point(aes(size=deg, colour=col)) +
    scale_x_sqrt() + scale_y_sqrt() + ggtitle(conf$project) +
    facet_grid(source~.) +
    xlab("Messages initiated (sqrt scale)") +
    ylab("Responses given (sqrt scale)")

  g2 <- ggplot(ir, aes(x=initiations, y=responses.received)) +
    geom_point(aes(size=deg, colour=col)) +
    scale_x_sqrt() + scale_y_sqrt() + ggtitle(conf$project) +
    facet_grid(source~.) +
    xlab("Messages initiated (sqrt scale)") +
    ylab("Responses received (sqrt scale)")

  return(list(g1, g2))
}
