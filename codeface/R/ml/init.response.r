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
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Helper functions to process/visualise initiation-response structures stored
## in the database
suppressPackageStartupMessages(library(ggplot2))

## ir needs to be obtained by query.initiate.response
## Assign colours to persons depending on their relative
## importance in the network as measured by their degree
prepare.initiate.response <- function(ir, threshold1=0.25, threshold2=0.5) {
  ir$col <- "Low"
  ir$col[ir$deg > threshold1 & ir$deg < threshold2] <- "Medium"
  ir$col[ir$deg >= threshold2] <- "High"

  return(ir)
}

plot.init.response <- function(ir, title=NULL) {
  g1 <- ggplot(ir, aes(x=initiations, y=responses)) +
    geom_point(aes(size=deg, colour=col)) +
    scale_x_sqrt() + scale_y_sqrt() +
    facet_grid(source~.) +
    xlab("Messages initiated (sqrt scale)") +
    ylab("Responses given (sqrt scale)")
  if (!is.null(title)) {
    g1 <- g1 + ggtitle(title)
  }

  g2 <- ggplot(ir, aes(x=initiations, y=responses.received)) +
    geom_point(aes(size=deg, colour=col)) +
    scale_x_sqrt() + scale_y_sqrt() +
    facet_grid(source~.) +
    xlab("Messages initiated (sqrt scale)") +
    ylab("Responses received (sqrt scale)")
  if (!is.null(title)) {
    g2 <- g2 + ggtitle(title)
  }

  return(list(g1, g2))
}

summarise.init.response <- function(con, ml.id, pid) {
    range.ids.list <- query.range.ids.con(con, pid)
    cycles <- get.cycles.con(con, pid)

    ## Comute a very heuristically motivated summary of
    ## the mailing list structure for each release range.
    ## For each range, compute the fraction of
    ## responses received/messages initiated. A high ratio
    ## indicates that topics of interest have been raised.
    ## Compute the 0.75 quantile of the distribution to
    ## obtain the most influential contributors
    ## Then, compute the median degree of these contributors

    dat <- lapply(range.ids.list, function(range.id) {
         dat <- query.initiate.response(con, ml.id, range.id)

         dat$frac <- dat$responses.received/dat$initiations
         qnt <- quantile(dat$responses.received/dat$initiations,
                         na.rm=TRUE, probs=0.75)
         median(dat$deg[dat$frac > qnt], na.rm=T)

         return(data.frame(cycle=cycles$cycle[cycles$range.id==range.id],
                           med.deg=median(dat$deg[dat$frac > qnt], na.rm=T),
                           num.msg=nrow(dat)))

     })

    return (do.call(rbind, dat))
}

## Visualise a given initiate-response data structure
plot.ir.summary <- function(dat.ir.summary) {
    g <- ggplot(dat.ir.summary, aes(x=cycle, y=med.deg)) +
           geom_point(aes(size=num.msg)) +
           xlab("Release cycle") + ylab("Med. important contributor degree") +
           scale_size_continuous("Number of\nmessages")
    return(g)
}
