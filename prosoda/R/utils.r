# Some utility functions that can be loaded with source("utility.r")

# This file is part of prosoda.  prosoda is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2010, 2011 by Wolfgang Mauerer <wm@linux-kernel.net>
# Copyright 2012, 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.

library(logging)

tstamp_to_date <- function(z) as.POSIXct(as.integer(z), origin="1970-01-01")

shannon.entropy <- function(p)
{
	if (min(p) < 0 || sum(p) <= 0)
		return(NA)
	p.norm <- p[p>0]/sum(p)
	-sum(log2(p.norm)*p.norm)
}

to.regts <- function(rawts, smooth)
{
	ts <- as.xts(rollmean(rawts, smooth))
	ts_reduced <- as.xts(to.period(ts, "hours")[,1])
	# Average difference in seconds between two data points
	tstart <- unclass(index(ts_reduced[1]))
	tend <- unclass(index(ts_reduced[length(ts_reduced)]))
	tdiff <- floor((tend-tstart)/length(ts_reduced))
	ts(data=coredata(ts_reduced), start=tstart, deltat=tdiff)
}

HOURLY <- 60*60
HALF.DAILY <- HOURLY*12
DAILY <- HOURLY*24

align_ts <- function(x,N) { tstamp_to_date(floor(as.integer(x)/N)*N)}

generate_regular_ts <- function(raw, N, delta.t=NA) {
  daily <- aggregate(raw, function(x) { align_ts(x, N) }, sum)
  if (is.na(delta.t)) {
    tseries <- as.ts(daily);
  } else {
    tseries <- ts(data=coredata(daily), deltat=delta.t);
  }
  # TODO: Strangely enough, the time propery of the index is lost,
  # and we retain only the numeric values. tstamp_to_date(index(series))
  # is, however, still correct
  tseries[is.na(tseries)] <- 0

  return(tseries)
}

## Convert a time series into a data frame
gen.df.from.ts <- function(ts, type) {
  df <- data.frame(time=index(ts), value=coredata(ts),
                   value.scaled=scale.data(coredata(ts)), type=type)

  return(df)
}

can.read.file <- function(file) {
  return (file.access(file, mode=2) != -1)
}

status <- function(str) {
	cat(paste("\r", paste(rep(" ", 75), collapse=''), sep=""))
	cat(paste("\r", str, sep=""))
}

## Scale a given data set to the range [min,max]
scale.data <- function(dat, .min=0, .max=1) {
  datMin <- min(dat)
  datMax <- max(dat)

  if (datMin == datMax) {
    dat <- .max
  }
  else {
    dat <- dat - min(dat)
    dat <- dat/max(dat)*(.max-.min)
    dat <- dat + .min
  }
  return(dat)
}

## Given an igraph edge list (data frame with columns to and from), create
## a weighted edge list
gen.weighted.edgelist <- function(edges) {
  edges.weighted <- lapply(unique(edges$fromId), function(from.vertex) {
    to.vertices <- edges[edges$fromId==from.vertex,]$toId
    res <- do.call(rbind, lapply(unique(to.vertices), function(to.vertex) {
      return(data.frame(fromId=from.vertex, toId=to.vertex,
                        weight=sum(to.vertices==to.vertex)))
    }))

    return(res)
  })

  return(do.call(rbind, edges.weighted))
}

## Give a cluster identifier and (optionally) the page rank technique,
## (re)construct an igraph object from the DB
construct.cluster <- function(con, cluster.id, technique=0) {
  edges <- query.cluster.edges(con, cluster.id)
  members <- query.cluster.members(con, cluster.id, prank=T, technique=technique)

  if (!all(unique(c(edges$toId, edges$fromId)) %in% members$person)) {
    stop("Internal error: edges for non-existent persons in cluster ", cluster.id)
  }

  if (is.null(edges)) {
    logwarn(paste("Duh: cluster without edges for id ", cluster.id, "?!\n"))
    return(NULL)
  }

  g <- graph.data.frame(edges, vertices=members)
  return(g)
}

# The following method to produce a coloured recurrence plot is taken from
# http://zoonek2.free.fr/UNIX/48_R/15.html#11
# The other functions are also from there
recurrence_plot <- function (
  x,
  m=1,           # Dimension of the embedding
  t=1,           # Lag used to define this embedding
  epsilon=NULL,  # If non-NULL, threshold
  box=TRUE, ...
) {
  stopifnot( length(m) == 1, m >= 1, m == floor(m),
             length(t) == 1, t >= 1, t == floor(t),
             is.null(epsilon) || (
               length(epsilon) == 1 && epsilon > 0 ) )
  stopifnot( length(x) > m * t )
  res <- outer(x, x, function (a,b) (a-b)^2)
  i <- 2
  LAG <- function (x, lag) {
    stopifnot(lag > 0)
    if (lag >= length(x)) {
      rep(NA, length(x))
    } else {
      c(rep(NA, lag), x[1:(length(x)-lag)])
    }
  }
  while (i <= m) {
    y <- LAG(x,t*(i-1))
    res <- res + outer(y, y, function (a,b) (a-b)^2)
    i <- i + 1
  }
  res <- sqrt(res)
  if (!is.null(epsilon)) {
    res <- res > epsilon
  }
  if (m>1) {
    # TODO: Check this...
    res <- res[ - (1:(t*(m-1))), ] [ , - (1:(t*(m-1))) ]
  }
  image(res, ...)
  if (box) {
    box()
  }
}


phase_plane_plot <- function (
  x,
  col=rainbow(length(x)-1),
  xlab = "x", ylab = "dx/dt",
  ...) {
  plot( x[-1], diff(x), col = col,
        xlab = xlab, ylab = ylab, ... )
}

# TODO: We could make the span for spectrum parametrisable (spans=N)
signal.and.spectrum <- function (x, main="") {
  op <- par(mfrow=c(2,1),
            mar=c(2,4,2,2)+.1,
            oma=c(0,0,2,0))
  plot(x, type="l", main="", ylab="Signal")
  spectrum(x, main="", xlab="")
  abline(v=.1*1:10, lty=3)
  par(op)
 mtext(main, line=1.5, font=2, cex=1.2)
}

###############################################################
