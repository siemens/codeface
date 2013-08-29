#! /usr/bin/env Rscript

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
## Copyright 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

suppressPackageStartupMessages(library(scales))
source("../../ts_utils.r", chdir=TRUE)

## Visualise time series including release boundaries
get.ts.data <- function(con, pid, name) {
  plot.id <- get.plot.id.con(con, pid, name)
  ts <- query.timeseries(con, plot.id)
  ts <- xts(x=ts$value, order.by=ts$time)
  return(ts)
}

do.ts.plot <- function(ts, boundaries, title, y.label, smooth, transform) {
  transforms <- c(function(x) { log(x+1) }, sqrt)
  smoothers <- c(apply.weekly, apply.monthly)
  ## We need the maximal/minimal values of the time series for the
  ## vertical release boundary lines
  boundaries$ymin = min(ts)
  boundaries$ymax = max(ts)

 if (smooth == 1) {
    ts <- smoothers[[1]](ts, median)
  } else if (smooth == 2) {
    ts <- smoothers[[2]](ts, median)
  }

  if (transform == 1) {
    coredata(ts) <- transforms[[1]](coredata(ts))
  } else if (transform == 2) {
    coredata(ts) <- transforms[[2]](coredata(ts))
  }

  ## ggplot needs a data.frame, so convert the time series into one
  ts <- data.frame(time=index(ts), value=coredata(ts))

  ## Visualisation
  g <- ggplot(ts, aes(x=time, y=value)) + geom_line() +
    geom_vline(aes(xintercept=as.numeric(date.end), colour="red"),
               data=boundaries) +
    scale_fill_manual(values = alpha(c("blue", "red"), .1)) +
    xlab("Time") + ylab(str_c(y.label)) +
    ggtitle(title)
  return(g)
}

widget.timeseries.plots <- list(
  title = "Plot Time Series",
  size.x = 2,
  size.y = 1,
  new = function(pid, smooth, transform) {
    w <- make.widget(pid)
    class(w) <- c("widget.timeseries.plots", w$class)
    w$plots <- dbGetQuery(conf$con, str_c("SELECT id, name FROM plots WHERE projectId=", pid, " AND releaseRangeId IS NULL"))
    w$boundaries <- get.cycles.con(conf$con, pid)
    w$smooth = smooth
    w$transform = transform
    return (w)
  },
  html = function(id) { plotOutput(id, width="100%", height="100%") }
)

widget.list$widget.timeseries.plots <- widget.timeseries.plots

renderWidget.widget.timeseries.plots <- function(w, view=NULL) {
  if (is.null(view)) {
    view <- w$plots$id[[1]]
  }
  name <- w$plots$name[[which(w$plots$id==view)]]
  ts <- get.ts.data(conf$con, w$pid, name)
  renderPlot({
    print(do.ts.plot(ts, w$boundaries, name, name, w$smooth(), w$transform()))
  })
}

listViews.widget.timeseries.plots <- function(w) {
  l <- w$plots$id
  names(l) <- w$plots$name
  l
}

