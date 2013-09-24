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

createWidgetClass(
  c("widget.timeseries.plots", "widget.timeseries"),
  "Show Time Series",
  "Can show different time series calculated for this project",
  size.x = 2,
  size.y = 1,
  html = widget.plotOutput.html
)

initWidget.widget.timeseries <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$boundaries <- reactive({get.cycles.con(conf$con, w$pid())})
  if (!is.null(w$smooth)) {
    w$smooth.or.def <- reactive({if (is.null(w$smooth())) { 0 } else { w$smooth() } })
  } else {
    w$smooth.or.def <- reactive({0})
  }
  if (!is.null(w$transform)) {
    w$transform.or.def <- reactive({if (is.null(w$transform())) { 0 } else { w$transform() } })
  } else {
    w$transform.or.def <- reactive({0})
  }
  return(w)
}

initWidget.widget.timeseries.plots <- function(w) {
  # Note: The superclass may use listViews on plots
  # so we have to initialize w$plots before we pass w on
  w$plots <- reactive({dbGetQuery(conf$con, str_c("SELECT id, name FROM plots WHERE projectId=", w$pid(), " AND releaseRangeId IS NULL"))})
  # Call superclass
  w <- NextMethod(w)
  return(w)
}

renderWidget.widget.timeseries <- function(w) {
  renderPlot({
    name <- w$plots()$name[[which(w$plots()$id==w$view())]]
    ts <- get.ts.data(conf$con, w$pid(), name)
    print(do.ts.plot(ts, w$boundaries(), name, name, w$smooth.or.def(), w$transform.or.def()))
  })
}

listViews.widget.timeseries <- function(w) {
  reactive({
    if (is.null(w$plots)) {
      stop("listViews.widget.timeseries.plots called with uninitialized widget!")
    }
    l <- w$plots()$id
    names(l) <- w$plots()$name
    l
  })
}

