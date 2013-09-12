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

createWidgetClass(
  c("widget.timeseries.messages.per.day", "widget.timeseries"),
  "Messages per Day",
  "Number of messages on the mailing list per day",
  size.x = 2,
  size.y = 1,
)

initWidget.widget.timeseries.messages.per.day <- function(w) {
  # Note that this class 'derives' from "widget.timeseries.plots", which
  # means we can reuse the listViews command and use the initialization
  w$plots <- reactive({
    dbGetQuery(conf$con, str_c("SELECT id, name FROM plots WHERE projectId=", w$pid(), " AND releaseRangeId IS NULL AND name LIKE '%activity'"))
  })
  w <- NextMethod(w)
  return(w)
}

renderWidget.widget.timeseries.messages.per.day <- function(w) {
  renderPlot({
    name <- w$plots()$name[[which(w$plots()$id==w$view())]]
    ts <- get.ts.data(conf$con, w$pid(), name)
    print(do.ts.plot(ts, w$boundaries(), name, "Messages per Day", w$smooth.or.def(), w$transform.or.def()))
  })
}

