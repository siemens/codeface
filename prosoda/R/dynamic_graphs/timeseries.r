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

## Visualise time series including release boundaries
## TODO: Make it possible to save results to PDF

s <- suppressPackageStartupMessages
s(library(ggplot2))
s(library(scales))
s(library(shiny))
s(library(xts))
source("config.r")
source("query.r")
source("ts_utils.r")
source("vis.ports.r")

## Global variables
conf <- config.from.args(require_project=FALSE)
###########

get.ts.data <- function(con, pid) {
  ## TODO: This is currently statically set to openssl.devel.activity;
  ## we will need to provide a selection of a) projects and b) available
  ## mailing lists
  plot.id <- get.plot.id.con(con, pid, "openssl.devel activity")
  ts <- query.timeseries(con, plot.id)
  ts <- xts(x=ts$value, order.by=ts$time)

  return(ts)
}

do.ts.plot <- function(ts, boundaries, smooth, transform) {
  transforms <- c(function(x) { log(x+1) }, sqrt)
  smoothers <- c(apply.weekly, apply.monthly)

  ## TODO: Validate input parameters to make sure they are not out
  ## or range

  ## We need the maximal/minimal values of the time series for the
  ## vertical release boundary lines
  boundaries$ymin = min(ts)
  boundaries$ymax = max(ts)

  ## TODO: Don't know why selecting via smooth does not work.
  ## I have not yet found a reactive wrapping that would make
  ## do.
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
  ## TODO: Does a cumulative series make sense for the mailing list timeseries?
  g <- ggplot(ts, aes(x=time, y=value)) + geom_line() +
    geom_vline(aes(xintercept=as.numeric(date.end), colour="red"),
               data=boundaries) +
    scale_fill_manual(values = alpha(c("blue", "red"), .1)) +
    xlab("Time") + ylab(str_c("Messages per day")) +
    ggtitle("Mailing list activity")

  ## na.omit is required to remove all cycles that don't contain
  ## rc regions.
  ## TODO: The following would work once revisions_view is augmented
  ## with rc_start dates
#  if (dim(na.omit(boundaries))[1] > 0) {
#    ## Only plot release candidate regions if there are any, actually
#    g <- g + geom_rect(aes(NULL, NULL, xmin=date.rc_start,
#                           xmax=date.end, ymin=ymin, ymax=ymax, fill="blue"),
#                       data=na.omit(boundaries.plot))
#  }
  return(g)
}

ml.timeseries.server <- function(input, output, session) {
  args.list <- parseQueryString(isolate(session$clientData$url_search))
  pid <- args.list[["projectId"]]
  
  if (!is.vector(pid)) {
    stop("No projectId parameter in URL")
  } else if (is.na(as.numeric(pid))) {
      stop("projectId URL parameter is empty")
  }
  
  loginfo(paste("projectId =", as.character(as.numeric(pid)), sep = " "))
  
  ## Loading of data is performed only once for performance reasons
  ts <- get.ts.data(conf$con, pid)
  boundaries <- get.cycles.con(conf$con, pid)

  output$distancePlot <- renderPlot({
    print(do.ts.plot(ts, boundaries, input$smooth, input$transform))
  })
}

ml.timeseries.ui <- 
  pageWithSidebar(headerPanel("Mailing list activity"),
                  div(class = "span2", 
                      tags$form(class = "well",
                                radioButtons("smooth", "Smoothing window size",
                                             choices = c("None" = 0, 
                                                         "Weekly" = 1, 
                                                         "Monthly" = 2)),
                                br(),
                                radioButtons("transform", "Transformation",
                                             choices = c("Normal" = 0,
                                                         "Logarithmic" = 1,
                                                         "Square root" = 2))
                                )),
                  div(class = "span10", 
                      plotOutput("distancePlot")))

## Dispatch the shiny server
runApp(list(ui=ml.timeseries.ui, server=ml.timeseries.server),
       port=PORT.TIMESERIES.ML)
