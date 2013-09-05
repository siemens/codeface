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
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Get a commit.info.splom widget
createWidgetClass(
  class = c("widget.commit.info.splom", "widget.commit.info", "widget.rangeid"),
  name = "Commit Information - Scatterplot",
  description = "Shows the commit information in a scatterplot.",
  size.x = 2,
  size.y = 1,
  compareable = FALSE
)

## Get a commit.info.corrgram widget
createWidgetClass(
  class = c("widget.commit.info.corrgram", "widget.commit.info", "widget.rangeid"),
  name = "Commit Information - Correlations",
  description = "Shows correlations in the commits.",
  size.x = 1,
  size.y = 1,
  compareable = FALSE
)

## Common initialization of commit.info widgets
initWidget.widget.commit.info <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$data <- reactive({
    gen.commits.info(conf$con, w$pid(), w$view())
  })
  return(w)
}

# Render a Scatterplot
renderWidget.widget.commit.info.splom <- function(w) {
  renderPlot({
    gen.commits.splom(w$data()$cmt.info, w$data()$plot.types)
  })
}

# Render Correlations
renderWidget.widget.commit.info.corrgram <- function(w) {
  renderPlot({
    gen.commits.corrgram(w$data()$cmt.info, w$data()$plot.types)
  })
}

