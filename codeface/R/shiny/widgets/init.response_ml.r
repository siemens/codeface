#! /usr/bin/env Rscript

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
## Copyright 2015 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

## Careful: Name is not a list of widgets, but a _single_ string
## containing a list of widgets
detailpage <- list(name="widget.ir.receiving,widget.ir.writing",
                   title="Initiation-Response details")

source("../../ml/init.response.r", chdir=TRUE)

createWidgetClass(
  class=c("widget.ir"),
  name="Initiation-response",
  description="Show connection between ML initiations and responses",
  topics=c("communication"),
  size.x=2,
  size.y=1,
  compareable = FALSE,
  detailpage=detailpage
)

initWidget.widget.ir <- function(w) {
  w <- NextMethod(w) # Call superclass

  w$dat.ir.sum <- reactive({summarise.init.response(conf$con,
                              query.ml.id.simple.con(conf$con, w$pid()),
                              w$pid())})
  return(w)
}

renderWidget.widget.ir <- function(w) {
  renderPlot({
      plot.ir.summary(w$dat.ir.sum())
  })
}


## Detail widgets
createWidgetClass(
  class=c("widget.ir.receiving", "widget.rangeid"),
  name="Initiation-response details (receiving responses)",
  description=paste("Scatterplot that depicts how many threads a list",
      "contributor has opened, and how many responses the threads received"),
  topics=c("communication"),
  size.x=2,
  size.y=1,
  compareable = FALSE,
  detailpage=detailpage
)

initWidget.widget.ir.receiving <- function(w) {
  w <- NextMethod(w) # Call superclass

  w$dat.ir <- reactive({prepare.initiate.response(
                        query.initiate.response(conf$con,
                              query.ml.id.simple.con(conf$con, w$pid()),
                              w$view()))})
  return(w)
}

renderWidget.widget.ir.receiving <- function(w) {
  renderPlot({
      plot.init.response(w$dat.ir(),
                title="Initiate-response structure (receiving responses)")[[2]]
  })
}


createWidgetClass(
  class=c("widget.ir.writing", "widget.rangeid"),
  name="Initiation-response details (writing responses)",
  description=paste("Scatterplot that depicts how many threads a list",
      "contributor has opened, and how many responses he has written"),
  topics=c("communication"),
  size.x=2,
  size.y=1,
  compareable = FALSE,
  detailpage=detailpage
)

initWidget.widget.ir.writing <- function(w) {
  w <- NextMethod(w) # Call superclass

  w$dat.ir <- reactive({prepare.initiate.response(
                        query.initiate.response(conf$con,
                              query.ml.id.simple.con(conf$con, w$pid()),
                              w$view()))})
  return(w)
}

renderWidget.widget.ir.writing <- function(w) {
  renderPlot({
      plot.init.response(w$dat.ir(),
                 title="Initiate-response structure (writing responses)")[[1]]
  })
}
