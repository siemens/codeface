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

source("../../ml/init.response.r", chdir=TRUE)

createWidgetClass(
  class=c("widget.ir", "widget.rangeid"),
  name="Initiation-Response",
  description="Show connection between ML initiations and responses",
  topics=c("communication"),
  size.x=2,
  size.y=1,
  compareable = FALSE
)

initWidget.widget.ir <- function(w) {
  w <- NextMethod(w) # Call superclass

  w$dat.ir <- reactive({prepare.initiate.response(
                        query.initiate.response(conf$con,
                                  query.ml.id.simple.con(conf$con, w$pid()),
                                  w$view()))})
  return(w)
}

renderWidget.widget.ir <- function(w) {
  renderPlot({
      plot.init.response(w$dat.ir(),
                         title="Initate-response structure")[[2]]
  })
}
