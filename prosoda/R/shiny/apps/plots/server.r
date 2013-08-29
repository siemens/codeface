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

source("../common.server.r", chdir=TRUE)
source("../../widgets.r", chdir=TRUE)

shinyServer(function(input, output, clientData, session) {
  pid = common.server.init(output, session, "plots")
	plot.id <- reactive({input$plotSelect})
  smooth <- reactive({input$smooth})
  transform <- reactive({input$transform})

  plot <- reactive({widget.timeseries.plots$new(pid(), smooth, transform)})
  observe({
    updateSelectInput(session, "plotSelect", choices=listViews(plot()))
  })

  observe({
    output$timeseriesPlot <- renderWidget(plot(), plot.id())
  })
  output$quantarchContent <- renderUI({
    tagList(
      sidebarPanel(
        selectInput("plotSelect", "Select Plot", choices = list(1)),
        radioButtons("smooth", "Smoothing window size",
                        choices = c("None" = 0,
                                    "Weekly" = 1,
                                    "Monthly" = 2)),
        radioButtons("transform", "Transformation",
                        choices = c("Normal" = 0,
                                    "Logarithmic" = 1,
                                    "Square root" = 2))
      ),
      mainPanel(
          plotOutput("timeseriesPlot")
      )
    )
  })
})
