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
source("../../widgets/timeseries.r", chdir=TRUE)

shinyServer(function(input, output, clientData, session) {
  pid = common.server.init(output, session, "timeseries")

  smooth <- reactive({input$smooth})
  transform <- reactive({input$transform})
  name <- reactive({
    paste(projects.list[[as.integer(pid())]], ".devel activity", sep='')
  })

  output$distancePlot <- timeseries.plot.messages.per.day(pid, name, smooth, transform)
  output$quantarchContent <- renderUI({
    pageWithSidebar(
      headerPanel("Mailing list activity"),
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
          )
      ),
      div(class = "span10", plotOutput("distancePlot"))
    )
  })
})
