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
  pid = common.server.init(output, session, "release_distance")
  range.id <- reactive({input$cycle})
  name2 <- reactive({input$name2})
  name3 <- reactive({input$name3})

  w <- reactive({make.widget.release.distance(pid(), name2, name3)})
  observe({
    output$distancePlot <- renderWidget(w())
  })
  output$quantarchContent <- renderUI({
    pageWithSidebar(
      headerPanel("Inter-Release Distance"),
      sidebarPanel(
        selectInput("name2", "Also show project:",
                    choices = projects.list$name),
        selectInput("name3", "Also show project:",
                    choices = projects.list$name),
        helpText("Interpretational aid: Smaller is better for this plot.")
      ),
      mainPanel(
        plotOutput("distancePlot")
      )
    )
  })
})
