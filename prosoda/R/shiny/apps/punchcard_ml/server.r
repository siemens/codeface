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
  pid = common.server.init(output, session, "punchcard_ml")
  plot <- reactive({widget.punchcard.ml$new(pid())})
	plot.id <- reactive({input$plotSelect})
  observe({
    updateSelectInput(session, "mlSelect", choices=listViews(plot()))
  })
  observe({
    output$mlPunchcard <- renderWidget(plot(), plot.id())
  })
  output$quantarchContent <- renderUI({
    tagList(
      sidebarPanel(
        selectInput("mlSelect", "Select Mailing list", choices = list(1))
      ),
      mainPanel(
          plotOutput("mlPunchcard")
      )
    )
  })
})
