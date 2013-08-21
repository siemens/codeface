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
source("../../widgets/commit.info.r", chdir=TRUE)

shinyServer(function(input, output, clientData, session) {
  pid = common.server.init(output, session, "commit.info")

  observe({
    range.ids.list <- query.range.ids.con(conf$con, pid())
    cycles <- reactive({get.cycles.con(conf$con, pid())})
    names(range.ids.list) <- cycles()$cycle
    updateSelectInput(session, "cycle", choices=range.ids.list)
  })

  range.id <- reactive({input$cycle})

  output$quantarchContent <- renderUI({
    pageWithSidebar(
      headerPanel("Commit Information"),
      sidebarPanel(
        selectInput("cycle", "Release Cycle", choices = list(1))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Scatter Plot", plotOutput("commitsSplom")),
          tabPanel("Correlations", plotOutput("commitsCorrgram"))
        )
      )
    )
  })
  output$commitsSplom <- commit.info.splom(pid, range.id)
  output$commitsCorrgram <- commit.info.corrgram(pid, range.id)
})
