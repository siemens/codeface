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

## Shiny dispatcher for commit statistics
s <- suppressPackageStartupMessages
s(library(shiny))
s(library(igraph))
s(library(logging))
s(library(corrgram))
s(library(ggplot2))
rm(s)
source("config.r")
source("db.r")
source("utils.r")
source("query.r")
source("commits.r")
source("vis.ports.r")

## Global variables
conf <- load.global.config("prosoda.conf")
conf <- init.db.global(conf)
projects.list <- query.projects(conf$con)

## Use the release ranges for the first project in the list
## as initial values
range.ids.list <- query.range.ids.con(conf$con, projects.list$id[[1]])

vis.clusters.server <- function(input, output, clientData, session) {
  ## TODO: Ensure that the selected project exists
  pid <- reactive({projects.list[projects.list$name==input$project,]$id})
  range.ids.list <- reactive({query.range.ids.con(conf$con, pid())})

  observe({
    updateSelectInput(session, "cycle", choices=range.ids.list())
  })

  range.id <- reactive({input$cycle})

  dat <- reactive({gen.commits.info(conf$con, pid(), range.id())})

  output$commitsSplom <- renderPlot({
    gen.commits.splom(dat()$cmt.info, dat()$plot.types)
  })

  output$commitsCorrgram <- renderPlot({
    gen.commits.corrgram(dat()$cmt.info, dat()$plot.types)
  })
}

vis.clusters.ui <- pageWithSidebar(
                         headerPanel("Commit structure"),
                         sidebarPanel(
                           selectInput("project", "Project",
                                       choices = projects.list$name),
                           selectInput("cycle", "Release Cycle",
                                       choices = range.ids.list),

                           submitButton("Update View")
                           ),

                         mainPanel(
                           tabsetPanel(
                             tabPanel("Scatter Plot", plotOutput("commitsSplom")),
                             tabPanel("Correlations", plotOutput("commitsCorrgram"))
                             )
                           )
                         )

## Dispatch the shiny server
basicConfig()
runApp(list(ui=vis.clusters.ui, server=vis.clusters.server),
       port=PORT.COMMIT.INFO)
