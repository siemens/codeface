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

## Show descriptive statistics about projects

s <- suppressPackageStartupMessages
s(library(ggplot2))
s(library(shiny))
s(library(logging))
rm(s)
source("../config.r", chdir=TRUE)
source("../utils.r", chdir=TRUE)
source("../query.r", chdir=TRUE)
source("../vis.ports.r", chdir=TRUE)

## Global variables
conf <- config.from.args(require_project=FALSE)
projects.list <- query.projects(conf$con)

## Use the release ranges for the first project in the list
## as initial values
range.ids.list <- query.range.ids.con(conf$con, projects.list$id[[1]])
cycles <- get.cycles.con(conf$con, projects.list$id[[1]])
names(range.ids.list) <- cycles$cycle
#####

prepare.prank.table <- function(range.id, technique) {
  prank.id <- get.pagerank.id.con(conf$con, range.id, technique)
  dat <- query.pagerank(conf$con, prank.id)
  dat <- cbind(dat, prank.scaled=dat$prank/max(dat$prank))
  dat <- dat[c("name", "prank", "prank.scaled")]
  colnames(dat) <- c("Name", "Page rank", "Page rank (scaled)")

  return(dat)
}

prepare.commits.table <- function(range.id) {
  dat <- query.top.contributors.commits(conf$con, range.id)
  if (!is.null(dat)) {
    colnames(dat) <- c("Name", "Number of commits")
  }

  return(dat)
}

prepare.changes.table <- function(range.id) {
  dat <- query.top.contributors.changes(conf$con, range.id)
  if (!is.null(dat)) {
    colnames(dat) <- c("Name", "Added LOC", "Deleted LOC", "Total")
  }

  return(dat)
}

vis.descriptive.server <- function(input, output, clientData, session) {
  range.ids.list <- reactive({query.range.ids.con(conf$con, pid())})

  observe({
    rid.list <- range.ids.list()
    names(rid.list) <- cycles()$cycle
    updateSelectInput(session, "cycle",
                      choices=rid.list,
                      selected=range.ids.list()[[1]])
  })

  range.ids.list <- reactive({query.range.ids.con(conf$con, pid())})
  pid <- reactive({projects.list[projects.list$name==input$project,]$id})
  cycles <- reactive({get.cycles.con(conf$con, pid())})

  range.id <- reactive({input$cycle})

  output$prTable <- renderTable({prepare.prank.table(range.id(), 0)})
  output$prTrTable <- renderTable({prepare.prank.table(range.id(), 1)})
  output$commitsTable <- renderTable({prepare.commits.table(range.id())})
  output$changesTable <- renderTable({prepare.changes.table(range.id())})
}

vis.descriptive.ui <-
  pageWithSidebar(
    headerPanel("Descriptive statistics"),
    sidebarPanel(
      selectInput("project", "Project",
                  choices = projects.list$name),
      selectInput("cycle", "Release Cycle",
                  choices = range.ids.list),

      helpText(paste("Interpretational aid: Page rank focuses on giving tags, ",
                     "transposed page rank on being tagged.")),

      submitButton("Update View")
      ),

    mainPanel(
      tabsetPanel(
        tabPanel("Page Rank", tableOutput("prTable")),
        tabPanel("Page Rank (tr)", tableOutput("prTrTable")),
        tabPanel("Commits", tableOutput("commitsTable")),
        tabPanel("Code Changes", tableOutput("changesTable"))
        )
      )
    )

## Dispatch the shiny server

#runApp(list(ui=vis.descriptive.ui, server=vis.descriptive.server),
#       port=PORT.CONTRIBUTORS)
