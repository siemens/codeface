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

## Create overviews about the types of collaboration graphs appearing in
## projects.

s <- suppressPackageStartupMessages
s(library(shiny))
s(library(igraph))
s(library(logging))
s(library(corrgram))
rm(s)
source("../config.r", chdir=TRUE)
source("../utils.r", chdir=TRUE)
source("../query.r", chdir=TRUE)
source("../clusters.r", chdir=TRUE)
source("../vis.ports.r", chdir=TRUE)

## Global variables
conf <- config.from.args(require_project=FALSE)
projects.list <- query.projects(conf$con)

## Use the release ranges for the first project in the list
## as initial values
range.ids.list <- query.range.ids.con(conf$con, projects.list$id[[1]])
#####

gen.clusters.list <- function(l, con) {
  clusters.list <- lapply(1:length(l), function(i) {
    g <- construct.cluster(con, l[[i]])

    ## Self-loops in the proximity analysis can become very strong;
    ## the resulting edges then typically destroy the visualisation
    ## completely. Get rid of them, thus.
    ## NOTE: simplify must be called before the cluster is annotated
    ## because the function
    g <- simplify(g, remove.loops=TRUE)

    return(g)
  })

  ## Remove empty clusters
  clusters.list[sapply(clusters.list, is.null)] <- NULL

  clusters.list <- lapply(clusters.list, function(g) {
    return(annotate.cluster(g))
  })

  return(clusters.list)
}

do.plot <- function(g) {
  V(g)$name <- NA
  plot(g)
}


prepare.clusters <- function(con, pid, range.id) {
  l <- query.cluster.ids.con(con, pid, range.id, "Spin Glass Community")
  clusters.list <- gen.clusters.list(l, con)

  ## Sort the clusters by number of vertices
  sizes <- sapply(clusters.list, vcount)
  clusters.list <- clusters.list[sort(sizes, index.return=TRUE, decreasing=TRUE)$ix]

  max.length <- 8
  if (length(clusters.list) < max.length) {
    max.length <- length(clusters.list)
  }

  return(clusters.list[1:max.length])
}

do.cluster.plots <- function(clusters.list) {
  par(mfcol=c(2,4))
  for (i in 1:length(clusters.list)) {
    do.plot(clusters.list[[i]])
  }
}

gen.cluster.info <- function(g) {
  return(data.frame(Reciprocity=g$rec, Strength=g$strength,
                    Degree=g$deg.graph, Size=g$size,
                    Cent.degree=g$cent.deg, Cent.closeness=g$cent.clo,
                    Cent.betweenness=g$cent.bet, Cent.eigenvec=g$cent.evc))
}

gen.cluster.summary <- function(clusters.list) {
  res <- lapply(1:length(clusters.list), function(i) {
    df <- gen.cluster.info(clusters.list[[i]])
    cbind(ID=i, df)
  })

  return(do.call(rbind, res))
}

vis.clusters.server <- function(input, output, clientData, session) {
  ## TODO: Ensure that the selected project exists
  pid <- reactive({projects.list[projects.list$name==input$project,]$id})
  range.ids.list <- reactive({query.range.ids.con(conf$con, pid())})

  observe({
    updateSelectInput(session, "cycle", choices=range.ids.list())
  })

  range.id <- reactive({input$cycle})
  cluster.list <- reactive({prepare.clusters(conf$con, pid(), range.id())})

  output$clustersPlot <- renderPlot({
    do.cluster.plots(cluster.list())
  }, height=1024, width=2048)

  output$correlationPlot <- renderPlot({
    dat <- {gen.cluster.summary(cluster.list())}
    dat <- dat[,c("Reciprocity", "Strength", "Degree", "Size",
                  "Cent.degree", "Cent.closeness", "Cent.betweenness",
                  "Cent.eigenvec")]
    corrgram(dat, order=FALSE, lower.panel=panel.shade, upper.panel=panel.pie,
              text.panel=panel.txt, main="")
  })

  output$clustersSummary <- renderTable({gen.cluster.summary(cluster.list())})
}

vis.clusters.ui <- pageWithSidebar(
                         headerPanel("Collaboration clusters"),
                         sidebarPanel(
                           selectInput("project", "Project",
                                       choices = projects.list$name),
                           selectInput("cycle", "Release Cycle",
                                       choices = range.ids.list),

                           submitButton("Update View")
                           ),

                         mainPanel(
                           tabsetPanel(
                             tabPanel("Clusters", plotOutput("clustersPlot")),
                             tabPanel("Correlations", plotOutput("correlationPlot")),
                             tabPanel("Numeric", tableOutput("clustersSummary"))
                             )
                           )
                         )

## Dispatch the shiny server

runApp(list(ui=vis.clusters.ui, server=vis.clusters.server),
       port=PORT.VIS.CLUSTERS)
