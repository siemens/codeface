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
s(library(ggplot2))
s(library(shiny))
s(library(logging))
s(library(stats))
rm(s)
source("config.r")
source("utils.r")
source("query.r")
source("commits.r")
source("vis.ports.r")

## Global variables
conf <- config.from.args()
projects.list <- query.projects(conf$con)
#####

## Perform dimensionality reduction on the per-cycle commits using
## multi-dimensional scaling
do.mds <- function(cmt.info.list, k=2, method="euclidean") {
  res <- lapply(cmt.info.list, function(cmt.info) {
    d <- dist(cmt.info, method=method)
    fit <- cmdscale(d, k=k)
    ## Alternative: fit <- isoMDS(d, k=2)

    res <- data.frame(x=fit[,1], y=fit[,2], inRC=cmt.info$inRC,
                      cycle=cmt.info$cycle)
    return(res)
  })

  res <- do.call(rbind, res)
  return(res)
}

## Perform dimensionality reduction on the per-cycle commits using
## principal component analysis
do.prcomp <- function(cmt.info.list, subset, method="euclidean") {
  res <- lapply(cmt.info.list, function(cmt.info) {
    if (all(is.na(cmt.info$NumTags))) {
      cmt.info <- cmt.info[!(colnames(cmt.info) %in% "NumTags")]
      cmt.info <- cmt.info[!(colnames(cmt.info) %in% "NumSignedOffs")]
      subset <- subset[!(subset %in% c("NumTags", "NumSignedOffs"))]
    }
    cmt.info <- na.omit(cmt.info)
    d <- dist(cmt.info, method=method)
    pr <- prcomp(cmt.info[subset], cor=TRUE, scale=TRUE)

    ## Sum up the proportion of variance for the first two
    ## principal components as an indicator how well the dimensionality
    ## reduced data reflects reality
    prop <- sum(summary(pr)$importance["Proportion of Variance", c("PC1", "PC2")])
    res <- data.frame(x=pr$x[,1], y=pr$x[,2], inRC=cmt.info$inRC,
                      cycle=cmt.info$cycle, prop=prop)
    return(res)
  })

  res <- do.call(rbind, res)
  return(res)
}


vis.commit.structure.server <- function(input, output, clientData, session) {
  pid <- reactive({projects.list[projects.list$name==input$project,]$id})

  subset <- c("CmtMsgBytes", "ChangedFiles", "DiffSize", "NumTags", "NumSignedOffs")
  cmt.info.list <- reactive({get.cmt.info.list(conf$con, pid(), subset)})

  output$mdsPlot <- renderPlot({
    dat <- reactive({do.mds(cmt.info.list(), method="euclidean")})

    g <- ggplot(dat(), aes(x=x, y=y, colour=inRC)) + geom_point() +
      facet_wrap(~cycle)

    print(g)
  })

  output$princompPlot <- renderPlot({
    dat <- reactive({do.prcomp(cmt.info.list(), subset)})

    g <- ggplot(dat(), aes(x=x, y=y, colour=prop, shape=inRC)) + geom_point() +
      facet_wrap(~cycle)

    print(g)
  })
}

vis.commit.structure.ui <- pageWithSidebar(
                         headerPanel("Commit Structure (dimensionality reduction)"),
                         sidebarPanel(
                           selectInput("project", "Project",
                                       choices = projects.list$name),

                           submitButton("Update View")
                           ),

                         mainPanel(
                           tabsetPanel(
                             tabPanel("Principal components",
                                      plotOutput("princompPlot")),
                             tabPanel("Multi-Dimensional Scaling",
                                      plotOutput("mdsPlot"))
                             )
                           )
                         )

## Dispatch the shiny server

runApp(list(ui=vis.commit.structure.ui, server=vis.commit.structure.server),
       port=PORT.COMMIT.STRUCTURE)
