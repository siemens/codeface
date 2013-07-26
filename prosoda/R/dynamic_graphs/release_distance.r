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
## Copyright 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Example server component to dynamically compare inter-release similarities
## for projects

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(shiny))
source("../config.r", chdir=TRUE)
source("../query.r", chdir=TRUE)
source("../ts_utils.r", chdir=TRUE)
source("../vis.ports.r", chdir=TRUE)

## Global variables
conf <- config.from.args(require_project=FALSE)
conf <- init.db.global(conf)
projects.list <- query.projects(conf$con)
###########

get.release.distance.data <- function(con, name.list) {
  pid.list <- lapply(name.list, function(name) {
    return(projects.list[projects.list$name==name,]$id)
  })

  plot.ids <- lapply(pid.list, function(pid) {
    return(get.plot.id.con(con, pid, "Release TS distance"))
  })

  ts.list <- lapply(plot.ids, function(plot.id) {
    return(query.timeseries(con, plot.id))
  })

  name.list <- lapply(pid.list, function(pid) {
    return(query.project.name(con, pid))
  })

  dat <- do.call(rbind, lapply(1:length(name.list), function(i) {
    data.frame(project=name.list[[i]], distance=ts.list[[i]]$value,
               date=ts.list[[i]]$time)
  }))

  return(dat)
}

do.release.distance.plot <- function(con, names.list) {
  dat <- get.release.distance.data(con, names.list)

  g <- ggplot(dat, aes(x=project, y=distance)) +
    geom_boxplot(outlier.colour="red") +
    geom_jitter(alpha=0.4, size=1.1) + xlab("Project") +
      ylab("Distribution of distance values")

  return(g)
}

release.distance.server <- function(input, output) {
  output$distancePlot <- renderPlot({
    print(do.release.distance.plot(conf$con,
                                   list(input$name1, input$name2, input$name3)))
  })
}

release.distance.ui <- pageWithSidebar(
                         headerPanel("Inter-Release Distance"),
                         sidebarPanel(
                           selectInput("name1", "Project 1",
                                       choices = projects.list$name),
                           selectInput("name2", "Project 2",
                                       choices = projects.list$name),
                           selectInput("name3", "Project 3",
                                       choices = projects.list$name),

                           helpText("Interpretational aid: Smaller is better for this plot."),
                           submitButton("Update View")
                           ),

                         mainPanel(
                           plotOutput("distancePlot")
                           )
                         )

## Dispatch the shiny server
runApp(list(ui=release.distance.ui, server=release.distance.server),
       port=PORT.RELEASE.DISTANCE)
