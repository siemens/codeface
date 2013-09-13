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

s <- suppressPackageStartupMessages
s(library(shiny))
s(library(igraph))
s(library(logging))
s(library(corrgram))
s(library(ggplot2))
rm(s)
source("../../config.r", chdir=TRUE)
source("../../utils.r", chdir=TRUE)
source("../../query.r", chdir=TRUE)
source("../../commits.r", chdir=TRUE)
source("../../vis.ports.r", chdir=TRUE)

## Global variables
conf <- config.from.args(require_project=FALSE)
projects.list <- query.projects(conf$con)

## breadcrumb
source("../nav/breadcrumb.shiny.r", chdir = TRUE)
source("../widgets.r", chdir=TRUE)
source("../nav/qa_cookie.r", chdir = TRUE)

common.server.init <- function(output, session, app.name) {
  loginfo("Common server init...")
  paramstr <- urlparameter.checked(isolate(session$clientData$url_search))
  loginfo(paste("paramstr= ", paramstr))
  output$quantarchBreadcrumb <- renderUI({renderBreadcrumbPanel(app.name,paramstr)})
  args.list <- urlparameter.as.list(paramstr)
  ## Read out PID from the URL and check if it is valid
  pid <- reactive({args.list[["projectid"]]})
  
  ## returns the choices named vector
  choices <- projects.choices(projects.list)
  ## returns a reactive list containing selected projects
  selected <- reactive({ projects.selected( projects.list, input$qacompareids) })
  
  ## demoes how to use the choices and adding options for chosen.jquery.js
  output$selectpidsui <- renderCompareWithProjectsInput(
    "selectedpids","",choices, selected(), list(width="100%"))
  
  ## demoes how to update the cookies from the "selectedpids" ui input
  ## also available via choices (but beware of duplicate project names)
  observe({
    updateCookieInput(session, "qacompareids", input$selectedpids, pathLevel=0, expiresInDays=1 )
  })
  
  loginfo("Common server init done.")  
  return(list(pid=pid,selected=selected,args.list=args.list ))
}


detailPage <- function(app.name, widgets, additional.input=list()){
  function(input, output, clientData, session) {
    loginfo(paste("Creating detail page for", app.name))
    
    allpids = common.server.init(output, session, app.name)
    pid = allpids$pid
    selected = allpids$selected  # to be used for comparisons
    
    observe({
      if (!is.vector(pid())) {
        stop("No projectid parameter in URL")
      } else if (is.na(as.numeric(pid()))) {
        stop("projectid URL parameter is empty")
      }
      loginfo(paste("New Project ID: ", pid()))
    })
      
    range.id <- reactive({input$view})

    widget.classes <- widget.list[widgets]
    widget.instances <- lapply(widget.classes,
                               function(cls) {
                                 w <- newWidget(cls, pid, range.id)
                                 return(w)
                               })
    ## Note that since R always works by value, you CAN NOT change an
    ## object in e.g. a for loop; you always have to get a copy
    ## of the modified object out.
    loginfo("Adding additional inputs to widgets and initializing...")
    widget.instances <- lapply(widget.instances, function(w) {
        w <- Reduce(function(w, name) {
          input.value <- reactive({ input[[name]] })
          observe({ print(paste(name, "is now", input.value())) })
          #print("Isolated value:")
          #print(isolate(input.value))
          w[[name]] <- input.value
          return(w)
        },
        names(additional.input),
        w)
        w <- initWidget(w)
        if (is.null(w)) {
          logfatal("Error in Widget initialization!")
          stop("Error in Widget initialization!")
        }
        return(w)
      }
    )

    # Render widgets into tabs
    panel.tabs <- mapply(function(i, cls, w) {
        id <- paste("widget", i, sep="")
        output[[id]] <- renderWidget(w)
        html <- div(style="width: 100%; height: 500px", cls$html(id))
        return(reactive({
          tabPanel(widgetTitle(w)(), html)
        }))
      },
      1:length(widget.instances), widget.classes, widget.instances,
      SIMPLIFY=FALSE
    )

    # Only show tabs in the main panel if >1 widget
    if (length(widgets) == 1) {
      main.panel <- reactive({panel.tabs[[1]]()})
    } else {
      main.panel <- reactive({do.call(tabsetPanel, lapply(panel.tabs, function(x){x()}))})
    }

    # Create user interface, using a sidebar if necessary
    observe({
      str(widget.instances)
      choices <- listViews(widget.instances[[1]])()
      sidebar <- list()
      if (length(choices) > 1) {
        sidebar <- c(sidebar, list(selectInput("view", "View:", choices=choices)))
      }
      sidebar <- c(sidebar, additional.input)
      names(sidebar) <- NULL

      if (length(sidebar) > 0) {
        output$quantarchContent <- renderUI({
          tagList(
            sidebarPanel(sidebar),
            mainPanel(main.panel())
          )
        })
      } else {
        output$quantarchContent <- renderUI({main.panel()})
      }
    })
    loginfo(paste("Finished creating detail page for", app.name))
  }
}

