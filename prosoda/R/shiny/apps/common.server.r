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
source("../../page.rank.r", chdir=TRUE)

## Global variables
conf <- config.from.args(require_project=FALSE)
projects.list <- query.projects(conf$con)

## breadcrumb
source("../nav/breadcrumb.shiny.r", chdir = TRUE)
source("../widgets.r", chdir=TRUE)
source("../nav/qa_cookie.r", chdir = TRUE)

common.server.init <- function(input, output, session, app.name) {
  loginfo("Common server init...")

  ## Get the parameters from the query string
  paramstr <- reactive({ urlparameter.checked(session$clientData$url_search) })
  args.list <- reactive({ urlparameter.as.list(paramstr()) })

  ## Render the breadcrumb panel; the app.name determines
  ## the position in the hierarchy
  output$quantarchBreadcrumb <- renderUI({renderBreadcrumbPanel(app.name, paramstr())})

  ## Read out PID from the URL and check if it is valid
  pid <- reactive({ args.list()$projectid })

  ## returns the choices named vector
  choices <- projects.choices(projects.list)

  ## returns a reactive list containing selected projects
  selected <- reactive({ projects.selected( projects.list, input$qacompareids) })
  selected.pids <- reactive({
    pids <- unlist(strsplit(input$qacompareids,",")) 
    if (is.null(pids)) { list() } else { pids }
  })

  ## Create project comparison user interface
  output$selectpidsui <- renderCompareWithProjectsInput(
    "selectedpids","",choices, selected(), list(width="100%"))

  ## Update the cookies for the project comparsion
  ## also available via choices (but beware of duplicate project names)
  observe({
    dat <- input$selectedpids
    dat <- if(is.null(dat)) { list() } else { dat }
    ## TODO: pathLevel=1 does not seem to work
    updateCookieInput(session, "qacompareids", dat, pathLevel=0, expiresInDays=1)
  })

  loginfo("Common server init done.")
  return(list(pid=pid,
              selected=selected.pids,
              args.list=args.list ))
}


detailPage <- function(app.name, widgets=NULL, additional.input=list()){
  function(input, output, clientData, session) {
    loginfo(paste("Creating detail page for", app.name))

    allpids = common.server.init(input, output, session, app.name)
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

    if (is.null(widgets)) {
      widgets <- isolate({unlist(strsplit(allpids$args.list()$widget,","))})
    }


    ## Note that since R always works by value, you CAN NOT change an
    ## object in e.g. a for loop; you always have to get a copy
    ## of the modified object out.
    initialize.widget <- function(w) {
      w <- Reduce(function(w, name) {
        input.value <- reactive({ input[[name]] })
        #force(isolate({input.value()}))
        #observe({ print(paste(name, "is now", input.value())) })
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

    widgets.by.id <<- list()

    ## Create or reuse a widget with a unique index i and of the class cls
    ## and make sure that all outputs are set. Returs a list of output ids
    ## which have been set.
    make.or.reuse.widget <- function(i, cls, project) {
      id.widget <- paste("widget", i, project, sep="_")
      id.panel <- paste("widget", i, project, "panel", sep="_")
      id.ui <- paste("widget", i, project, "ui", sep="_")
      id.help <- paste("widget", i, project, "help", sep="_")
      project.title <- projects.list$name[[which(projects.list$id == project)]]
      if (is.null(widgets.by.id[[id.widget]])) {
        w <- initialize.widget(newWidget(cls, reactive({project}), reactive({input[[paste("view", project, sep="")]]}), selected))
        widgets.by.id[[id.widget]] <<- w
        output[[id.widget]] <- renderWidget(w)
        output[[id.panel]] <- renderUI({div(style="width: 100%; height: 500px", cls$html(id.widget))})
        output[[id.help]] <- renderUI({tagList(h4(widgetTitle(w)()), p(widgetExplanation(w)()))})
        output[[id.ui]] <- renderUI({
          choices <- listViews(w)()
          if (length(choices) > 1) {
            title <- if(project == pid()) { "View:" } else { paste("View (", project.title, "):", sep="") }
            list(selectInput(paste("view", project, sep=""), h3(title), choices=choices))
          } else {
            list()
          }
        })
      }
      tab.title <- widgetTitle(widgets.by.id[[id.widget]])()
      return(list(id=id.panel, project.title=project.title,
                  tab.title=tab.title, id.ui=id.ui, id.help=id.help))
    }

    ## Create and set up the widgets for this project (given by pid())
    widget.ids.base <- reactive({lapply(1:length(widgets), function(i) {
      make.or.reuse.widget(i, widget.list[[widgets[[i]]]], pid())
    })})

    ## If the widget is not compareable, we need additional tabs
    all.instance.ids <- lapply(1:length(widgets), function(i) {
      widget <- widgets[[i]]
      cls <- widget.list[[widget]]
      id <- paste("widget", i, sep="")
      if (!cls$compareable) {
        ## If the list of compared widgets changes, first make sure all widgets are there
        ## If we have projects to compare...
        instance.ids <- reactive({lapply(selected(), function(project) {
          make.or.reuse.widget(i, cls, project)
        })})

        output[[id]] <- renderUI({
          if (length(selected()) > 0) {
            do.call(tabsetPanel, lapply(c(list(widget.ids.base()[[i]]), instance.ids()), function(ids) {
                                        tabPanel(ids$project.title, uiOutput(ids$id))}))
          } else {
            uiOutput(widget.ids.base()[[i]]$id)
          }
        })
      } else {
        instance.ids <- reactive({list()})
        output$additional.views <- renderUI({list()})
        output[[id]] <- renderUI({ uiOutput(widget.ids.base()[[i]]$id) })
      }
      instance.ids
    })

    output$additional.views <- renderUI({
      if (length(selected()) > 0) {
        return(tagList(lapply(all.instance.ids[[1]](), function(id) { uiOutput(id$id.ui) })))
      } else {
        list()
      }
    })


    # Only show tabs in the main panel if >1 widget
    output$main.panel <- renderUI({
      if (length(widgets) == 1) {
        return(uiOutput("widget1"))
      } else {
        widget.tabs <- lapply(1:length(widgets), function(i) {
          id <- paste("widget", i, sep="")
          tabPanel(widget.ids.base()[[i]]$tab.title, uiOutput(id))
        })
        return(do.call(tabsetPanel, widget.tabs))
      }
    })

    # Create user interface, using a sidebar if necessary
    observe({
      sidebar <- list(uiOutput(widget.ids.base()[[1]]$id.ui))
      sidebar <- c(sidebar, list(uiOutput("additional.views")))
      sidebar <- c(sidebar, additional.input)
      sidebar <- c(sidebar, tagList(h3("Detail Information:")))
      sidebar <- c(sidebar, lapply(widget.ids.base(), function(w) {uiOutput(w$id.help)}))
      names(sidebar) <- NULL
      output$quantarchContent <- renderUI({
        tagList(
          sidebarPanel(sidebar),
          mainPanel(uiOutput("main.panel"))
        )
      })
    })
    loginfo(paste("Finished creating detail page for", app.name))
  }
}

