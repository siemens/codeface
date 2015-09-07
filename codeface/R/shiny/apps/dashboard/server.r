## This file is part of Codeface. Codeface is free software: you can
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

##
## Software Project dashboard (server.r)
##

suppressPackageStartupMessages(library(RJSONIO))
suppressPackageStartupMessages(library(shinyGridster))

source('gridsterWidgetsExt.r', chdir=TRUE)
source("../common.server.r", chdir=TRUE)   # REMARK: only source and library statements used here !!!

## generate a unique name to be added to list
## template used is: "prefix<integer>"
internal.getuniqueid.last <<- 1
internal.getuniqueid.existing <<- character(0)
getuniqueid <- function(prefix = "") {
  internal.getuniqueid.last <<- internal.getuniqueid.last + 1
  id <- paste(prefix, internal.getuniqueid.last, sep="")
  if (id %in% internal.getuniqueid.existing) {
    getuniqueid(prefix)
  } else {
    id
  }
}

## Generic widgetUI based on widget instance
widgetUI.header <- function(x, ...) UseMethod("widgetUI.header")

widgetUI.header.widget <- function(w, id) {
  ## define basic widget ui for widget instances
  w$titleid <- paste(id,"_title",sep="")
  title <- div(id = w$titleid, class = "shiny-text-output widget_title")
  w$ui <- tags$div( class="title_bar", 
                    tags$table(width="100%", tags$tr(tags$td(title))))
  if (length((isolate({listViews(w)()}))) > 1) {
    w <- widgetbase.output.selectview(w, id)
  }
  w
}

widgetUI.header.widget.rangeid <- function(w, id) {
  ## define basic widget ui for a widget instances with rangeids
  w <- NextMethod()
  #w$viewid <- paste(id,"_views",sep="")
  w <- widgetbase.output.selectview(w, id)
  w
}

widgetbase.output.selectview <- function(w, id) {
  ## render a selectInput
  mychoices <- as.list(isolate({listViews(w)()}))
  myselected <- isolate({w$view()})
  myselected <- names(mychoices[mychoices %in% myselected])
  
  inputView.id.local <- paste(id, "_selectedview",sep="")
  title <- div(id = w$titleid, class = "shiny-text-output widget_title")
  selector <- selectInput(inputView.id.local, "",
                                choices = mychoices,
                                selected = myselected)
  selector[[2]]$attribs$class <- "widget_view_select"
  w$ui <- div(class="title_bar", 
              tags$table(width="100%", tags$tr(tags$td(title), tags$td(selector))))
  wselectedviewid <- inputView.id.local
  w
}

##
## Widget Builder for new widgets
##
widgetbase.output.new <- function(input, output, id, widget.class, topic, pid, selected.pids) {
  widgetbase.output(input, output, id, widget.class, topic, pid, widget.class$size.x, widget.class$size.y, 1, 1, selected.pids)
}

##
## Widget builder for fully configured widgets (pid and selected.pids must be reactive)
##
widgetbase.output <- function(input, output, id, widget.class, topic, pid, size_x, size_y, col, row, selected.pids) {
  wb <- list()
  tryCatch({
    ## Widget creation and initialization (see: widget.r)
    titleOutput.id <- paste(id, "_title",sep="")
    inputView.id <- paste(id, "_selectedview",sep="")

    loginfo(paste("Start initialisation of new widget:", widget.class))
    inst <- initWidget(newWidget(widget.class, pid, reactive({input[[inputView.id]]}), selected.pids))
    
    ## reactive assignments (can be done in advance)
    output[[id]] <- renderWidget(inst)
    output[[titleOutput.id]] <- renderText(paste(projects.list$name[[which(projects.list$id == pid())]], widgetTitle(inst)(), sep=" / "))
    loginfo(paste("Finished initialising new widget:", inst$name))

    ## build ui header
    inst.ui <- widgetUI.header(inst, id)
    
    ##
    ## render a link to detail pages specific for this widget (breadcrumb will display all links)
    ##
    #str(widget.class$detailpage)
    
    ## (1) handled by details app, needs a project id (or NULL), a widget class name and a configured topic (or NULL)
    if (!is.null(widget.class$detailpage$name)) {
      name <- widget.class$detailpage$name
      link <- paste("../details/?projectid=", isolate(pid()), "&widget=", name, "&topic=", isolate({topic()}), sep="")
      
      ## TODO use a details icon instead
      detail.link <- a(class="link_details", href=link, "")
    
      ## (2) handled by an app, needs an app, project id and a configured topic  
    } else if (!is.null(widget.class$detailpage$app)) {
      app <- widget.class$detailpage$app
      link <- paste("../", app, "/?projectid=", isolate(pid()), "&topic=", isolate({topic()}), sep="")
      
      detail.link <- a(class="link_details", href=link, "")
    } else {
      
      detail.link <- list()
    }
    
    all.link <- div(style="float:right;", 
                    a(class="link_projects", href=paste("?widget=", widget.class[[1]], sep=""), ""))
    
    ## append footer
    footer <- tags$div(class="link_bar", detail.link, all.link)

    wb$html <- tagList(inst.ui$ui, div(class="content_box", widget.class$html(id)), footer)

    #cat("==========selectview=========\n")
    #print(widgetbase.output.selectview(inst.ui, id))
    ## build the widget's property list
    wb$id <- id
    ##wb$titleid <- inst$titleid
    ##wb$viewid <- inst$viewid
    wb$widget <- inst.ui
    wb$widget.class <- widget.class
    wb$size_x <- size_x
    wb$size_y <- size_y
    wb$col <- col
    wb$row <- row
    ## title and text for help popover
    wb$help <- list(title=isolate(widgetTitle(inst)()), content=isolate(widgetExplanation(inst)()), html=TRUE, trigger="click")
    ## remark adding "in" as the first element of placement will put the html code representing the popover inside the element
    ## however this produces problems with stacking, because other widgets will form stacking contexts and you cannot place
    ## the popover in front of them easily, see: http://philipwalton.com/articles/what-no-one-told-you-about-z-index/
    ## So it is better to place the popover's html inside the body element, so it will be displayed pretty above the others
    ## (z-index:1010)
    #wb$help <- list(title=isolate(widgetTitle(inst)()), content=isolate(widgetExplanation(inst)()), html=TRUE, trigger="click", placement="in bottom" )
  }, warning = function(warn) {
    logwarn(paste("widgetbase.output.new(id=", id, " w=<", widget.class$name,">, pid=",isolate(pid()),":", toString(warn)))
    print(traceback(warn))
  }, error = function(err) {
    logerror(paste("widgetbase.output.new(id=", id, " w=<", widget.class$name,">, pid=",isolate(pid()),":", toString(err)))
    print(traceback(err))
  }, {})
  wb
}

##
## Filter Widget List (remove widgets that take too long to load)
##
widget.list.filtered <- widget.list[
  names(widget.list) != "widget.commit.structure.mds" &
    names(widget.list) != "widget.punchcard.ml"
  ]

##
## Function to send a widget (needs session environment)
##    (parameter w: as generated by widgetbase.output)
##
sendWidgetContent <- function(session, w) {
  basehtml <- function(x) {
    tags$li(class="qawidget",
      style=paste("background-color:",isolate(widgetColor(w$widget)()),";box-shadow: 10px 10px 5px #CCC;", sep=""),
      tags$i( class="icon-remove-sign hidden", style="position:absolute;z-index:99;"),
      tags$div("data-qaclass"=class(w$widget)[1], "data-qaid"=w$id ),
      x) }
  #print(as.character(basehtml(w$html)))
  session$sendCustomMessage(
    type = "GridsterMessage",
    message = list(
      msgname = "addWidget",   			# Name of message to send
      html = as.character(basehtml(w$html)),		# this is the html for the widget
      size_x = as.character(w$size_x),	# in units of grid width
      size_y = as.character(w$size_y),	# dto
      col = as.character(w$col),			# column in grid
      row = as.character(w$row),			# row in grid
      qaid = w$id,
      help = w$help
  )
)}

##
## Function to configure the button menue (needs session environment)
##
sendGridsterButtonOptions <- function(session, options=list()) {
  session$sendCustomMessage(
    type = "GridsterMessage",
    message = list(
      msgname = "options",     		# Name of message to send
      options = options
    )
)}

##
## Function to update widget properties (needs session environment)
##
sendWidgetViewUpdates <- function(session, w) {
  session$sendCustomMessage(
    type = "GridsterMessage",
    message = list(
      msgname = "updatewidget",     	# Name of message to send
      qaid = w$id,
      help = w$help
  )
)}

##
## The Server function
##
shinyServer(function(input, output, session) {

  ## log the Url parameters
  loginfo(isolate(names(session$clientData$url_search)))

  ## Callback when client terminates session
  session$onSessionEnded(function() {
    print("Session ended.")
    #if (dbDisconnect(conf$con)) cat("Database connection closed.")
  })

  ## Url parameter String and Parameter List (reactive statements)
  ##    (Parameter string gets checked, see: nav/breadcrumb.shiny.r)
  paramstr <- reactive({urlparameter.checked(session$clientData$url_search)})
  paramlist <- reactive({urlparameter.as.list(paramstr())})

  ## Project id (reactive statement)
  pid <- reactive({
    pid <- paramlist()$projectid
    pid
  })

  ## Topic variable (reactive statement)
  topic <- reactive({t <- paramlist()$topic; if(is.null(t)) "overview" else t })

  ## Config file name variable (reactive statement)
  config.file <- reactive({paste("widget",topic(),"config",sep=".")})

  ## Render the breadcrumb (reactice output assignment)
  output$quantarchBreadcrumb <- renderUI({
    if (is.null(pid())) {
      renderBreadcrumbPanel("projects",paramstr())
      } else if (topic() == "overview") {
        renderBreadcrumbPanel("dashboard",paramstr())
        } else {
          renderBreadcrumbPanel("dashboard2",paramstr())
          }
    })

  ## Handle the project IDs in the cookie
  ## Returns the choices parameter for selectInput as a named vector
  choices <- projects.choices(projects.list)

  ## Returns a list of selected project names (reactive statement)
  selected <- reactive({ projects.selected( projects.list, input$qacompareids)
  })

  selected.pids <- reactive({
    if (is.null(input$qacompareids)) {
      return(list())
    } else {
      return(unlist(strsplit(input$qacompareids,",")))
  }})

  ## Outputs an enhanced selectInput "selectedpids" (reactive assignment)
  ##    (uses the chosen.jquery.js plugin)
  output$selectpidsui <- renderCompareWithProjectsInput(
    "selectedpids","",choices, selected(), list(width="100%"))

  ## Update the "qacompareids" cookie with input from the "selectedpids" selectInput
  ##    (but beware of duplicate project names)
  observe({
    dat <- input$selectedpids
    dat <- if(is.null(dat)) { list() } else { dat }
    ## TODO: pathLevel=1 does not seem to work
    updateCookieInput(session, "qacompareids", dat, pathLevel=0, expiresInDays=1)
  })

  ##
  ## Observe context executed once on session start
  ##
  initial.widget.config <- reactive({
    #print(pid)
    loginfo(paste("Current PID =",pid()))

    ## Obtain the widget configuration. If the PID is null, construct an overview
    ## configuration, using the widget input parameter if present. Otherwise,
    ## get the widget.config from a configuration file  (TODO: select secure path)
    if (is.null(pid())) {
      ## Create a volatile configuration  (not stored) if no projectid was found in Url
      cls.name <- "widget.overview.project"
      ## can also be defined via Url parameter "widget"
      if(!is.null(paramlist()$widget)) {
        cls.name <- paramlist()$widget
      }
      if (is.null(widget.list[[cls.name]])) {
        logerror(paste("No such widget found: ", cls.name))
        cls.name <- "widget.overview.project"
      }
      cls <- widget.list[[cls.name]]
      widget.config <- list(
        widgets=lapply(projects.list$id, function(pid) {
          w <- list(col = 1, row = 1,
               size_x = cls$size.x, size_y = cls$size.y,
               id = paste("widget",pid,sep=""),
               cls = cls.name,
               pid = pid)
          #force(w)
          #str(w)
          w
        })
      )
    } else if (topic() == "testall") {
      ## This topic shows all widgets for this project
      widget.config <- list(
        widgets=lapply(1:length(widget.list), function(i) {
          cls <- widget.list[[i]]
          w <- list(col = 1, row = 1,
               size_x = cls$size.x, size_y = cls$size.y,
               id = paste("widget", i, sep=""),
               cls = cls$widget.classes[[1]],
               pid = pid)
          w
        })
      )
    } else {
      ## Read from config file belonging to current topic (Url parameter)
      loginfo(paste("Try to read config file", config.file()))
      widget.config <- dget(config.file())
      if (is.null(widget.config)) {
        widget.config <- list(widgets=list())
      }
    }
    widget.config
  })
  
  widget.select.list <- reactive({
    topical.widgets <- sapply(widget.list, function(x) {
      is.null(x$topics) || topic() %in% x$topics })
    widget.titles <- vapply(widget.list[topical.widgets], FUN=function(x){
      x$name},FUN.VALUE=character(1))
    widget.select.list <- names(widget.titles)
    names(widget.select.list) <- widget.titles
    widget.select.list
  })
  
  ## Save the existing widget IDs so we don't overwrite them with new ones
  internal.getuniqueid.existing <<- sapply(isolate(initial.widget.config())$widgets, function(w) { w$id })
  
  ## Render the "Add Widget" dialog (disabled for pid==NULL)
  observe({
    sendGridsterButtonOptions(session, options=list(addwidget=!is.null(pid())))
  })
  
  output$addWidgetDialog <- renderUI({
    if (!is.null(pid())) {
      selectInput("addwidget.class.name", "Select Widget content:", widget.select.list())
    } else { NULL }
  })
  

  ## Send all widgets found in widget.config to the client
  for ( w in isolate(initial.widget.config())$widgets ) {
    ## NULL pid means that each widget has its own pid
    if (is.null(isolate(pid()))) {
      this.pid <- reactive({w$pid})
      ## This evaluation is necessary, since otherwise
      ## w changes in the for loop and all have the same pid!
      force(isolate(this.pid()))
    } else {
      this.pid <- pid
    }
    
    ## Build widget using the widgetbase.output builder
    loginfo(paste("Creating widget from config: ", w$id, "for classname: ", w$cls ))
    widget.classname <- as.character(w$cls)
    widget.class <- widget.list[[widget.classname]]

    ## widgetbase is the output object and holds the widget.ui and knows how to render this ui
    widgetbase <- widgetbase.output(input, output, w$id, widget.class, topic, this.pid, w$size_x, w$size_y, w$col, w$row, selected.pids)

    ## Send a custom message to Shiny client for adding the base widget
    sendWidgetContent(session, widgetbase)
  } # end for

  ##
  ## Observe the gridster action menu save button (see also: nav/gidsterWidgetExt.js)
  ##
  observe({
    ## Button input returns widget configuration as JSON
    cjson <- input$gridsterActionMenu
    #loginfo(paste("Got input from button:",cjson))
    if (!is.null(cjson) && isValidJSON(cjson,TRUE)) {
      ## Create an R list object (unnamed) from JSON
      ## The first element is either "save" or "update". If it is "save", the
      ## configuration should be saved to file, otherwise only the displayed
      ## widgets should be updated.
      button.info <- fromJSON(cjson)
      save.or.update <- button.info[[1]]
      widgets.displayed <- button.info[[2]]

      widget.config <- list(widgets=widgets.displayed)

      ## Save this configuration as a .config file
      if (save.or.update == "save" && (!is.null(pid()) || (topic() == "testall"))) {
        ## update configuration file
        ## TODO: save as cookie
        #widget.config$content <- widget.content
        dput(widget.config, file = config.file(),
              control = c("keepNA", "keepInteger", "showAttributes"))
        loginfo("Saved configuration file.")
      }
    }
  })

  ##
  ## Observe the "Add Widget" dialog input
  ##
  observe({

    ## modal dialog Save button will trigger this context
    if (input$addWidgetButton == 0) return()
    ## modal dialog selectInput is isolated, so it will only buffer the data
    widget.classname <- isolate({input$addwidget.class.name})

    ## check for null and empty string, because initially this could be delivered
    if (!is.null(widget.classname) && length(widget.classname) > 0) {
      ## Get a unique ID for the new widget
      id <- getuniqueid(prefix="widget")

      ## get the widget class
      widget.class <- widget.list[[widget.classname]]

      ## add html to widget instance which wraps into gridster item
      widgetbase <- widgetbase.output.new(input, output, id, widget.class, topic, pid, selected.pids)

      ## finally send widget base to client
      sendWidgetContent(session, widgetbase)
    } #end if
 }) #end observe
})
