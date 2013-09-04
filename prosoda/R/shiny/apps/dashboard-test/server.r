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

##
## Software Project Dashboard-Test (server.r) 
##

suppressPackageStartupMessages(library(RJSONIO))

source("../common.server.r", chdir=TRUE)

##
## Widget-List
## ===========
##
## (1) get widget.list
##
source("../../widgets.r", chdir=TRUE)

##
## (2) Filter Widget List
##
## Remove widgets that take too long to load
widget.list.filtered <- widget.list[
  names(widget.list) != "widget.commit.structure.mds" &
    names(widget.list) != "widget.punchcard.ml"
  ]

##
## the server function
##

shinyServer(function(input, output, session) {
  
  ## log the Url parameters
  loginfo(isolate(session$clientData$url_search))
  
  ## Callback when client terminates session
  session$onSessionEnded(function() {
    print("Session ended.")
    #if (dbDisconnect(conf$con)) cat("Database connection closed.")
  })
  
  ## render base widget (assumes that cls, wid and pid have valid values)
  ##
  ## Parameters
  ## ==========
  ##  cls: widget class from widget.list
  ##  wid: html element id of shiny output
  ##  pid: Project-Id (integer)
  ##
  ## Returns
  ## =======
  ##  widget instance complemented with base html
  ##
  widgetbase.output.new <- function(id, w, pid) {
    widgetbase.output(id,w,pid,w$size.x, w$size.y, 1, 1 )
  }
  
  widgetbase.output <- function(id, w, pid, size_x=NULL, size_y=NULL, col=NULL, row=NULL) {
    wb <- list()
    tryCatch({
      inst <- w$new(pid)    
      wb$id <- id
      wb$widget <- inst
      wb$widget.class <- w
      wb$html <- tags$li(
        style=paste("background-color:",widgetColor(inst),";box-shadow: 10px 10px 5px #CCC;", sep=""),
        tags$i( class="icon-remove-sign hidden", style="float:right"),
        w$html(id)
      )
      wb$size_x <- size_x
      wb$size_y <- size_y
      wb$col <- col
      wb$row <- row
    }, warning = function(warn) {
      logwarn(paste("widgetbase.output.new(id=", id, " w=<", w$title,">, pid=",pid,":", toString(warn)))
    }, error = function(err) {
      logerror(paste("widgetbase.output.new(id=", id, " w=<", w$title,">, pid=",pid,":", toString(err)))
    }, {})
    wb
  }  
  
  
  ## Send a widget
  sendWidgetContent <- function(w) {
    session$sendCustomMessage(
    type = "GridsterMessage", 
    message = list(
      msgname = "addWidget", 				# Name of message to send
      html = as.character(w$html),		# this is the html for the widget
      size_x = as.character(w$size_x),	# in units of grid width
      size_y = as.character(w$size_y),	# dto
      col = as.character(w$col),			# column in grid
      row = as.character(w$row)			# row in grid
    )
  )}
  
  getuniqueid <- function( x = list(), prefix = "") {
    idrange <- length(x)+10
    newid <- paste(prefix,as.character(sample(1:idrange,1)),sep="")
    while ((newid %in% names(x))) {
      newid <- paste(prefix,as.character(sample(1:idrange,1)),sep="")
    }
    newid 
  }
  
  ## project id is set once after executing the server fun
  pid <- NULL
  widget.config <- list()
  widget.content <- list() 
  widgets.for.rendering <- list() #all generated widgets created
  
  
  ## observe context executed once on session start
  observe({
    
    ## get url parameter string and extract projectid 
    paramstr <- urlparameter.checked(session$clientData$url_search)
    loginfo(paste("valid paramstr =",paramstr))
    paramlist <- urlparameter.as.list(paramstr)
    pid <<- isolate(paramlist$projectid)
    #print(pid)
    
    ## output breadcrumb
    output$quantarchBreadcrumb <- renderUI({renderBreadcrumbPanel("dashboard",paramstr)})
    
    ## get the stored widget configuration (TODO: select secure path)
    loginfo("Try to read widget.config")
    widget.config <- dget("widget.config") # must exist
    if (is.null(widget.config)) {
      widget.config <- list(widgets=list(), content=list())
    }
    #cat("Read widget.config:")
    #print(widget.config)
    widget.content <- widget.config$content # maps widget ids to content renderers
        
    ## render all widgets found in config
    for ( w in widget.config$widgets ) {
      loginfo(paste("Creating widget from config: ", w$id, "for class: ", widget.content[[w$id]] ))
      widget.classname <- as.character(widget.content[[w$id]]) 
      widget.class <- widget.list[[widget.classname]]
      widgetbase <- widgetbase.output(w$id, widget.class, pid, w$size_x, w$size_y, w$col, w$row)
      sendWidgetContent(widgetbase)
      widgets.for.rendering[[w$id]] <<- widgetbase
      }
    
    ## render the add widget dialog
    widget.titles <- vapply(widget.list, FUN=function(x){x$title},FUN.VALUE=character(1))
    select.list <- names(widget.titles)
    names(select.list) <- widget.titles
    #print(widget.titles)
    output$addWidgetDialog <- renderUI( 
      selectInput("addwidget.class.name", "Select Widget content:", select.list))

    }) # end observe  
    
    
  ##TODO : listViews(w)
  
  ## Observe the gridster action menu button
  
    ## example JSON delivered by button input: 
    ## {"widgets":[{"col":1,"row":1,"size_x":1,"size_y":1,"id":"widget1"},{...}],
    ##   "content":["class1",...]}
    ##
    ## see also: nav/gidsterWidgetExt.js
  
  observe({
    cjson <- input$gridsterActionMenu 
    ## input updates whenever the client side widget configuration has been updated
    
    ## just for debugging
    loginfo(paste("got input from button:",cjson))
    
    if (!is.null(cjson) && isValidJSON(cjson,TRUE)) {
      widgets.current <- fromJSON(cjson)
      #print(widgets.current)
      n1 <- vapply(widgets.current, FUN=function(x){x$id},FUN.VALUE=character(1))
      #n1 <- widgets.current$id
      #print(n1)
      n2 <- names(widgets.for.rendering)
      #print(n2)
      n3 <- n2[n2 %in% n1]
      
      for (n in n3) {
        local({
        tryCatch({
          wout <- widgets.for.rendering[[n]]
          widget.content[[n]] <<- class(wout$widget)[1]
          #views <- listViews(wout$widget)
          ## isolate from this environment

              output[[wout$id]] <- renderWidget(wout$widget)
        
          ## remove rendered widget from rendering list
          #widgets.for.rendering[[n]] <<- NULL
          
          }, warning = function(wr) {
            logwarn(paste("While rendering widget", wout$widget.class$title, ":", toString(wr)))
          }, error = function(e) {
            logerror(paste("While rendering widget", wout$widget.class$title, ":", toString(e)))
          }, {})
        }) # end local
        } # end for
      
      ## update configuration file
      ## TODO: move to extra observe block
      ## TODO: save as cookie
      widget.config$widgets <- fromJSON(cjson)
      widget.config$content <- widget.content
       dput(widget.config, file = "widget.config",
            control = c("keepNA", "keepInteger", "showAttributes")) 
      } #end if
    
    ## debug output to screen
    #output$testid <- renderText(paste(cjson,toJSON(widget.content)))
    
    }) # end observe


  ## observes the add widget dialog input
  
  observe({
    
    ## modal dialog Save button will trigger this context
    if (input$addWidgetButton == 0) return()
    
    ## modal dialog selectInput is isolated, so it will only buffer the data
    widget.classname <- isolate({input$addwidget.class.name})
    
    ## check for null and empty string, because initially this could be delivered
    if (!is.null(widget.classname) && length(widget.classname) > 0) {
      ## get a new widgetid
      id <- getuniqueid(widget.content, prefix="widget")
      ## save widget class to widget id to class map
      widget.content[[id]] <<- widget.classname
      
      ## create the widget class
      widget.class <- widget.list[[widget.classname]]
      
      ## add html to widget instance which wraps into gridster item
      widgetbase <- widgetbase.output.new(id, widget.class, pid)
      
      loginfo(paste("Creating new widget: ", id, "for class: ", widget.content[[id]] ))
      
      ## finally send widget base to client
      sendWidgetContent(widgetbase)
      
      ## push widget instance to rendering list
      ## when a new widget has been added, the widget button input will trigger
      ## the addition of content
      widgets.for.rendering[[id]] <<- widgetbase
      } #end if
    
  }) #end observe
   
})
