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

# get widget.list
source("../../widgets.r", chdir=TRUE)

# >>>>>>deprecated:
# widgetlist <- list()
# ##		
# widgetlist[[1]] <- list(
#   id=1,
#   html=tags$li( style="background-color: #DDD;box-shadow: 10px 10px 5px #CCC;", 
#                 tags$i( class="icon-remove-sign hidden", style="float:right"),  # could also use class="pull-right"
#                 tags$div( id="widget1", class="shiny-html-output", tags$p("Widget 1"))), 
#   size_x=1, size_y=1, col=1, row=1)
# ##
# widgetlist[[2]] <- list(
#   id=2,
#   html=tags$li(
#     style="background-color:yellow;box-shadow: 10px 10px 5px #CCC;",
#     tags$i( class="icon-remove-sign hidden", style="float:right"),
#     tags$div( id="widget2", class="shiny-html-output", tags$p("Widget 2")) 
#   ), 
#   size_x=2, size_y=1, col=2, row=1)
# ##	
# widgetlist[[3]] <- list(
#   id=3,
#   html=tags$li(
#     style="background-color:blue;box-shadow: 10px 10px 5px #CCC;", 
#     tags$i( class="icon-remove-sign hidden", style="float:right"),
#     tags$div( id="widget3", class="shiny-html-output", tags$p("Widget 3"))), 
#   size_x=1, size_y=1, col=1, row=2)
# ##
# widgetlist[[4]] <- list(
#   id=4,
#   html=tags$li(
#     style="background-color:green;box-shadow: 10px 10px 5px #CCC;", 
#     tags$i( class="icon-remove-sign hidden", style="float:right"),
#     tags$div( id="widget4", class="shiny-html-output", tags$p("Widget 4"))), 
#   size_x=1, size_y=1, col=2, row=2)
# ##
# widgetlist[[5]] <- list(
#   id=5,
#   html=tags$li(
#     style="background-color:white;box-shadow: 10px 10px 5px #CCC;", 
#     tags$i( class="icon-remove-sign hidden", style="float:right"),
#     tags$div( id="widget5", class="shiny-html-output", tags$p("Widget 5"))), 
#   size_x=1, size_y=1, col=3, row=2)	
#<<<<<<<


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
  
  ## render base widget (assumes that id, size_x and size_y have valid values)
  renderBaseWidget <- function(w) {
    w$html <- tags$li(
           style="background-color:white;box-shadow: 10px 10px 5px #CCC;", 
           tags$i( class="icon-remove-sign hidden", style="float:right"),
           tags$div( id=w$id, class="shiny-html-output", tags$p(w$id)))
    if (is.null(w$col)) w$col <- 1
    if (is.null(w$row)) w$row <- 1
    w
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
    cat("Read widget.config:")
    print(widget.config)
    widget.content <- widget.config$content # maps widget ids to content renderers
    
    ## render all widgets found in config
    for ( w in widget.config$widgets ) {
      loginfo(paste("Creating widget: ", w$id))
      bw <- renderBaseWidget(w)
      sendWidgetContent(bw)
      contentclass <- widget.content[[bw$id]]
      loginfo(contentclass)
      if (!is.null(contentclass)) {
        widgetclassname <- as.character(contentclass)    
        loginfo(paste("Adding widget content:",as.character(widgetclassname)))
        widgetcontent <- widget.list[[widgetclassname]]$new(pid)
        output[[bw$id]] <- renderWidget(widgetcontent)
        }
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
    
    ## observe the gridster action menu button (delivers widget config as JSON)
    ## example JSON: 
    ## {"widgets":[{"col":1,"row":1,"size_x":1,"size_y":1,"id":"widget1"},{...}],
    ##   "content":["class1",...]}
    ## see also: nav/gidsterWidgetExt.js
    observe({
      cjson <- input$gridsterActionMenu
      ## just for debugging
      output$testid <- renderText(paste(cjson,toJSON(widget.content)))
      
      loginfo(paste("got input from button:",cjson))
      
      if (!is.null(cjson) && isValidJSON(cjson,TRUE)) {
        #cat("OK")
         widget.config$widgets <- fromJSON(cjson)
         ##TODO: check if content for widgets still needed
         ## unneeded outputs should no longer exist after unbindall() ??? 
         widget.config$content <- widget.content
         #cat("New widget.config (for saving):")
         print(widget.config)
         dput(widget.config, file = "widget.config",
              control = c("keepNA", "keepInteger", "showAttributes")) 
        } else {
          loginfo(paste("Action menue returned:",cjson))
        }
      })
 

  ## observes add widget dialog inputs
  observe({
    if (input$addWidgetButton == 0) return()
    Widget.list.index <- isolate({input$addwidget.class.name})

    ## check for length > 0, because initially this will be delivered automatically
    if (!is.null(Widget.list.index) && Widget.list.index > 0) {
      #print(Widget.list.index)
      #print(widget.list[[Widget.list.index]])
      
      ## first get a new widgetid (e.g. get random number, until widget id is unique)
      new.widgetid <- getuniqueid(widget.content, prefix="widget")
      ## widget.content must be updated before adding the widget 
      ## otherwise it wont be stored into widget.config
      widget.content[[new.widgetid]] <<- Widget.list.index
      # create the widget
      newwidget <- list(
        html=tags$li(
          style="background-color:white;box-shadow: 10px 10px 5px #CCC;", 
          tags$i( class="icon-remove-sign hidden", style="float:right"),
          tags$div( id=new.widgetid, class="shiny-html-output", 
            tags$p(paste("Widget will display:",widget.list[[Widget.list.index]]$title,sep = "")))), 
        size_x=widget.list[[Widget.list.index]]$size_x, 
        size_y=widget.list[[Widget.list.index]]$size_y, 
        col=1, 
        row=1)  
      sendWidgetContent(newwidget)
      
      output[[new.widgetid]] <- renderWidget(widget.list[[Widget.list.index]]$new(pid))
      }
    
  })
  
  
	#output$testid <- renderText(input$addwidget.class.name)
   
})
