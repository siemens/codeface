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
## Software Project Dashboard (server.r)
##

source("../common.server.r", chdir=TRUE)

## Load widgets
source("../../widgets.r", chdir=TRUE)

##
## the server function
##

shinyServer(function(input, output, session) {
	#loginfo(isolate(session$clientData$url_search)) # log query string
	pid <- common.server.init(output, session, "dashboard")

  ## Remove widgets that take too long to load
  widgets <- widget.list

  values <- reactiveValues()

  widgetlist <- list()
  for (i in 1:length(widgets)) {
    cls <- widgets[[i]]
    tryCatch({
      values[[cls$name]] <- NULL
      view <- reactive({values[[cls$name]]})
      view <- reactive({NULL})
      w <- newWidget(cls, pid, view)
      id <- paste("widgetBox", i, sep="")
      widgetlist[[i]] <- list(
        id=id,
        widget=w,
        widget.class=cls,
        html=
          tags$li(
            style=paste("background-color:white;box-shadow: 10px 10px 5px #CCC;", sep=""),
            cls$html(id)
          ),
        size_x=cls$size.x,
        size_y=cls$size.y,
        col=1, row=i
      )
    }, warning = function(wr) {
      logwarn(paste("While adding widget", cls$name, ":", wr$message))
      traceback(wr)
    }, error = function(e) {
      logerror(paste("While adding widget", cls$name, ":", e$message))
      traceback(e)
    }, {})
  }

	## Process widget list
	for ( w in widgetlist ) {
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
      )
  }

  ## Render all once-only widgets
  observe({
    cat("Rendering all widgets once!\n")
    ## Render all widgets
    for ( w in widgetlist ) {
      tryCatch({
        output[[w$id]] <- renderWidget(w$widget)
      }, warning = function(wr) {
        logwarn(paste("While rendering widget", w$widget.class$name, ":", wr$message))
      }, error = function(e) {
        logerror(paste("While rendering widget", w$widget.class$name, ":", e$message))
      }, {})
    }
  })

  ## Render all animated views
  #dashboard.view.animation.i <<- 0
  #observe({
    #cat("Rendering all widgets!\n")
    ### Render all widgets
    #for ( w in widgetlist() ) {
      #tryCatch({
        #views <- listViews(w$widget)()
        #if (length(views) > 1) {
          #view.id <- dashboard.view.animation.i %% length(views) + 1
          #view <- views[[view.id]]
          #cat("Rendering view :")
          #str(view)
          #values[[w$widget$name]] <- view
        #}
      #}, warning = function(wr) {
        #logwarn(paste("While rendering widget", w$widget.class$name, ":", wr$message))
      #}, error = function(e) {
        #logerror(paste("While rendering widget", w$widget.class$name, ":", w$message))
      #}, {})
    #}
    #cat("View animation ID: ", dashboard.view.animation.i, "\n")
    #dashboard.view.animation.i <<- dashboard.view.animation.i + 1
    #invalidateLater(4000, session)
  #})
})
