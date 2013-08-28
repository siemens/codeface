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

	observe({
	
		#loginfo(paste("Dashboard: ", pid()))
	
		widgetlist <- list()
		##		
		widgetlist[[1]] <- list(
			html=tags$li( style="background-color: #DDD;box-shadow: 10px 10px 5px #CCC;", 
						  justgageOutput("live_gauge", width=250, height=200)), 
			size_x=1, size_y=1, col=1, row=1)
		##
		widgetlist[[2]] <- list(
			html=tags$li(
				style="background-color:yellow;box-shadow: 10px 10px 5px #CCC;",
				htmlOutput("tempWidget2")
				), 
			size_x=2, size_y=1, col=2, row=1)
		##	
		widgetlist[[3]] <- list(
		html=tags$li(
			style="background-color:blue;box-shadow: 10px 10px 5px #CCC;", 
			tags$p("Widget C")), 
			size_x=1, size_y=1, col=1, row=2)
		##
		widgetlist[[4]] <- list(
			html=tags$li(
				style="background-color:green;box-shadow: 10px 10px 5px #CCC;", 
				tags$p("Widget D")), 
				size_x=1, size_y=1, col=2, row=2)
		##
		widgetlist[[5]] <- list(
			html=tags$li(
				style="background-color:white;box-shadow: 10px 10px 5px #CCC;", 
				tags$p("Widget E")), 
				size_x=1, size_y=1, col=3, row=2)	

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
		#names(widget.list)
		#listViews(w)
		output[["tempWidget2"]] <- renderWidget(widget.list[["widget.general.info.overview"]]$new(pid()))

	}) # end observe

	## Reactive Block: Set the value for the gauge
	## TODO: improve the justgage integration to provide title and max values
	output$live_gauge <- reactive({
		## IMPLEMENTATION: provide some meaningful (reactive) value
		10.0 * as.integer(pid())
	  })

	## Reactive Block: Status output
	# output$status <- reactive({
		# list(text="Warn", subtext = "Mean of last 10 approaching threshold (200)",
			# gridClass="warning")
		# })

})
