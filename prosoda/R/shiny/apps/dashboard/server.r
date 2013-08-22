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

	loginfo(isolate(session$clientData$url_search)) # log query string
  
	## values synchonizes the following reactive blocks  
	values <- reactiveValues()
	
	## Reactive Block: process session variables and create breadcrumb
	observe({
	
		## (1) process session variable to get project id
		paramstr <- session$clientData$url_search
		args.list <- parseQueryString(paramstr)
		pid <- args.list[["projectid"]]
		loginfo(paste("projectid=<",as.character(pid),">"))
		
		## (2) use project id to generate breadcrumb
		navData <- breadcrumbPanelData("dashboard", as.character(paramstr))
		output$quantarchBreadcrumb <- renderUI({
			breadcrumbPanel( navData )
			})
		
		## (3) output headline and include project name
		output$dashboardTitleOutput <- renderText({
			paste( 	as.character(projects.list$name[projects.list$id == pid]),
					"Dashboard")}
			)
		widget1 <- widget.list[[1]](pid)
		output$widget1 <- renderWidget(widget1)

		widget2 <- widget.list[[2]](pid)
		output$widget2 <- renderWidget(widget2)

		widget3 <- widget.list[[4]](pid)
		output$widget3 <- renderWidget(widget3)
			
		## IMPLEMENTATION: insert initial stuff depending on project id here   
 
	}) # end observe

	## Reactive Block: Set the value for the gauge
	## TODO: improve the justgage integration to provide title and max values
	output$live_gauge <- reactive({
		## IMPLEMENTATION: provide some meaningful (reactive) value
		10.0
	  })

	## Reactive Block: Status output
	output$status <- reactive({
		list(text="Warn", subtext = "Mean of last 10 approaching threshold (200)",
			gridClass="warning")
		})

})
