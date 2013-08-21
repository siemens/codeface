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

suppressPackageStartupMessages(library(shiny))

## initialize globals in local scope, so they are visible for all user sessions

## get prosoda configuration
source("../../../config.r", chdir=TRUE, local=TRUE)  # this activates logging
## database functionality
## TODO: improve session management to avoid excessive number of open sessions
source("../../../query.r", chdir=TRUE, local=TRUE)
source("../../../dynamic_graphs/timeseries.r", chdir=TRUE, local=TRUE)
conf <- config.from.args(require_project=FALSE)
projects.list <- query.projects(conf$con) # this is needed by breadcrumb.r
##dbDisconnect(conf$con) # close database session, because we got the data needed for this app

## source the breadcrumb functionality
source(file.path("..","..","nav","breadcrumb.r"), chdir = TRUE, local=TRUE)

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
			

		ts <- get.ts.data(conf$con, pid)
		boundaries <- get.cycles.con(conf$con, pid)

		output$timeseriesWidget <- renderPlot({
			print(do.ts.plot(ts, boundaries, 0, 0))
			})
			
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
