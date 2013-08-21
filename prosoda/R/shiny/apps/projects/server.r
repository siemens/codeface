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
## Software Projects Navigation Sidebar (server.r) 
##

suppressPackageStartupMessages(library(shiny))

## initialize globals in local scope, so they are visible for all user sessions

## get prosoda configuration
source("../../../config.r", chdir=TRUE, local=TRUE)  # this activates logging
## database functionality
## TODO: improve session management to avoid excessive number of open sessions
source("../../../query.r", chdir=TRUE, local=TRUE)
conf <- config.from.args(require_project=FALSE)
projects.list <- query.projects(conf$con) # this is needed by breadcrumb.r
dbDisconnect(conf$con) # close database session, because we got the data needed for this app

## source the breadcrumb functionality
source(file.path("..","..","nav","breadcrumb.r"), chdir = TRUE, local=TRUE)

##
## the server function
##
shinyServer(function(input, output, session) {
  
	loginfo(isolate(session$clientData$url_search)) # log query string

	## values synchonizes the following reactive blocks  
	values <- reactiveValues()

	## Reactive Block
	observe({

		## (1) Handle parameters

		paramstr <- session$clientData$url_search
		args.list <- parseQueryString(paramstr)
		pid <- args.list[["projectid"]]
		#loginfo(paste("projectid=<",as.character(pid),">"))
		
		## (2) Create and render breadcrumb navigation as HTML 
		
		navData <- breadcrumbPanelData("quantarch", as.character(paramstr))
		
		## this is needed for iframe integration only
		#print(navData)
		##breadcrumbhtml <- as.character(breadcrumbBrandville( navData ))
		#logdebug(breadcrumbhtml)
		# session$sendCustomMessage(
		  # type = "sendToIframeParent", 
		  # message = list(
			# # Name of message to send
			# msgname = "setNav",
			# navhtml = breadcrumbhtml
			# ))
		
		## this is needed for direct integration
		output$quantarchBreadcrumb <- renderUI({
			breadcrumbPanel( navData )
			})
			
		## (3) Handle project selected by URL parameter "projectid" (if any)
		
		selectedId <- 0 # assume that nothing was selected (no URL parameter)
		projects.n <- nrow(projects.list) # get number of projects
		if (projects.n == 0) stop("no projects") # must be non-zero
		## if URL parameter supplied a project id, check valid range
		if (!is.null(pid) && !(str_trim(as.character(pid)) == "")) {
			selectedId <- as.integer(pid)
			if (selectedId < 1) selectedId <- 1
			if (selectedId > projects.n) selectedId <- projects.n  
			}

		## finally ends with a valid selection
		## store in values to trigger output
		values$sid <- selectedId

		}) # end observe
    
	
	## Reactive Block: Output navigation sidebar with current projects
	output$selectionlistelements <- renderUI({      
	
		selectedItem <- values$sid
		childrenIds <- nav.list$quantarch$childrenIds(paramstr) # reuse info from nav.list
		#print(childrenIds)
		
		## childtags holds the html for the list elements
		childtags <- tagList()
		
		if (nrow(childrenIds) > 0) {
		  for (i in 1:nrow(childrenIds)) {
			
			ptr <- nav.list[[as.character(childrenIds$id[i])]]
			
			cparamstr <- as.character(childrenIds$params[i])
			
			if ( i == selectedItem) {
			  childtags <- tagAppendChild(childtags, 
				  tags$li( class = "active", a(href = ptr$url(cparamstr), ptr$label(cparamstr))))
			} else {
			  childtags <- tagAppendChild(childtags, 
				  tags$li(a(href = ptr$url(cparamstr), ptr$label(cparamstr))))
			}
			#print(childtags)
		  }
		}
    	#print(childtags)
		
		## finally return the html
		tagsul <- tags$ul( class = "nav nav-list", 
							tags$li( class="nav-header",  "Open Source Projects"))
		tagAppendChild( tagsul, childtags )
		})

})