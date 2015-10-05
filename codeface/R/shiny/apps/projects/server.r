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
## Software Projects Navigation Sidebar (server.r)
##

##
## the server function
##
shinybootstrap2::withBootstrap2({
shinyServer(function(input, output, session) {
  paramstr <- reactive({urlparameter.checked(session$clientData$url_search)})
  observe({
    output$quantarchBreadcrumb <- renderUI({renderBreadcrumbPanel("projects",paramstr())})
  })
  args.list <- reactive({urlparameter.as.list(paramstr())})
  ## Read out PID from the URL and check if it is valid
  pid <- reactive({ args.list()[["projectid"]] })

	## Calculate valid selected PID
	valid.pid <- reactive({
    projects.n <- nrow(projects.list);
		if (projects.n == 0) stop("no projects") # must be non-zero
    valid.pid <- 0
		## if URL parameter supplied a project id, check valid range
		if (!is.null(pid()) && !(str_trim(as.character(pid())) == "")) {
			valid.pid <- as.integer(pid())
			if (valid.pid < 1) valid.pid <- 1
			if (valid.pid > projects.n) valid.pid <- projects.n
    }
    valid.pid
  })

	## Reactive Block: Output navigation sidebar with current projects
	output$selectionlistelements <- renderUI({
		childrenIds <- nav.list$projects$childrenIds(paramstr()) # reuse info from nav.list
		## childtags holds the html for the list elements
		childtags <- tagList()

		if (nrow(childrenIds) > 0) {
		  for (i in 1:nrow(childrenIds)) {
			  ptr <- nav.list[[as.character(childrenIds$id[i])]]
  			cparamstr <- as.character(childrenIds$params[i])
  			if ( i == valid.pid()) {
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
}) # end of shinybootstrap
