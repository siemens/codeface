# This file is part of prosoda.  prosoda is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2013 by Siemens AG, Albert Eckert <albert.eckert@siemens.com>


source("../common.server.r", chdir=TRUE)

shinyServer(function(input, output, session) {

	loginfo(isolate(session$clientData$url_search)) # log query string

  observe({
    if (is.null(input$currentapp))
        app.class <- c("projects")
      else
        app.class <- input$currentapp
    loginfo(paste("app.class=",app.class))
    paramstr <- urlparameter.checked(session$clientData$url_search)
    loginfo(paste("paramstr= ", paramstr))
    output$quantarchBreadcrumb <- renderUI({renderBreadcrumbPanel(app.class,paramstr)})
    })

## alternative usage without observer:
##
#     app.class <- reactive({
#       if (is.null(input$currentapp))
#         c("projects")
#       else
#         input$currentapp
#      })
#     loginfo(paste("app.class=",isolate(app.class())))
#     paramstr <- reactive({urlparameter.checked(session$clientData$url_search)})
#     loginfo(paste("Dashboard: ", isolate(app.class()), isolate(paramstr())))
#     output$quantarchBreadcrumb <- renderUI({renderBreadcrumbPanel(app.class(),paramstr())})

## when the client session ends, database connections will be closed down

	session$onSessionEnded(function() {
	  print("Session ended.")
    if (dbDisconnect(conf$con)) cat("Database connection closed.")
	})
})

