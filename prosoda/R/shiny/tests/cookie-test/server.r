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
## Copyright 2013 by Siemens AG, Albert Eckert <albert.eckert@siemens.com>
## All Rights Reserved.

source("../common.server.r", chdir=TRUE)
## Load widgets (her only needed for demonstration)

source("qa_cookie.r")

##
## the Shiny server function
##
shinyServer(function(input, output, session) {

#   select.list <- projects.list$id
#   names(select.list) <- projects.list$name
  
  ## returns the choices named vector
  choices <- projects.choices(projects.list)
  ## returns a reactive list containing selected projects
  selected <- reactive({ projects.selected( projects.list, input$qacompareids) })
  
  ## demoes how to use the choices and adding options for chosen.jquery.js
  output$selectpidsui <- renderCompareWithProjectsInput(
    "selectedpids","",choices, selected(), list(width="100%"))

  ## demoes how to update the cookies from the "selectedpids" ui input
  ## also available via choices (but beware of duplicate project names)
  observe({
    updateCookieInput(session, "qacompareids", input$selectedpids, pathLevel=0, expiresInDays=1 )
  })

  ## monitor the current cookie's value
  #output$monitor <- renderText({ input$qacompareids })

})
