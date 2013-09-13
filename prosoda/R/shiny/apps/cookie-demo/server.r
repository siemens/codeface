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
