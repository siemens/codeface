source("../common.server.r", chdir=TRUE)
## Load widgets (her only needed for demonstration)

source("qa_modal.r")
#source("qa_cookie.r")

##
## the Shiny server function
##

shinyServer(function(input, output, session) {
  
  observe({
    
    # render the modal dialog ui
    select.list <- projects.list$id
    names(select.list) <- projects.list$name
    output$selectpidsui <- renderUI(
      selectInput("selectedpids", "Select analysis", select.list, multiple=TRUE)) 
    })

  comparewith <- reactive({
    #print(input$selected)
    input$selected # submit button pressed
    if (input$selected == 0) return(NULL) # ignore if not yet pressed
    pids <- isolate({ input$selectedpids })
    updateCookieInput(session, "qacompareids", pids, pathLevel=0, expiresInDays=1 )
    return(pids)
    })


 output$monitor <- renderText({input$qacompareids})
#  output$monitor <- renderText({comparewith()})
  
  # comparewith must be requested to save the cookie!!! 
  # So better use an observer obove (not a reactive statement) for getting the modal dialog's input
  observe({
    print(comparewith())
  })
  
})