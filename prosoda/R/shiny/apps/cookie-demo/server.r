source("../common.server.r", chdir=TRUE)
## Load widgets (her only needed for demonstration)

#source("qa_cookie.r")

##
## the Shiny server function
##

shinyServer(function(input, output, session) {

  # render the modal dialog ui
  select.list <- projects.list$id
  names(select.list) <- projects.list$name
  output$selectpidsui <- renderUI({
    select <- selectInput("selectedpids", "", select.list, multiple=TRUE)
    select.tag <- select[[2]]
    select.tag$attribs$class <- "chosen-select"
    select[[2]] <- select.tag
    tagList(select, tags$script('$(".chosen-select").chosen()'))
  })

  comparewith <- reactive({
    #print(input$selected)
    pids <- input$selectedpids
    updateCookieInput(session, "qacompareids", pids, pathLevel=0, expiresInDays=1 )
    return(pids)
  })

  output$monitor <- renderText({ comparewith() })
  #output$monitor <- renderText({comparewith()})

  # comparewith must be requested to save the cookie!!!
  # So better use an observer obove (not a reactive statement) for getting the modal dialog's input
  observe({
    print(comparewith())
  })

})
