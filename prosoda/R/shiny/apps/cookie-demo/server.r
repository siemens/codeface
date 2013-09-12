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
  selected.projects <- reactive({
    projects.pids <- unlist(strsplit(input$qacompareids,","))
    projects.index <- projects.list$id %in% projects.pids
    unlist(projects.list$name[projects.index])})
  
  output$selectpidsui <- renderUI({
    print(selected.projects())  
    select <- selectInput("selectedpids", "", select.list, multiple=TRUE, selected=selected.projects())
    select.tag <- select[[2]]
    select.tag$attribs$class <- "chosen-select"
    select[[2]] <- select.tag
    tagList(select, tags$script('$(".chosen-select").chosen({width: "100%"});'))
  })

  comparewith <- reactive({
    pids <- input$selectedpids
    updateCookieInput(session, "qacompareids", pids, pathLevel=0, expiresInDays=1 )
    return(pids)
  })

  output$monitor <- renderText({ input$qacompareids })
  #output$monitor <- renderText({comparewith()})

  # comparewith must be requested to save the cookie!!!
  # So better use an observer obove (not a reactive statement) for getting the modal dialog's input
  observe({
    print(comparewith())
  })

})
