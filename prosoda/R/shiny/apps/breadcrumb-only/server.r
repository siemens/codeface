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

