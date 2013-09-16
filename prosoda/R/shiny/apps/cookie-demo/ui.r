## do not source or load here, if not exclusively needed (user server.r instead)
shinyUI(bootstrapPage(
  tagList(
    
    singleton(tags$head(tags$link(rel = "stylesheet", 
                                  type = "text/css",  
                                  href = "chosen.min.css"), 
                        tags$script(src = "chosen.jquery.min.js"))),
    
    div(class = "container",
        div(class = "row",
            div( class="span8", style = "padding: 10px 0px;",
                 textOutput("monitor")),
            div(class = "span4", style = "padding: 10px 0px;",
                #htmlOutput("selectpidsui")
                 compareWithProjectsOutput("selectpidsui"))
        )
    ),
    cookieInput("qacompareids"))
))
