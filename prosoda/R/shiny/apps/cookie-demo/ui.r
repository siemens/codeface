source("qa_cookie.r")

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
            div( class="span2", style = "padding: 10px 0px;",  
                 htmlOutput("selectpidsui")))
    )
    ,
    cookieInput("qacompareids"))
))
