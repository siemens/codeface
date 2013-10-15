# Copyright Siemens AG 2013
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

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
