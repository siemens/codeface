## do not source or load here, if not exclusively needed (user server.r instead)
shinyUI(bootstrapPage(
  tagList(
    
    singleton(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap-carousel.css"), 
      tags$script(src = "bootstrap-carousel.js")
      )),
    
    div(class = "container",
        div(class = "row",
            div( class="span12", style = "padding: 10px 0px;",
                 includeHTML('www/carousel.html'))
        )
    )
  )
))
