library(shiny)

## TODO: propably does not matter, if executed here or in server.r
addResourcePath("navigation", "../../nav") # hint: do not add a final "/"

shinyUI(bootstrapPage(
  ## div holds the list of links for navigator
  tags$div( class="row", 
    tags$div( id="selectionlistelements", class="span4 well shiny-html-output" , list()),
    tags$div( class="span8", list() )),
  
  
  ## init iframe communication (a resource path is relative to the server root)
  tags$script(src = "navigation/initiframecomm.js")
  
))