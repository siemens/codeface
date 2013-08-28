## This file is part of prosoda.  prosoda is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

##
## Software Project Dashboard-Test (server.r) 
##

library(shiny)

#
# Init widgetlist
#

widgetlist <- list()
##		
widgetlist[[1]] <- list(
  id=1,
  html=tags$li( style="background-color: #DDD;box-shadow: 10px 10px 5px #CCC;", 
                tags$i( class="icon-remove-sign hidden", style="float:right"),  # could also use class="pull-right"
                tags$div( id="widget1", class="shiny-html-output", tags$p("Widget 1"))), 
  size_x=1, size_y=1, col=1, row=1)
##
widgetlist[[2]] <- list(
  id=2,
  html=tags$li(
    style="background-color:yellow;box-shadow: 10px 10px 5px #CCC;",
    tags$i( class="icon-remove-sign hidden", style="float:right"),
    tags$div( id="widget2", class="shiny-html-output", tags$p("Widget 2")) 
  ), 
  size_x=2, size_y=1, col=2, row=1)
##	
widgetlist[[3]] <- list(
  id=3,
  html=tags$li(
    style="background-color:blue;box-shadow: 10px 10px 5px #CCC;", 
    tags$i( class="icon-remove-sign hidden", style="float:right"),
    tags$div( id="widget3", class="shiny-html-output", tags$p("Widget 3"))), 
  size_x=1, size_y=1, col=1, row=2)
##
widgetlist[[4]] <- list(
  id=4,
  html=tags$li(
    style="background-color:green;box-shadow: 10px 10px 5px #CCC;", 
    tags$i( class="icon-remove-sign hidden", style="float:right"),
    tags$div( id="widget4", class="shiny-html-output", tags$p("Widget 4"))), 
  size_x=1, size_y=1, col=2, row=2)
##
widgetlist[[5]] <- list(
  id=5,
  html=tags$li(
    style="background-color:white;box-shadow: 10px 10px 5px #CCC;", 
    tags$i( class="icon-remove-sign hidden", style="float:right"),
    tags$div( id="widget5", class="shiny-html-output", tags$p("Widget 5"))), 
  size_x=1, size_y=1, col=3, row=2)	

##
## the server function
##

shinyServer(function(input, output, session) {

  
  sendWidgetContent <- function(w) {
    session$sendCustomMessage(
    type = "GridsterMessage", 
    message = list(
      msgname = "addWidget", 				# Name of message to send
      html = as.character(w$html),		# this is the html for the widget
      size_x = as.character(w$size_x),	# in units of grid width
      size_y = as.character(w$size_y),	# dto
      col = as.character(w$col),			# column in grid
      row = as.character(w$row)			# row in grid
    )
  )}
  
	observe({
	  #session
		
    ## Render uiOutputs to to widgets

    for ( w in widgetlist ) {
			session$sendCustomMessage(
				type = "GridsterMessage", 
				message = list(
					msgname = "addWidget", 				# Name of message to send
					html = as.character(w$html),		# this is the html for the widget
					size_x = as.character(w$size_x),	# in units of grid width
					size_y = as.character(w$size_y),	# dto
					col = as.character(w$col),			# column in grid
					row = as.character(w$row)			# row in grid
					)
				)
			}
	  
    ## Render widget content into uiOutputs
    
    output[["widget5"]] <- renderUI({
      #cat(toString(input$gridsterActionMenu),"\n")
      if(length(input$gridsterActionMenu) == 0) {
        "nix"
      } else { 
        tags$p(input$gridsterActionMenu)
      }})
    
    
    output$addWidgetDialog <- renderUI( selectInput("addwidget.class.name", "Widget content:",
                                                   c("Cylinders" = "cyl.class",
                                                     "Transmission" = "am.class",
                                                     "Gears" = "gear.class")))
	}) # end observe

  
  observe({
    newWidgetClass <- input$addwidget.class.name
    ## check for length > 0, because initially this will be delivered automatically
    if (length(newWidgetClass) > 0) {  
      newWidgetId <- widgetlist[[length(widgetlist)]]$id + 1
      newWidgetName <- paste("widget", as.character(newWidgetId))
      widgetlist[[newWidgetId]] <- list( 
        id=newWidgetId,
        html=tags$li(
          style="background-color:white;box-shadow: 10px 10px 5px #CCC;", 
          tags$i( class="icon-remove-sign hidden", style="float:right"),
          tags$div( id=newWidgetName, class="shiny-html-output", 
                    tags$p(paste("This was added:",newWidgetClass,sep = "<br>")))), 
        size_x=1, size_y=1, col=3, row=2)  
        sendWidgetContent(widgetlist[[newWidgetId]])
      }
    
  })
  
  #output$testid <- renderText(input$gridsterActionMenu)
	output$testid <- renderText(input$addwidget.class.name)
   
})
