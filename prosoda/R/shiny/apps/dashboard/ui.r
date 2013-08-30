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
## Software Project Dashboard (ui.r) 
##

suppressPackageStartupMessages(library(shinyGridster))
source('../../nav/breadcrumbOutput.r')
source('gridsterWidgetsExt.r')

shinyUI(bootstrapPage(

	tags$head(
		
		## Adapt the styles as needed (some seem to be ignored)
		tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
		
		## For JustGage, http://justgage.com/
		tags$script(src = 'js/raphael.2.1.0.min.js'),
		tags$script(src = 'js/justgage.1.0.1.min.js'),
		# For the Shiny output binding for status text and JustGage
		tags$script(src = 'shiny_status_binding.js'),
		tags$script(src = 'justgage_binding.js')
		
		# for adding widgets dynamically by Shiny customMessageHandler
		#, tags$script(src = 'initwidgets.js')
	
	),
  
	div(class = "container",
		div(class = "row",
			breadcrumbOutput("quantarchBreadcrumb", gridsterGetConfigButton("saveConfigButton") )#uiOutput("quantarchBreadcrumb")
			#, h1(div(id = "dashboardTitleOutput", class = "shiny-text-output span12"))
			# ,tags$br()
			# ,selectInput("variable", "Variable:",
            # c("Cylinders" = "cyl",
              # "Transmission" = "am",
              # "Gears" = "gear"))
			),
		div(class = "row",
			div(class = "span12",
				gridster(width = 250, height = 250))
			)
		)

	))
