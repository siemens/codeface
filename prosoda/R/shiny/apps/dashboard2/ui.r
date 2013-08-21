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
source('dashwidgets.r', local=TRUE)
source('../../nav/quantarch-shiny.r', local=TRUE)
## TODO: This is needed, if we include scripts for breadcrumb navigation
addResourcePath("navigation", "../../nav") # hint: do not add a final "/"

shinyUI(bootstrapPage(

	tags$head(
		
		## TODO: adapt the styles as needed (or ignore it)
		tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
		## For JustGage, http://justgage.com/
		tags$script(src = 'js/raphael.2.1.0.min.js'),
		tags$script(src = 'js/justgage.1.0.1.min.js'),
		# For the Shiny output binding for status text and JustGage
		tags$script(src = 'shiny_status_binding.js'),
		tags$script(src = 'justgage_binding.js'),
		tags$script(src = 'navigation/initwidgets.js')
	
	),
  
	div(class = "container",
		div(class = "row",
			uiOutput("quantarchBreadcrumb"),
			h1(div(id = "dashboardTitleOutput", class = "shiny-text-output span12"))),
		div(class = "row",
			gridster(width = 250, height = 250
			#, class = "shiny-html-output", id = "gridsterOutput" 
				# gridsterItem(col = 1, row = 1, sizex = 1, sizey = 1, 
					# tags$p("Widget 1,1")
					# ),
				# ## sample plotOutput for timeseries plot
				# ## to assure that plot fits in widgets, must define height = "100%" or 200
				# gridsterItem(col = 2, row = 1, sizex = 2, sizey = 1,
					# plotOutput("timeseriesWidget",height="250px")
					# ),
				# gridsterItem(col = 1, row = 2, sizex = 1, sizey = 1,
					# justgageOutput("live_gauge", width=250, height=200)
					# ),
				# gridsterItem(col = 2, row = 2, sizex = 1, sizey = 1,
					# tags$div(class = 'grid_title', 'Status'),
					# statusOutput('status'),
					# a(href="apps/contributors/","more")
					# ),
				# gridsterItem(col = 3, row = 2, sizex = 1, sizey = 1,
					# tags$p("Widget 1,3")
					# )
				))
	)
	  ## Read Javascript code from a separate file
	  ## if needed, init iframe communication
	  #tags$script(src = "navigation/initiframecomm.js")
	))
