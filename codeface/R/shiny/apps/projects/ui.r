## This file is part of Codeface. Codeface is free software: you can
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
## Software Projects Navigation Sidebar (ui.r) 
##

suppressPackageStartupMessages(library(shiny))

## TODO: This is needed, if we include scripts for breadcrumb navigation
#addResourcePath("navigation", "../../nav") # hint: do not add a final "/"

shinybootstrap2::withBootstrap2({
shinyUI(bootstrapPage(
	tags$div( class = "container",
		## div holds breadcrumb
		tags$div( class="row", 
			uiOutput("quantarchBreadcrumb"))
		
		## div holds navigation sidebar
		,tags$div( class="row", 
			tags$div( id="selectionlistelements", class="span4 well shiny-html-output" , list()),
			tags$div( class="span8", list() ))
		
		## TODO: init iframe communication, if needed
		#,tags$script(src = "navigation/initiframecomm.js")
		) 
))
})
