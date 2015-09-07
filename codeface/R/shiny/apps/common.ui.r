#! /usr/bin/env Rscript

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

shinybootstrap2::withBootstrap2({
shinyUI(bootstrapPage(
  tagList(
    tags$div( class = "container",
      ## first row contains breadcrumb and selectors for projects to compare with
  		tags$div( class="row", style="margin-top: 10px; height: 36px;",
  		    tags$div( class = "span8", style = "height: 100%;",
                   breadcrumbOutput("quantarchBreadcrumb")),
  		    tags$div( class = "span4",
                   compareWithProjectsOutput("selectpidsui"))
          ),
      ## second row contains some header or title
  	  tags$div( class="row",
  		  tags$div( class="span12", style = "padding: 10px 0px;", 
                  tags$h1( textOutput("quantarchHeader")))
        ),
      ## third row is the content
      tags$div( class="row", 
        uiOutput("quantarchContent") )
      ),
  	  ## finally some hidden input
  	  cookieInput("qacompareids"))
))
})
