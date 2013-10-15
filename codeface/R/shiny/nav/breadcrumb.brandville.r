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


## Creates HTML code like this for dropdown breadcrumbs in HTML5 brandville design
##
## <div class="nav">
##   <nav>
##     <!-- NOTE FOR CMS INTEGRATION: Don't forget to place <b> tags around the last element -->
##     <!-- We need a link for the last element here to make it focusable -->
## 		<h1><a href="#"><b>Level 3</b></a></h1>
## 		<ul>
## 			<li><a href="#">Level 3 Item 1</a></li>
## 			<li><a href="#" class="path">Level 3 Item 2</a></li>
## 			<li><a href="#">Level 3 Item 3</a></li>
## 			<li><a href="#">Level 3 Item 4</a></li>
## 			<li><a href="#">Level 3 Item 5</a></li>
## 		</ul>
## 	</nav>
## </div>
##
## Parameters
## ==========
##   breadcrumbdata: list structure created by funtion breadcrumbPanelData()
##
## Returns
## =======
##	HTML code for Siemens brandville Styles
##
breadcrumbBrandville <- function( breadcrumbdata ) {

  popdown.tags <- function(x) {
    tags$li(a(href=as.character(x$url),as.character(x$label)))
  }

  navtags <- tagList()

  for (bc.element in breadcrumbdata) {

    childlist <- tags$ul(tagList(lapply(bc.element$children, popdown.tags)))
    navtag <- tags$div(class = "nav quantarch", tag("nav", list( h1( a(href = as.character(bc.element$url),
                                                                       as.character(bc.element$label))), childlist )))
    navtags <- tagAppendChild(navtags, navtag)
  }

  navtags

}
