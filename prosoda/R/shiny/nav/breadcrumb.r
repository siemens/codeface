#! /usr/bin/env Rscript

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
## Central configuration of navigation elements for Quantarch apps 
##

## REMARKS: - All sourcing is done in server.r. 
##			- The only data needed currently is projects.list

## TODO: Move testing to test_breadcrumb.r
## UNCOMMENT THIS FOR STANDALONE TESTING
# if (interactive()) {
  # suppressPackageStartupMessages(library(shiny))
  # suppressPackageStartupMessages(library(logging))
  # source("../../config.r", chdir=TRUE)
  # source("../../query.r", chdir=TRUE)
  # conf <- config.from.args(require_project=FALSE)
  # projects.list <- query.projects(conf$con)
# }
## ALTERNATIVE TESTING: dummy projects.list (for demonstration if no database available)
##projects <- list("4" = "qed", "6" = "linux core", "7" = "github")
##id <- c(4, 6, 7)
##name <- c("qed","linux core", "github")
##projects.list <- data.frame(id, name)

suppressPackageStartupMessages(library(RJSONIO))

## nav.list holds the methods used to generate the breadcrumb
nav.list <- list()

## configuration of nav.list
## adapt this for every app used

################## START CONFIGURATION SECTION #####################

## configure the base application (this is where projects are selected)
nav.list$quantarch <- list(
  ## (1) Configure he label displayed in the breadcrumb entry 
  label = function(paramstr = NULL) {
	"Quantarch"
	},
  ## (2) configure URL for the breadcrumb entry 
  url = function(paramstr = NULL) {
	"../projects/"
	}, # params kann man z.B. zum highliten verwende
  
  ## (3) configure children displayed in dropdown
  childrenIds = function(paramstr = NULL) {
	id <- c("dashboard") # in this example we have 1 child
	params <- paste("projectid",projects.list$id,sep = "=") # needs project id
	data.frame(id, params)
	},
  
  ## (4) configure parent
  parentId = function(paramstr = NULL) {
	NULL # no parent
	})



## Configure contributors app
project.apps <- list(
    c("commit.info", "Commit Information"),
    c("commit.structure", "Commit Structure"),
    c("contributions", "Contributions overview"),
    c("contributors", "Contributors"),
    c("punchcard", "Activity punch cards"),
    c("punchcard_ml", "ML activity punch cards"),
    c("release_distance", "Inter-Release Distance"),
    c("timeseries", "Mailing list activity"),
    c("vis.clusters", "Collaboration clusters")
)

constant.func <- function(value) {
  return(function(paramstr = NULL) { value })
}

constant.func.url <- function(name) {
  return(function(paramstr = NULL) { paste("../", name, "/?", paramstr, sep='') })
}

for (app in project.apps) {
  name <- app[[1]]
  title <- app[[2]]

  nav.list[[name]] <- list(
    label = constant.func(title),
    url = constant.func.url(name),
    childrenIds = function(paramstr) { data.frame() }, # NULL
    parentId = function(paramstr = NULL) {
      id <- c("dashboard")
      data.frame(id)
    }
  )
  ## Note: We need to force evaluation of these functions here, since
  ## otherwise the name and title variables will not be "captured" by
  ## the closure; and will be overwritten in the next loop iteration.
  force(nav.list[[name]]$label())
  force(nav.list[[name]]$url())
}

## Configure the project dashboard
nav.list$dashboard <- list(
  ## (1) Configure he label displayed in the breadcrumb entry 
  label = function(paramstr) {  # paramstr must contain project id, e.g. "projectid=4&..."
    pel <- parseQueryString(paramstr)
    as.character(paste(projects.list$name[projects.list$id == as.numeric(pel$projectid)],
                       "Home"))
    },
  ## (2) configure URL for the breadcrumb entry
  url = function(paramstr) {
    paste("../dashboard/",paramstr, sep = "?")
    },
  ## (3) configure children displayed in dropdown
  childrenIds = function(paramstr) {
#                   id <- c("timeseries","contributors")
#                   params <- c(paramstr)
	data.frame(id = sapply(project.apps, "[", 1), params = c(paramstr))
	},
  ## (4) configure parent
  parentId = function(paramstr) {
    id <- c("quantarch")
    data.frame(id)
	})

################## END CONFIGURATION SECTION #####################


## Creates a data structure (list of lists) describing the breadcrumb
## 
## Requires the configuration function supplied by nav.list in parent environment
##
## Parameters
## ==========
## 	originid: 	id-string of the current app
## 	paramstr: 	URL parameter String (leading ? allowed, but not required) 
##				received by current app
## Returns
## =======
##	list of lists, where each sublist contains label, url and a list of children
##	each list of children contains label and url.
##	The last list entry is marked with active=TRUE (e.g. to allow special formatting)
##	TODO: also mark children to indicate which one is the active one
##
breadcrumbPanelData <- function (originId, paramstr = "" ) {

    ## check paramstr and remove leading ? if present
    if (is.null(paramstr)) paramstr <- ""
    if (nchar(paramstr) > 0 && substring(paramstr,1,1) == "?"){
      paramstr <- substring(paramstr,2)
    }
  
    bclist <- list() # the resultlist   
    count <- 0
  
    pid <- originId
  
    while(!is.null(pid)) {
      count <- count + 1
      n <- nav.list[[as.character(pid)]] # point to nav.list element of current app
      p <- n$parentId(paramstr) # get parent app id (p$id)
      cdf <- n$childrenIds(paramstr)
      child.list <- list()
      if (nrow(cdf) > 0) {
        for (i in 1:nrow(cdf)){
          ptr <- nav.list[[as.character(cdf$id[i])]]
          cparamstr <- as.character(cdf$params[i])
          child.list[[i]] <- list(label = ptr$label(cparamstr), url = ptr$url(cparamstr))
        }
      }
      bclist[[count]] <- list(label = n$label(paramstr), url = n$url(paramstr), active=(count == 1), children = child.list)
      ##print(count)
      ##print(bclist)
      pid <- p$id
    }
  
  ## reshuffle bclist in reverse order
  bclist <- bclist[length(bclist):1]

}

## Creates HTML code like this for dropdown breadcrumbs in HTML5 brandville design
##
## <div class="nav">
##   <nav>
##     <!-- NOTE FOR CMS INTEGRATION: Don't forget to place <b> tags around the last element -->
##   	<!-- We need a link for the last element here to make it focusable -->
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
##   breadcrumb: list structure created by funtion breadcrumbPanelData()
##
## Returns
## =======
##	HTML code for Siemens brandville Styles
##
breadcrumbBrandville <- function( breadcrumb ) {
  
  popdown.tags <- function(x) {
    tags$li(a(href=as.character(x$url),as.character(x$label)))
  }
  
  navtags <- tagList()
  
  for (bc.element in breadcrumb) {

    childlist <- tags$ul(tagList(lapply(bc.element$children, popdown.tags)))
    navtag <- tags$div(class = "nav quantarch", tag("nav", list( h1( a(href = as.character(bc.element$url),
                                            as.character(bc.element$label))), childlist )))
    navtags <- tagAppendChild(navtags, navtag)
  }
  
  navtags
  
}

## Creates HTML code like this for dropdown breadcrumbs in Bootstrap
##
## <div>
##   <ul class="breadcrumb">
##   <li><a href="#">Link1</a></li>
##   <li class="dropdown">
##   <a href="#">
##   Operations
##	 <b class="caret" class="dropdown-toggle" data-toggle="dropdown"></b>
##   </a>
##   <ul class="dropdown-menu">
##   <li><a href="#">Operations 1</a></li>
##   <li><a href="#">Operations 2</a></li>
##   </ul>
##	 <span class="divider">/</span>
##   </li>
##   <li> ... </li>
##   </ul>
##</div>
## 
## Parameters
## ==========
##   breadcrumb: list structure created by funtion breadcrumbPanelData()
##
## Returns
## =======
##	Bootstrap HTML code.
##
breadcrumbPanel <- function( breadcrumb ) {
  # create Bootstrap compatible XML
  
  
  divider.tag <- function( x=FALSE ) {
	if(x) { tags$span() } else { tags$span( class="divider","/")}}
	
	bc.link <- function( bcurl, bclabel, x=FALSE ) {
		if(x) {
			tags$b( as.character(bclabel) )
		} else {
			a( "data-target"="#", href = as.character(bcurl),
                        as.character(bclabel))
		}}
	
	bc.children <- function( bcchildren, x=FALSE ) {
		popdown.tags <- function(x) {
			tags$li(a(href=as.character(x$url),as.character(x$label)))
			}  
		if( length(bcchildren) == 0 ) {
			tags$div()
		} else {
			childtags <- tagList(lapply(bcchildren, popdown.tags))
			childlist <- tags$ul(class="dropdown-menu", childtags)
			tagList(tags$b( class="dropdown-toggle caret", "data-toggle" = "dropdown"  ), 
						childlist)
		}
		}
  
  navul <- tags$ul(class = "breadcrumb")
  
  for (bc.element in breadcrumb) {
    # childtags <- tagList(lapply(bc.element$children, popdown.tags))
    # childlist <- tags$ul(class="dropdown-menu", childtags)
	navtag <- tags$li(class = "dropdown", 
					bc.link( bc.element$url ,bc.element$label, bc.element$active), 
					bc.children( bc.element$children ),
					divider.tag( bc.element$active ))
    navul <- tagAppendChild(navul, navtag)
  }
  navul  
  #tagList(div(class = "span12", style = "padding: 10px 0px;", navul ))
} # end breadcrumbBootstrap

## TODO: Move testing to test_breadcrumb.r
## UNCOMMENT THIS FOR STANDALONE TESTING
# if (interactive()) {
  # cat("TESTS")
  # cat("=====\n")
  # bcd <- breadcrumbPanelData("contributors","projectid=4")
  # cat("R data structure")
  # print(bcd)
  # cat("\nJSON data structure")
  # cat(toJSON(bcd, pretty = TRUE))
  # cat("\nBootstrap Html code")
  # cat(as.character(breadcrumbPanel(bcd)))
  # cat("\nBrandville Html code")
  # cat(as.character(breadcrumbBrandville(bcd)))
# } 
