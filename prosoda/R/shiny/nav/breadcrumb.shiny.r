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
## source breadcrumb config
##

## this depends on having created projects.list before sourcing this script
## can be done like this:
##
# s <- suppressPackageStartupMessages
# s(library(shiny))
# s(library(logging))
# rm(s)
# source("../../config.r", chdir=TRUE)
# source("../../query.r", chdir=TRUE)
# conf <- config.from.args(require_project=FALSE)
# projects.list <- query.projects(conf$con)

source("breadcrumb.config.r")  # source in this environment

## Creates a data structure (list of lists) describing the breadcrumb
##
## Requires the configuration function supplied by nav.list in parent environment
##
## Parameters
## ==========
##   originid: 	id-string of the current app
## 	paramstr: 	URL parameter String (leading ? allowed, but not required)
##				received by current app
## Returns
## =======
##	list of lists, where each sublist contains label, url and a list of children
##	each list of children contains label and url.
##	The last list entry is marked with active=TRUE (e.g. to allow special formatting)
##	TODO: also mark children to indicate which one is the active one
##
breadcrumbPanelData <- function (originId, paramstr = "") {

  if (!exists("nav.list", 1))
    stop("ERROR: nav.list not found." ) #else print(nav.list)

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
    #print(n)
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

#
# utility functions
#

## get a list with unchecked url parameter labels and values
##
## Parameters
## ==========
##  urlsearch:  session variable, for shiny server function
##              assign "session$clientData$url_search"
##
## Returns
## =======
##  list of parameters, each of which has a vector of values
##
urlparameter.as.list <- function(urlsearch) {
  # TODO: parseQueryString does not check for multiple occurences etc.
  parseQueryString(urlsearch)
}

urlparameter.list.checked <- function(urlparameter.list) {
  args <- urlparameter.list
  if (!exists("projects.list",1) || nrow(projects.list) == 0)
    stop("ERROR: projects.list not found or empty." )
  if (is.null(args$projectid) ||
        args$projectid == "" ||
        is.na(as.integer(args$projectid)) ||
        (args$projectid != as.integer(args$projectid) ) ||
        !(args$projectid %in% projects.list$id)) {
    args$projectid <- as.character(projects.list$id[1])
  }
  args
}

urlparameter.as.string <- function( urlparameter.list, PREFIX = "?" ) {
  keys <- vapply(names(urlparameter.list), function(x) URLencode(x), FUN.VALUE = character(1))
  values <- vapply(as.character(urlparameter.list), function(x) URLencode(x), FUN.VALUE = character(1))
  paste(PREFIX,paste(keys,values,sep="=",collapse="&"),sep="")
}

urlparameter.checked <- function(urlsearch) {
  prefix <- substring(urlsearch,1,1)
  prefix <- if (prefix == "?") "?" else ""
  x <- urlparameter.list.checked(urlparameter.as.list(urlsearch))
  urlparameter.as.string(x, PREFIX=prefix)
}
## get a checked projectid
##
## Parameters
## ==========
##  urlsearch:  session variable, for shiny server function
##              assign "session$clientData$url_search"
##
## Returns
## =======
##  projectid as a character value containing an integer
##
projectIdChecked <- function(urlsearch) {
  args <- parseQueryString(urlsearch)
  if (!exists("project.list") || nrow(projects.list) == 0)
    stop("ERROR: projects.list not found or empty." )
  if (is.null(args$projectid) ||
        args$projectid == "" ||
        is.na(as.integer(args$projectid)) ||
        (args$projectid != as.integer(args$projectid) ) ||
        !(args$projectid %in% projects.list$id)) {
    args$projectid <- as.character(projects.list$id[1])
  }
  as.character(args$projectid)
}

##
## Html generators to be used in ui.r
##

breadcrumbOutput <- function( outputId, ... ) {
  div(class = "span12", style = "padding: 10px 0px;",
      div(id = outputId, class = "shiny-html-output"), ... )
}

pageWithBreadcrumbAndHeader <- function (breadcrumbPanel, headerPanel, mainPanel) {
  bootstrapPage(div(class = "container", div(class = "row",
                                             breadcrumbPanel), div(class = "row", headerPanel,
                                                                   mainPanel)))
}

##
## Utility function to be used in server.r
##

renderBreadcrumbPanel <- function(originid = "", paramstr = "") {
  if ( originid %in% names(nav.list) ) {

    data <- breadcrumbPanelData( originid, paramstr )
    breadcrumbPanel( data )

    } else {
      paste("ERROR (renderBreadcrumbPanel): originid=", originid, "is not a configured app id.")
    }
}
