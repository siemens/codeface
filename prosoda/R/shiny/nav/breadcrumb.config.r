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
##  		- The only data needed currently is projects.list


## nav.list holds the methods used to generate the breadcrumb
nav.list <- list()

## configuration of nav.list
## adapt this for every app used

################## START CONFIGURATION SECTION #####################

##
## configure the base application (this is where projects are selected)
##

nav.list$projects <- list(
  ## (1) Configure he label displayed in the breadcrumb entry
  label = function(paramstr = NULL) {
    "Quantarch projects"
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

##
## Configure contributors app
##

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

# constant.func <- function(value) {
#   return(function(paramstr = NULL) { value })
# }
#
# constant.func.url <- function(name) {
#   return(function(paramstr = NULL) { paste("../", name, "/?", paramstr, sep='') })
# }

constant.func <- function(name) {
  paste("function(paramstr = NULL) {\"",as.character(name),"\"}",sep="")
}
constant.func.url <- function(name) {
  paste("function(paramstr = NULL) { paste(\"../", as.character(name), "?\", paramstr, sep='') }", sep="")
}

for (app in project.apps) {
  name <- app[[1]]
  title <- app[[2]]

  nav.list[[name]] <- list(
    label = eval(parse(text=constant.func(title))),
    url = eval(parse(text=constant.func.url(name))),
    childrenIds = function(paramstr) { data.frame() }, # NULL
    parentId = function(paramstr = NULL) {
      id <- c("dashboard")
      data.frame(id)
    }
  )

  ## Note: We need to force evaluation of these functions here, since
  ## otherwise the name and title variables will not be "captured" by
  ## the closure; and will be overwritten in the next loop iteration.
  #force(nav.list[[name]]$label())
  #force(nav.list[[name]]$url())
}

##
## Configure the project dashboard
##

nav.list$dashboard <- list(
  ## (1) Configure he label displayed in the breadcrumb entry
  label = function(paramstr) {  # paramstr must contain project id, e.g. "projectid=4&..."
    pel <- parseQueryString(paramstr)
    as.character(paste(projects.list$name[projects.list$id == as.numeric(pel$projectid)],
                       "Home"))
  },
  ## (2) configure URL for the breadcrumb entry
  url = function(paramstr) {
    paste("../dashboard-test/",paramstr, sep = "?")
  },
  ## (3) configure children displayed in dropdown
  childrenIds = function(paramstr) {
       id <- c("dashboard2")
       topics <- c("Information","Collaboration","Communication","Complexity")
       params <- as.character(paste(paramstr,"&topic=",topics,sep=""))
       data.frame(id, params)
  },
  ## (4) configure parent
  parentId = function(paramstr) {
    id <- c("projects")
    data.frame(id)
  })

##
## Configure the sencond level project dashboard
##

nav.list$dashboard2 <- list(
  ## (1) Configure he label displayed in the breadcrumb entry 
  label = function(paramstr) {  # paramstr must contain project id, e.g. "projectid=4&..."
    pel <- parseQueryString(paramstr)
    as.character(pel$topic)
  },
  ## (2) configure URL for the breadcrumb entry
  url = function(paramstr) {
    paste("../dashboard2/",paramstr, sep = "?")
  },
  ## (3) configure children displayed in dropdown
  childrenIds = function(paramstr) {
    #                   id <- c("timeseries","contributors")
    #                   params <- c(paramstr)
    #data.frame(id = sapply(project.apps, "[", 1), params = c(paramstr))
    data.frame(id = sapply(project.apps, FUN = function(x) {x[1]}), params = c(paramstr))
  },
  ## (4) configure parent
  parentId = function(paramstr) {
    id <- c("dashboard")
    data.frame(id)
  })
################## END CONFIGURATION SECTION #####################

test.nav.list <- function(paramstr) {
  z <- list()
  i <- 1
  for (x in nav.list) {
    z[[i]] <- list(label=x$label(paramstr),
    url=x$url(paramstr),
    childrenIds=x$childrenIds(paramstr),
    parentId=x$parentId(paramstr) )
    i <- i + 1
  }
  z
}
