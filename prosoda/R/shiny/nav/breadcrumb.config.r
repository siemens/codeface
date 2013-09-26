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

source("../symbols.r")
source("../widgets.r", chdir=TRUE)

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
  label = function(paramstr = "") {
    "Quantarch projects"
  },
  ## (2) configure URL for the breadcrumb entry
  url = function(paramstr = "") {
    "../dashboard-test/"
  }, # params kann man z.B. zum highliten verwende

  ## (3) configure children displayed in dropdown
  childrenIds = function(paramstr = "") {
    id <- c("dashboard") # in this example we have 1 child
    params <- paste("projectid",projects.list$id,sep = "=") # needs project id
    data.frame(id, params)
  },

  ## (4) configure parent
  parentId = function(paramstr = "") {
    NULL # no parent
  })

##
## Configure contributors app
##
detail.apps <- function(topic) {
  detailpage.titles <- sapply(widget.list, function(w.cls) {w.cls$detailpage$title})
  names(detailpage.titles) <- sapply(widget.list, function(w.cls) {w.cls$detailpage$name})
  ## Remove not-fitting detail pages
  detailpage.use <- sapply(widget.list, function(w.cls) {is.null(w.cls$topics) || topic %in% w.cls$topics})
  detailpage.titles <- detailpage.titles[detailpage.use]
  ## Remove "unset" detailpages
  detailpage.titles <- detailpage.titles[!sapply(detailpage.titles, is.null)]
  ## Make the list unique
  detailpage.titles <- detailpage.titles[unique(names(detailpage.titles))]
  ## Create link
  apps <- lapply(1:length(detailpage.titles), function(i) {
    if (is.null(detailpage.titles[[i]])) {stop(paste("Title missing for detail page", names(detailpage.titles)[[i]]))}
    return(c(paste("details?topic=", topic, "&widget=", names(detailpage.titles)[[i]], sep=""), detailpage.titles[[i]]))
  })
  names(apps) <- NULL
  return(apps)
}

topic.ids <- c("basics", "communication", "construction", "complexity", "collaboration")

project.apps.basics <- detail.apps("basics")

project.apps.communication <- c(list(
  c("timeseries", "Mailing list activity")
), detail.apps("communication"))

project.apps.construction <- detail.apps("construction")

project.apps.complexity <- c(list(
  c("plots", "Time series of complexity metrics")
), detail.apps("complexity"))

project.apps.collaboration <- detail.apps("collaboration")

merge.query.strings <- function(p, q) {
  p <- parseQueryString(p)
  q <- parseQueryString(q)
  p[names(q)] <- q
  p <- p[unique(names(p))]
  do.call(paste, c(lapply(1:length(p), function(i) {
    paste(names(p)[[i]], p[[i]], sep="=")
  }), list(sep="&")))
}

constant.func <- function(value) {
  f <- function(paramstr="") { value }
  force(f())
  return(f)
}

constant.func.url <- function(name) {
  if (grepl("\\?", name)) {
    url.split <- strsplit(name, split="?", fixed=TRUE)[[1]]
    q <- url.split[[2]]
    f <- function(paramstr="") {
      new.paramstr <- merge.query.strings(paramstr, q)
      paste("../", url.split[[1]], "?", new.paramstr, sep='')
    }
  } else {
    f <- function(paramstr="") {
      paste("../", name, "?", paramstr, sep='')
    }
  }
  return(f)
}

for (app in c(project.apps.basics, project.apps.communication, project.apps.construction, project.apps.complexity, project.apps.collaboration)) {
  name <- app[[1]]
  title <- app[[2]]
  nav.list[[name]] <- list(
    label = constant.func(title),
    url = constant.func.url(name),
    childrenIds = function(paramstr) { data.frame() }, # NULL
    parentId = function(paramstr="") {
      id <- c("dashboard2")
      data.frame(id=id, paramstr=paramstr)
    }
  )
  ## Note: We need to force evaluation of these functions here, since
  ## otherwise the name and title variables will not be "captured" by
  ## the closure; and will be overwritten in the next loop iteration.
  force(nav.list[[name]]$label())
  force(nav.list[[name]]$url())
}

nav.list$details <- list(
  label = constant.func("Details"),
  url = constant.func.url("details"),
  childrenIds = function(paramstr) { data.frame() }, # NULL
  parentId = function(paramstr="") {
    id <- c("dashboard2")
    data.frame(id=id, paramstr=paramstr)
  }
)
force(nav.list$details$label())
force(nav.list$details$url())

##
## Configure the project dashboard
##

nav.list$dashboard <- list(
  ## (1) Configure he label displayed in the breadcrumb entry
  label = function(paramstr) {  # paramstr must contain project id, e.g. "projectid=4&..."
    pel <- parseQueryString(paramstr)
    pname <- projects.list$name[projects.list$id == as.numeric(pel$projectid)]
    return(pname)
  },
  ## (2) configure URL for the breadcrumb entry
  url = function(paramstr) {
    paste("../dashboard-test/", paramstr, sep = "?")
  },
  ## (3) configure children displayed in dropdown
  childrenIds = function(paramstr) {
    id <- c("dashboard2")
    params <- sapply(topic.ids, function(id) {
                     merge.query.strings(paramstr, paste("topic", id, sep="="))
              })
    data.frame(id, params)
  },
  ## (4) configure parent
  parentId = function(paramstr) {
    id <- c("projects")
    data.frame(id=id, paramstr="")
  })


##
## Configure the sencond level project dashboard
##

nav.list$dashboard2 <- list(
  ## (1) Configure he label displayed in the breadcrumb entry 
  label = function(paramstr) {  # paramstr must contain project id, e.g. "projectid=4&..."
    pel <- parseQueryString(paramstr)
    if (is.null(pel$topic) || !pel$topic %in% topic.ids) {
      pel$topic <- topic.ids[[1]]
    }
    symbol <- get(paste("symbol", pel$topic, sep="."))
    s <- pel$topic
    capname <- paste(toupper(substring(s, 1,1)), substring(s,2), sep="")
    tagList(symbol, capname)
  },
  ## (2) configure URL for the breadcrumb entry
  url = function(paramstr) {
    paste("../dashboard-test/",paramstr, sep = "?")
  },
  ## (3) configure children displayed in dropdown
  childrenIds = function(paramstr) {
    #                   id <- c("timeseries","contributors")
    #                   params <- c(paramstr)
    #data.frame(id = sapply(project.apps, "[", 1), params = c(paramstr))
    pel <- parseQueryString(paramstr)
    if (is.null(pel$topic) || !pel$topic %in% topic.ids) {
      pel$topic <- topic.ids[[1]]
    }
    apps <- get(paste("project.apps", pel$topic, sep="."))
    if (length(apps) == 0) {
      return(data.frame())
    }
    data.frame(id = sapply(apps, FUN = function(x) {x[1]}), params = c(paramstr))
  },
  ## (4) configure parent
  parentId = function(paramstr) {
    id <- c("dashboard")
    pel <- parseQueryString(paramstr)
    paramstr <- paste("projectid=", pel$projectid, sep="")
    data.frame(id=id, paramstr=paramstr)
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
