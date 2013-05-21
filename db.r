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

## Generic database helper functions. More specific queries should
## be directly included into the code

## TODO: Add S3 database helper class similar to dbManager.py

suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(stringr))

sq <- function(string) {
  return(str_c("'", string, "'"))
}

get.project.id <- function(con, name) {
  res <- dbGetQuery(con, str_c("SELECT id FROM project WHERE name=", sq(name)))
  
  return(res$id)
}

## Determine the ID of a given plot for a given project. Since
## plots are not created in parallel, we need no locking
get.plot.id <- function(conf, plot.name) {
  res <- dbGetQuery(conf$con, str_c("SELECT id from plots WHERE name=",
                                    sq(plot.name), "AND projectId=", conf$pid))

  if (length(res) == 0) {
    dbGetQuery(conf$con, str_c("INSERT INTO plots (name, projectId) VALUES (",
                               sq(plot.name), ", ", conf$pid, ")"))
    res <- dbGetQuery(conf$con, str_c("SELECT id from plots WHERE name=",
                                      sq(plot.name), "AND projectId=", conf$pid))
  }

  if (length(res) != 1) {
    stop("Internal error: Plot ", plot.name, " appears multiple times in DB",
         "for project ", conf$project)
  }

  return(res$id)
}

## Determine the ID of a tag, given its textual form
get.revision.id <- function(conf, tag) {
  res <- dbGetQuery(conf$con,
                    str_c("SELECT id FROM release_timeline WHERE projectId=",
                          conf$pid, " AND tag=", sq(tag), " AND type='release'"))

  if (length(res) > 1) {
    stop("Internal error: Revision if for tag ", tag, " (project ", conf$project,
         ") appears multiple times in DB!")
  }

  return(res$id)
}

# Get all release dates (without release candidates) for a given project
get.release.dates <- function(conf) {
  res <- dbGetQuery(conf$con,
                    str_c("SELECT * FROM release_timeline WHERE projectId=",
                          conf$pid, sep=""))
  res$type <- as.factor(res$type)
  res$date <- ymd_hms(res$date, quiet=T)
  res <- res[res$type=="release",]

  return(res)
}


## Establish the connection and store the relevant configuration
## parameters in the project specific configuration structure
init.db <- function(conf, global.conf) {
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv, host=global.conf$dbhost, user=global.conf$dbuser,
                   password=global.conf$dbpwd, dbname=global.conf$dbname)
  conf$pid <- get.project.id(con, conf$project)
  conf$con <- con

  if (is.null(conf$pid)) {
    stop("Internal error: No ID assigned to project ", conf$project, "\n",
         "(Did you not run the VCS analysis before the ml analysis?)\n")
  }

  return(conf)
}
