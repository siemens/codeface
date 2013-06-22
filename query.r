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

## Helper functions to query data from the database. All routines
## in this file should only use a-priori knowledge that is available
## in the configuration object, not any other state.

suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(stringr))

## Obtain the series.merged object constructed in do.ts.analysis
query.series.merged <- function(conf, subset=NULL) {
  res <- lapply(c("Averaged (small window)", "Averaged (large window)",
                  "Cumulative"),
                function(type) {
                  plot.id <- get.plot.id(conf, str_c("Progress TS [", type, "]"))

                  dat <- query.timeseries(conf$con, plot.id, subset)
                  dat$type <- type

                  return(dat)
                })

  return(do.call(rbind, res))
}

## Obtain the data for the timeseries identified by a specific plot.id
query.timeseries <- function(con, plot.id, subset=NULL) {
  query <- str_c("SELECT time, value, value_scaled ",
                 "FROM timeseries where plotId=",
                 plot.id)
  if (!is.null(subset)) {
    ## TODO: Handle the case of subset selection
    ## by modifying query appropriately
  }

  dat <- dbGetQuery(con, query)
  dat$time <- ymd_hms(dat$time, quiet=T)
  colnames(dat)[3] <- "value.scaled"

  return(dat)
}

query.project.name <- function(con, pid) {
  dat <- dbGetQuery(con, str_c("SELECT name FROM project WHERE id=", sq(pid)))

  return(dat$name)
}

query.projects <- function(con) {
  dat <- dbGetQuery(con, str_c("SELECT id, name FROM project"))

  return(dat)
}

## Obtain all (db-internal) release range identifier for a project
query.range.ids.con <- function(con, pid) {
  dat <- dbGetQuery(con, str_c("SELECT id FROM release_range where projectID=",
                                    pid))

  return(dat$id)
}


## Simple frontend when the conf object is available
query.range.ids <- function(conf) {
  return(query.range.ids.con(conf$con, conf$pid))
}


## Obtain the per-release-range statistics
get.range.stats <- function(con, range.id) {
  dat <- dbGetQuery(con, str_c("SELECT ID, Name, added, deleted, total, ",
                               "numcommits FROM author_commit_stats_view ",
                               "WHERE releaseRangeId=", range.id))
  dat$Name <- as.character(dat$Name)
  Encoding(dat$Name) <- "UTF-8"

  return(dat)
}

get.commits.by.ranges <- function(conf, subset=NULL, FUN=NULL) {
  ts <- vector("list", length(conf$revisions)-1)
  tstamps <- conf$tstamps.release

  for (i in 1:(dim(tstamps)[1]-1)) {
    range.id <- get.range.id(conf, tstamps$tag[i], tstamps$tag[i+1])
    dat <- dbGetQuery(conf$con, str_c("SELECT * FROM commit where projectId=",
                                      conf$pid, " AND releaseRangeId=", range.id))

    if (!is.null(FUN)) {
      dat <- FUN(dat, subset)
    }

    if (dim(dat)[1] == 0) {
      cat("Skipping empty cycle", tstamps$tag[i], "..", tstamps$tag[i+1])
      next
    }

    ts[[i]] <- cbind(data.frame(revision=tstamps$tag[[i+1]],
                                date=tstamps$date[[i+1]]), dat)
  }

  return(ts)
}

## Get the IDs of all clusters for the given release range
query.cluster.ids.con <- function(con, pid, range.id, cluster.method) {
  dat <- dbGetQuery(con, str_c("SELECT id FROM cluster WHERE ",
                               "projectId=", pid, " AND releaseRangeId=",
                               range.id, " AND clusterMethod=",
                               sq(cluster.method)))

  return(dat$id)
}

query.cluster.ids <- function(conf, range.id, cluster.method) {
  return(query.cluster.ids.con(conf$con, conf$pid, range.id, cluster.method))
}

## Get all members (in terms of person id) of a cluster
## technique can be 0 for normal pagerank and 1 for pagerank based
## on the transposed adjacency matrix
query.cluster.members <- function(con, cluster.id, prank=F, technique=0) {
  query <- "SELECT personId"
  if (prank) {
    query <- str_c(query, ", rankValue")
  }
  query <- str_c(query, " FROM cluster_user_pagerank_view WHERE clusterId=",
                 cluster.id, " AND technique=", technique)

  dat <- dbGetQuery(con, query)

  if (prank)
    return(dat)
  else
    return(dat$personId)
}

## Query an edgelist for a given cluster
query.cluster.edges <- function(con, cluster.id) {
  dat <- dbGetQuery(con, str_c("SELECT * FROM ",
                               "edgelist WHERE clusterId=", cluster.id))

  if (dim(dat)[1] > 0) {
    return(dat[,c("fromId", "toId", "weight")])
  } else {
    return(NULL)
  }
}

### General SQL helper functions
## Test if a table is empty (returns false) or not (returns true)
table.has.entries <- function(conf, table) {
  dat <- dbGetQuery(conf$con, str_c("SELECT * from ", table))

  return (dim(dat)[1] > 0)
}
