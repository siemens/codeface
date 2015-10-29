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

## Helper functions to query data from the database. All routines
## in this file should only use a-priori knowledge that is available
## in the configuration object, not any other state.

suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(logging))

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
                 "FROM timeseries WHERE plotId=",
                 plot.id)
  if (!is.null(subset)) {
    ## TODO: Handle the case of subset selection
    ## by modifying query appropriately
  }

  dat <- dbGetQuery(con, query)
  dat$time <- ymd_hms(dat$time, quiet=TRUE)
  colnames(dat)[3] <- "value.scaled"

  return(dat)
}

query.sloccount.ts <- function(con, plot.id) {
  query <- str_c("SELECT time, person_months, total_cost, ",
                 "schedule_months, avg_devel FROM sloccount_ts ",
                 "WHERE plotId=", plot.id)

  dat <- dbGetQuery(con, query)
  colnames(dat) <-  c("time", "person.months", "total.cost", "schedule.months",
                      "avg.devel")
  dat$time <- ymd_hms(dat$time, quiet=TRUE)

  return(dat)
}

query.understand.ts <- function(con, plot.id, measure, kind="File",
                                which.val="q3") {
  query <- str_c("SELECT time, value FROM understand_raw WHERE ",
                 "plotId=", plot.id, " AND variable='", measure, "' ",
                 "AND name='", which.val, "' AND kind='", kind, "'")

  dat <- dbGetQuery(con, query)
  colnames(dat) <-  c("time", "value")
  dat$time <- ymd_hms(dat$time, quiet=TRUE)

  return(dat)
}

query.project.name <- function(con, pid) {
  dat <- dbGetQuery(con, str_c("SELECT name FROM project WHERE id=", sq(pid)))

  return(dat$name)
}

query.projects <- function(con, analysis.method=NULL) {
  if(is.null(analysis.method)){
    query.str <-  str_c("SELECT id, name FROM project")
  }
  else {
    query.str <- str_c("SELECT id, name FROM project ",
                       "WHERE analysisMethod= ", sq(analysis.method))
  }
  dat <- dbGetQuery(con, query.str)

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


## Obtain all release cycles of a project (date boundaries, internal ID,
## cycle name)
get.cycles.con <- function(con, pid, boundaries=FALSE, allow.empty.ranges=FALSE) {
  res <- dbGetQuery(con, str_c("SELECT * ",
                               "FROM revisions_view ",
                               "WHERE projectId=", pid))
  if (nrow(res) == 0 & !allow.empty.ranges) {
    stop(paste("No release range found for projectId=", pid))
  }

  colnames(res) <- gsub("_", ".", colnames(res))
  colnames(res)[colnames(res)=="releaseRangeID"] <- "range.id"

  if(boundaries) {
    column.selection <- c("date.start", "date.end", "date.rc.start", "tag",
                          "cycle")
  }
  else{
    column.selection <- c("range.id", "date.start", "date.end", "cycle")
  }

  res <- res[, column.selection]
  res$date.start <- ymd_hms(res$date.start, quiet=TRUE)
  res$date.end <- ymd_hms(res$date.end, quiet=TRUE)

  return(res)
}

get.cycles <- function(conf, ...) {
  return(get.cycles.con(conf$con, conf$pid, ...))
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

## Obtain commit information for all cycles of a project
get.commits.by.ranges <- function(conf, subset=NULL, FUN=NULL) {
  ts <- vector("list", length(conf$revisions)-1)
  tstamps <- conf$tstamps.release

  for (i in 1:(dim(tstamps)[1]-1)) {
    range.id <- get.range.id(conf, tstamps$tag[i], tstamps$tag[i+1])
    dat <- get.commits.by.range(conf, range.id, subset, FUN)

    if (is.null(dat)) {
      logdevinfo(paste("Skipping empty cycle ", tstamps$tag[i], "..", tstamps$tag[i+1], sep=""),
                 logger="query")
      next
    }

    ts[[i]] <- cbind(data.frame(revision=tstamps$tag[[i+1]],
                                date=tstamps$date[[i+1]]), dat)
  }

  return(ts)
}

get.commits.by.date.con <- function(con, pid, start.date, end.date,
                                    commit.date=TRUE, commit.count=FALSE) {
  if (commit.date==TRUE) {
    date.type <- "commitDate"
  } else {
    date.type <- "authorDate"
  }

  if (commit.count==TRUE) {
    query <- "SELECT author, COUNT(*) as freq"
    group.by <- " GROUP BY author"
  } else {
    query <- "SELECT *"
    group.by <- NULL
  }

  query <- str_c(query,
                 " FROM commit",
                 " WHERE projectId=", pid,
                 " AND ", date.type, ">=", sq(start.date),
                 " AND ", date.type, "<", sq(end.date),
                 group.by)

  dat <- dbGetQuery(con, query)

  return(dat)
}

## Obtain commit information for a specific cycle of a project
get.commits.by.range.con <- function(con, pid, range.id, subset=NULL, FUN=NULL) {
  dat <- dbGetQuery(con, str_c("SELECT * FROM commit where projectId=",
                               pid, " AND releaseRangeId=", range.id))

  if (!is.null(FUN)) {
    dat <- FUN(dat, subset)
  }

  if (dim(dat)[1] == 0) {
    return(NULL)
  }

  return(dat)
}

get.commits.by.range <- function(conf, range.id, subset=NULL, FUN=NULL) {
  return(get.commits.by.range.con(conf$con, conf$pid, range.id, subset, FUN))
}

## Simplified version of the above that only gets commit hashes, but
## orders them by commit date
get.commit.hashes.by.range.con <- function(con, pid, range.id) {
  dat <- dbGetQuery(con, str_c("SELECT commitHash, commitDate FROM commit ",
                               "WHERE projectId=",
                               pid, " AND releaseRangeId=", range.id,
                               " ORDER BY commitDate"))
  dat$commitDate <- ymd_hms(dat$commitDate, quiet=TRUE)

  return(dat)
}

get.commit.hashes.by.range <- function(conf, range.id) {
  return(get.commit.hashes.by.range.con(conf$con, conf$pid, range.id))
}

## Get scaled commit infos for all cycles of pid
## This is similar to get.commits.by.ranges, but does not require
## a full conf object
get.cmt.info.list <- function(con, pid, subset, scale=TRUE) {
  range.ids.list <- query.range.ids.con(con, pid)
  cycles <- get.cycles.con(con, pid)

  res <- lapply(range.ids.list, function(range.id) {
    cmt.info <- get.commits.by.range.con(con, pid, range.id,
                                         c(subset, "inRC"),
                                         normalise.commit.dat)
    if (scale) {
      cmt.info.scaled <- scale(cmt.info[, subset])
    } else {
      cmt.info.scaled <- cmt.info[, subset]
    }
    cmt.info.scaled <- data.frame(cmt.info.scaled,
                                  inRC=mapvalues(cmt.info$inRC, c(0,1),
                                    c("Regular", "RC"), warn_missing=FALSE),
                                  cycle=cycles$cycle[cycles$range.id==range.id])
    return(cmt.info.scaled)
  })

  return(res)
}

## Get the IDs of all clusters for the given release range
query.cluster.ids.con <- function(con, pid, range.id, cluster.method) {
  dat <- dbGetQuery(con, str_c("SELECT id FROM cluster WHERE ",
                               "projectId=", pid, " AND releaseRangeId=",
                               range.id, " AND clusterMethod=",
                               sq(cluster.method), " AND clusterNumber >= 0"))

  return(dat$id)
}

query.cluster.ids <- function(conf, range.id, cluster.method) {
  return(query.cluster.ids.con(conf$con, conf$pid, range.id, cluster.method))
}

## Cluster -1 is not a proper cluster, but contains the global
## collaboration structure
query.global.collab.con <- function(con, pid, range.id, cluster.method="Spin Glass Community") {
  dat <- dbGetQuery(con, str_c("SELECT id FROM cluster WHERE ",
                               "projectId=", pid, " AND releaseRangeId=",
                               range.id, " AND clusterMethod=",
                               sq(cluster.method), " AND clusterNumber=-1"))

  return(dat$id)
}

query.global.collab <- function(conf, range.id, cluster.method) {
 return(query.global.collab.con(conf$con, conf$pid, range.id, cluster.method))
}

## Get all members (in terms of person id) of a cluster
## technique can be 0 for normal pagerank and 1 for pagerank based
## on the transposed adjacency matrix
query.cluster.members <- function(con, cluster.id, prank=FALSE, technique=0) {
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

## Query an edgelist for a given cluster. Note that for single-contributor
## clusters, there are no edges, so we need to take this case into account.
query.cluster.edges <- function(con, cluster.id) {
  dat <- dbGetQuery(con, str_c("SELECT * FROM ",
                               "edgelist WHERE clusterId=", cluster.id))

  if (dim(dat)[1] > 0) {
    return(dat[,c("fromId", "toId", "weight")])
  } else {
    return(NULL)
  }
}

## Query the per-cluster, per-person statistics for a given cluster id
query.cluster.person.stats <- function(con, cluster.id, person.id=NULL,
                                       technique=0) {
  query <- str_c("SELECT clusterId, personId, rankValue, added, deleted, total, ",
                 "numcommits FROM per_person_cluster_statistics_view WHERE ",
                 "clusterId=", cluster.id, " AND technique=", technique)
  if (!is.null(person.id)) {
    query <- str_c(query, " AND personId=", person.id)
  }

  dat <- dbGetQuery(con, query)

  colnames(dat) <- c("cluster.id", "person.id", "rankValue", "added", "deleted",
                     "total", "numcommits")
  dat$cluster.id <- as.factor(dat$cluster.id)

  return(dat)
}

## Query the per-cluster statistics for a given cluster id.
## In contrast to query.cluster.person.stats, the data are cluster-global
## and not resolved by person
query.cluster.stats <- function(con, cluster.id, technique=0) {
  dat <- dbGetQuery(con, str_c("SELECT num_members, added, deleted, total,
                                       numcommits, prank_avg FROM ",
                               "per_cluster_statistics_view WHERE ",
                               "clusterId=", cluster.id, " AND technique=",
                               technique))

  colnames(dat) <- c("num.members", "added", "deleted", "total", "numcommits",
                     "prank.avg")
  return(dat)
}

## Map a systematic person ID to a proper name
## We consider the caser person.id because this can eliminate the need for
## special casing in the caller
query.person.name <- function(con, person.id) {
  if (is.na(person.id)) {
    return(NA)
  }

  dat <- dbGetQuery(con, str_c("SELECT name FROM person WHERE id=", person.id))

  if (dim(dat)[1] > 0) {
    return(dat$name)
  }

  return(NA)
}

## Obtain the id of the first mailing list associated with a
## project. This is required since currently, the type of a mailing
## list (devel/user) is no stored in the database. Consequently,
## we cannot distinguish the type without access to the configuration
## file, as is the case for the web frontend.
## The function should go away once this inconsistency is resolved.
query.ml.id.simple.con <- function(con, pid) {
    query <- str_c("SELECT id, name FROM mailing_list WHERE projectId=",
                   pid)
    res <- dbGetQuery(con, query)

    if (!is.null(res)) {
        return(res[[1]])
    }
    return(NULL)
}

## Obtain a mailing list id. The explicit name of the mailing
## list is required
query.ml.id.con <- function(con, pid, ml) {
    res <- dbGetQuery(con, str_c("SELECT id from mailing_list ",
                                 "WHERE projectId=", pid,
                                 " AND name=", sq(ml)))

    return(res)
}

## Obtain the mailing list using a full conf object. It suffices
## to specify the _type_ of the mailing list (i.e., "dev" or "user")
query.ml.id <- function(conf, ml.type) {
    if (!(ml.type %in% c("dev", "user"))) {
        stop("Internal Error: query.ml.id used with invalid type")
    }

    ## Select the smallest id for which the list type matches
    ## the desired type (NULL if no match is found)
    idx <- do.call(c, lapply(1:length(conf$mailinglists), function(i) {
               if (conf$mailinglists[[i]]$type==ml.type) {
                   return(i)
               }

               return(NULL)
           }))[[1]]

    if (is.null(idx)) {
        return(NULL)
    }

    return(query.ml.id.con(conf$con, conf$pid,
                           conf$mailinglists[[idx]]$name)$id)
}



## Query a two-mode edgelist (as used by the mailing list analysis in
## author-interest graphs) from the database
## type specifies the base data for the object, "subject" or "content"
## ml gives the name of the mailing list
query.twomode.edgelist <- function(con, type, ml, range.id) {
  dat <- dbGetQuery(con, str_c("SELECT fromVert, toVert, weight FROM ",
                               "twomode_edgelist WHERE releaseRangeId=", range.id,
                               " AND source=", sq(type), " AND ml=", sq(ml)))

  return(dat)
}

## Same as above for the vertex list
query.twomode.vertices <- function(con, type, ml, range.id) {
  dat <- dbGetQuery(con, str_c("SELECT name, degree, type FROM ",
                               "twomode_vertices WHERE releaseRangeId=", range.id,
                               " AND source=", sq(type), " AND ml=", sq(ml)))

  return(dat)
}

query.initiate.response <- function(con, ml.id, range.id, type=NULL) {
  if (!is.null(type) && !(type %in% c("subject", "content"))) {
    stop("type in query.intiate.response must be NULL or subject or content!")
  }

  if (!is.null(type)) {
    ## Convert human-readable type (subject, content) to in-DB encoding (0,1)
    type <- which(type == c("subject", "content")) - 1
  }

  query <- str_c("SELECT responses, initiations, responses_received, deg, source ",
                 "FROM initiate_response WHERE releaseRangeId=", range.id,
                 " AND mlId=", ml.id)
  if (!is.null(type)) {
    query <- str_c(query, " AND source=", type)
  }

  dat <- dbGetQuery(con, query)

  colnames(dat) <- c("responses", "initiations", "responses.received",
                     "deg", "source")
  dat$source <- mapvalues(dat$source, from=c(0,1), to=c("subject", "content"),
                          warn_missing=FALSE)
  dat$source <- as.factor(dat$source)

  return(dat)
}

query.thread.info <- function(con, ml, range.id) {
  dat <- dbGetQuery(con, str_c("SELECT subject, createdBy, mailThreadId, ",
                               "creationDate, numberOfAuthors, numberOfMessages ",
                               "FROM mail_thread WHERE releaseRangeId=", range.id,
                               " AND ml=", sq(ml)))
  if (!is.null(dat)) {
    colnames(dat) <- c("subject", "createdBy", "tid", "creationDate", "authors",
                       "messages")
    dat$tid <- as.factor(dat$tid)
  }

  return(dat)
}

query.pagerank <- function(con, prank.id, limit=20) {
  dat <- dbGetQuery(con, str_c("SELECT authorId, name, rankValue FROM ",
                               "pagerank_view WHERE pageRankID=", prank.id,
                               " ORDER BY rankValue DESC LIMIT ", limit))

  if (!is.null(dat)) {
    colnames(dat) <- c("person.id", "name", "prank")
  }

  return(dat)
}

query.top.contributors.commits <- function(con, range.id, limit=20) {
  dat <- dbGetQuery(con, str_c("SELECT Name, numcommits ",
                               "FROM author_commit_stats_view WHERE ",
                               "releaseRangeId=", range.id,
                               " ORDER BY numcommits DESC LIMIT ", limit))
  if (!is.null(dat)) {
    colnames(dat) <- c("name", "numcommits")
  }

  return(dat)
}

query.top.contributors.changes <- function(con, range.id, limit=20) {
  dat <- dbGetQuery(con, str_c("SELECT Name, added, deleted, total ",
                               "FROM author_commit_stats_view WHERE ",
                               "releaseRangeId=", range.id,
                               " ORDER BY total DESC LIMIT ", limit))
  if (!is.null(dat)) {
    colnames(dat) <- c("name", "added", "deleted", "total")
  }

  return(dat)
}

## Compute edgelist for mailing list communication
query.mail.edgelist <- function(con, pid, start.date, end.date) {
  query <- str_c("SELECT mail_from.author as `from`, mail_to.author as `to`,
                         COUNT(*) as `weight`",
                 "FROM mail mail_from, mail mail_to",
                 "WHERE mail_from.projectId=", pid,
                 "AND mail_from.projectId=mail_to.projectId",
                 "AND mail_from.threadId=mail_to.threadId",
                 "AND mail_from.mlId=mail_to.mlId",
                 "AND mail_from.author!=mail_to.author",
                 "AND mail_from.creationDate > mail_to.creationDate",
                 "AND mail_from.creationDate >=", sq(start.date),
                 "AND mail_from.creationDate <", sq(end.date),
                 "AND mail_to.creationDate >=", sq(start.date),
                 "AND mail_to.creationDate <", sq(end.date),
                 "GROUP BY mail_from.author, mail_to.author", sep=" ")
  dat <- dbGetQuery(con, query)

  return(dat)
}

## Distributions for commit statistics
query.contributions.stats.range <- function(con, range.id, include.id=FALSE) {
  if (include.id) {
    query <- "SELECT authorId, "
  } else {
    query <- "SELECT "
  }

  query <- str_c(query, "numcommits, total FROM author_commit_stats ",
                 "WHERE releaseRangeId=", range.id)
  dat <- dbGetQuery(con, query)

  return(dat)
}

query.contributions.stats.project <- function(con, pid) {
  range.ids.list <- query.range.ids.con(con, pid)

  dat <- lapply(range.ids.list, function(range.id) {
    res <- query.contributions.stats.range(con, range.id, include.id=TRUE)
    res <- cbind(range.id=range.id, res)

    return(res)
  })

  dat <- do.call(rbind, dat)

  if (!is.null(dat)) {
    colnames(dat) <- c("range.id", "author.id", "numcommits", "total")
  }

  dat$range.id <- as.factor(dat$range.id)
  dat$author.id <- as.factor(dat$author.id)
  return(dat)
}

## Obtain a mapping between local and in-DB mail IDs
query.mlid.map <- function(con, ml.id) {
  dat <- dbGetQuery(con, str_c("SELECT id, mailThreadId FROM ",
                               "mail_thread WHERE mlId=", ml.id))

  if (!is.null(dat)) {
    colnames(dat) <- c("db.id", "local.id")
  }

  return(dat)
}

## Query mailing list activity data -- return a list of all time stamps
## when a message was sent
query.ml.activity <- function(con, ml.id, range.id) {
  ## First, select all thread initiating messages
  threads <- dbGetQuery(conf$con, str_c("SELECT id, creationDate FROM ",
                                        "mail_thread WHERE mlId=", ml.id,
                                        " AND releaseRangeID=", range.id))

  ## Determine all replies for each thread, and store their dates
  if (dim(threads)[1] > 0) {
    threads$creationDate <- ymd_hms(threads$creationDate, quiet=TRUE)

    dat <- lapply(threads$id, function(thread.id) {
      res <- dbGetQuery(conf$con, str_c("SELECT mailDate FROM ",
                                        "thread_responses WHERE ",
                                        "mailThreadId=", thread.id))
      if (dim(res)[1] > 0) {
        return(ymd_hms(res$mailDate, quiet=TRUE))
      }

      return(NULL)
    })

    dat <- do.call(c, dat)

    return(c(threads$creationDate, dat))
  }

  return(NULL)
}

## Generate a data frame with descriptive statistics (or factoids, as
## other portals like Ohloh call them). While we can observe intervals
## of interest at different granularity (per cycle, yearly, complete
## project duration), the base query is always identical.
gen.factoids.basequery <- function() {
  return(str_c("SELECT COUNT(DISTINCT(author)) AS numauthors, ",
               "COUNT(id) as numcommits, ",
               "SUM(addedLines+deletedLines) as changedLines, ",
               "SUM(ChangedFiles) AS changedFiles FROM commit"))
}

query.factoids.cycle.con <- function(con, range.id) {
  query <- gen.factoids.basequery()
  query <- str_c(query, " WHERE releaseRangeId=", range.id)
  res <- dbGetQuery(con, query)

  return(res)
}

## Same factoids as before, but this time resolved by year
query.factoids.yearly.con <- function(con, year) {
  query <- gen.factoids.basequery()
  query <- str_c(query, " WHERE commitDate >= '", year, "-01-01' AND ",
                 " commitDate <= '", year, "-31-12'")
  res <- dbGetQuery(con, query)

  return(res)
}

## When the conf object is available, we can perform a sanity check
## to ensure that the desired year is in the analysis range
query.factoids.yearly <- function(conf, year) {
  if (year < year(conf$boundaries$date.start[1]) |
      year > year(conf$boundaries$date.start[length(conf$boundaries$date.start)])) {
    return(NA)
  }

  return(query.factoids.yearly.con(conf$con, year))
}

## .. and the factoids for the global project
query.factoids.total.con <- function(con) {
  query <- gen.factoids.basequery()
  res <- dbGetQuery(con, query)

  return(res)
}


### General SQL helper functions
## Test if a table is empty (returns false) or not (returns true)
table.has.entries <- function(conf, table) {
  dat <- dbGetQuery(conf$con, str_c("SELECT * from ", table))

  return (dim(dat)[1] > 0)
}
