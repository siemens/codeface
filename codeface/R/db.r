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

## Generic database helper functions. More specific queries should
## be directly included into the code

## TODO: Add S3 database helper class similar to dbManager.py

suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
source("boundaries.r")

sq <- function(string) {
  if(is.null(string)) return(NULL)
  return(str_c("'", string, "'"))
}

get.project.id <- function(con, name) {
  res <- dbGetQuery(con, str_c("SELECT id FROM project WHERE name=", sq(name)))

  return(res$id)
}

## Determine the ID of a given plot for a given project. Since
## plots are not created in parallel, we need no locking.
## Also, clear the plot for new data. This function is supposed to be
## used for time series that are created during the global analysis.
get.clear.plot.id.con <- function(con, pid, plot.name, range.id=NULL,
                                  labelx=NULL, labely=NULL) {
  query <- str_c(" FROM plots WHERE name=", sq(plot.name),
                 " AND projectId=", pid)
  if (!is.null(range.id)) {
    query <- str_c(query, " AND releaseRangeId=", range.id)
  }

  dbGetQuery(con, str_c("DELETE", query))

  ## Prepare data and insert
  insert.data <- c(name=sq(plot.name),
                   projectId=pid,
                   releaseRangeId=range.id,
                   labelx=sq(labelx),
                   labely=sq(labely))
  values <- paste(insert.data, collapse=", ")
  columns <-  paste(names(insert.data), collapse=", ")
  insert.query <- str_c("INSERT INTO plots (", columns, ") ",
                        "VALUES (", values, ")")
  dbGetQuery(con, insert.query)

  res <- dbGetQuery(con, str_c("SELECT id", query))

  if (nrow(res) != 1) {
    stop("Internal error: Plot ", plot.name, " appears multiple times in DB",
         "for project ID ", pid)
}

  return(res$id)
}

get.clear.plot.id <- function(conf, plot.name, range.id=NULL, labelx=NULL,
                              labely=NULL) {
  return(get.clear.plot.id.con(conf$con, conf$pid, plot.name, range.id,
                               labelx, labely))
}

## Obtain a plot ID for a plot that is known to exist
get.plot.id.con <- function(con, pid, plot.name, range.id=NULL) {
  query <- str_c(" FROM plots WHERE name=", sq(plot.name),
                 " AND projectId=", pid)
  if (!is.null(range.id)) {
    query <- str_c(query, " AND releaseRangeId=", range.id)
  }
  res <- dbGetQuery(con, str_c("SELECT id", query))
  if (nrow(res) < 1) {
    stop("Internal error: Plot ", plot.name, " not found in DB",
         " for project ID ", pid)
  }
  return(res$id)
}

get.plot.id <- function(conf, plot.name, range.id=NULL) {
  return(get.plot.id.con(conf$con, conf$pid, plot.name, range.id))
}

## Create a plot ID for a given plot if it does not exists, or return
## an existing one. This function is supposed to be used for plots that
## are built incrementally, that is, can grow when new releases are
## added to the project.
get.or.create.plot.id.con <- function(con, pid, plot.name, range.id=NULL) {
  query <- str_c("SELECT id FROM plots WHERE name=", sq(plot.name),
                 " AND projectId=", pid)
  if (!is.null(range.id)) {
    query <- str_c(query, " AND releaseRangeId=", range.id)
  }
  res <- dbGetQuery(con, str_c(query, ";"))

  if (nrow(res) < 1) {
    ## Plot ID is not assigned yet, create one
    res <- get.clear.plot.id.con(con, pid, plot.name, range.id)
  } else {
    res <- res$id
  }

  return(res)
}

get.or.create.plot.id <- function(conf, plot.name, range.id=NULL) {
  return(get.or.create.plot.id.con(conf$con, conf$pid, plot.name, range.id))
}


## Determine the ID of a tag, given its textual form
get.revision.id <- function(conf, tag) {
  res <- dbGetQuery(conf$con,
                    str_c("SELECT id FROM release_timeline WHERE projectId=",
                          conf$pid, " AND tag=", sq(tag), " AND type='release'"))

  if (nrow(res) > 1) {
    stop("Internal error: Revision if for tag ", tag, " (project ", conf$project,
         ") appears multiple times in DB!")
  }

  return(res$id)
}

get.range.id <- function(conf, tag.start, tag.end) {
  start.id <- get.revision.id(conf, tag.start)
  end.id <- get.revision.id(conf, tag.end)

  res <- dbGetQuery(conf$con,
                    str_c("SELECT id FROM release_range WHERE projectId=",
                          conf$pid, " AND releaseStartId=", start.id,
                          " AND releaseEndId=", end.id))
  return(res$id[1])
}

# Get release and release candidate dates for a given project
get.release.rc.dates <- function(conf) {
  res <- dbGetQuery(conf$con,
                    str_c("SELECT * FROM release_timeline WHERE projectId=",
                          conf$pid, sep=""))
  res$type <- as.factor(res$type)

  ## When no rc dates are available yet, all entries of res$date
  ## are NAs -- in which case ymd_hms will throw an error, so we need
  ## to skip trying to convert the vector. If there's at least one
  ## valie date entry included, ymd_hms will happily convert the
  ## NAs to NAs.
  if (sum(is.na(res$date)) != length(res$date)) {
    res$date <- ymd_hms(res$date, quiet=TRUE)
  }

  return(res)
}

## Get release dates (without release candidates) for a given project
get.release.dates <- function(conf) {
  res <- get.release.rc.dates(conf)
  res <- res[res$type=="release",]

  return(res)
}


clear.all.clusters <- function(conf, range.id, method) {
  dbGetQuery(conf$con, str_c("DELETE FROM cluster ",
          "WHERE clusterMethod=", sq(method),
          " AND projectId=", conf$pid,
          " AND releaseRangeId=", range.id))
}

get.clear.cluster.id <- function(conf, range.id, method, num) {
  dbGetQuery(conf$con, str_c("DELETE FROM cluster ",
                             "WHERE clusterMethod=", sq(method),
                             " AND projectId=", conf$pid,
                             " AND releaseRangeId=", range.id,
                             " AND clusterNumber=", num))

  dbGetQuery(conf$con, str_c("INSERT INTO cluster (projectId, clusterNumber, ",
                              "releaseRangeId, clusterMethod) VALUES (",
                              conf$pid, ", ", num, ", ", range.id, ", ",
                              sq(method), ")"))
  res <- dbGetQuery(conf$con, str_c("SELECT id from cluster ",
                                    "WHERE clusterMethod=", sq(method),
                                    " AND projectId=", conf$pid,
                                    " AND releaseRangeId=", range.id,
                                    " AND clusterNumber=", num))

  return(res$id)
}

## Obtain the ID of a per release-range, per technique pagerank table.
## technique is by convention 0 for normal pagerank and 1 for transposed
## pagerank
get.clear.pagerank.id.con <- function(con, range.id, technique) {
  if (technique != 0 && technique != 1) {
    stop("Internal error: Invalid technique specified in get.pagerank.id")
  }
  dbGetQuery(con, str_c("DELETE FROM pagerank ",
                        "WHERE releaseRangeId=", range.id,
                        " AND technique=", technique))
  dbGetQuery(con, str_c("INSERT INTO pagerank (releaseRangeId, technique)",
                        " VALUES (", range.id, ", ", technique, ")"))
  return(get.pagerank.id.con(con, range.id, technique))
}

get.pagerank.id.con <- function(con, range.id, technique) {
  if (technique != 0 && technique != 1) {
    stop("Internal error: Invalid technique specified in get.pagerank.id")
  }
  res <- dbGetQuery(con, str_c("SELECT id from pagerank ",
                               "WHERE releaseRangeId=", range.id,
                               " AND technique=", technique))
  return(res$id)
}

get.clear.pagerank.id <- function(conf, range.id, technique) {
  return(get.clear.pagerank.id.con(conf$con, range.id, technique))
}

## Obtain a unique ID for mailing list, given name of the list and project id.
## Delete any existing mailing lists of this name.
gen.clear.ml.id.con <- function(con, ml, pid) {
  dbGetQuery(con, str_c("DELETE FROM mailing_list ",
                               "WHERE projectId=", pid,
                               " AND name=", sq(ml)))
  dbGetQuery(con, str_c("INSERT INTO mailing_list (projectID, name)",
                        " VALUES (", pid, ", ", sq(ml), ")"))
  res <- dbGetQuery(con, str_c("SELECT id from mailing_list ",
                                "WHERE projectId=", pid,
                                " AND name=", sq(ml)))
  return(res$id)
}

gen.clear.ml.id <- function(conf, ml) {
  return(gen.clear.ml.id(conf$con, ml, conf$pid))
}

## Augment the configuration "object" with information that
## is of interest to several analysis passes/operations
augment.conf <- function(conf) {
  conf$tstamps.release <- get.release.dates(conf)
  conf$tstamps.all <- get.release.rc.dates(conf)

  conf$boundaries <- prepare.release.boundaries(conf)

  return(conf)
}

## Establish the connection and store the relevant configuration
## parameters in the project specific configuration structure
init.db <- function(conf) {
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv, host=conf$dbhost, user=conf$dbuser,
                   password=conf$dbpwd, dbname=conf$dbname,
                   port=conf$dbport)
  conf$pid <- get.project.id(con, conf$project)
  conf$con <- con
  dbGetQuery(con, "SET NAMES utf8")

  if (is.null(conf$pid)) {
    stop("Internal error: No ID assigned to project ", conf$project, "\n",
         "(Did you not run the VCS analysis before the ml analysis?)\n")
  }

  ## Set Session wait_timeout variable to 24 hours, default is 8
  query <- str_c("SET SESSION wait_timeout=", 24*60*60)
  dbGetQuery(con, query)

  conf <- augment.conf(conf)

  return(conf)
}

## Same in blue for use cases when no single project is considered.
## We augment the configuration with the con object in this case.
## Can also be used to initialise connections inside parallel worker threads.
init.db.global <- function(conf) {
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv, host=conf$dbhost, user=conf$dbuser,
                   password=conf$dbpwd, dbname=conf$dbname,
                   port=conf$dbport)
  conf$con <- con
  dbGetQuery(con, "SET NAMES utf8")
  return(conf)
}

## provided with a configuration file connect to db and return connection object
connect.db <- function(conf.file) {
  conf <- load.config(conf.file)
  conf <- init.db.global(conf)
  return(conf)
}

## computes a local graph representation
get.graph.data.local <- function(con, p.id, range.id, cluster.method=NULL) {
  g.id <- query.global.collab.con(con, p.id, range.id, cluster.method)
  edgelist.db <- query.cluster.edges(con, g.id)
  v.global.ids <- query.cluster.members(con, g.id)

  ## compute global to local index map
  id.map <- get.index.map(v.global.ids)

  # compute local node ids
  v.local.ids <- map.ids(v.global.ids, id.map)

  ## map global edge list index to local
  edgelist <- data.frame(from=map.ids(edgelist.db$fromId, id.map),
      to=map.ids(edgelist.db$toId, id.map),
      weight=edgelist.db$weight)

  if(is.null(cluster.method)) {
    comm=NULL
    rank=NULL
  }
  else {
    ## get graph communities 
    local.comm <- get.communities.local(con, p.id, range.id, id.map,
                                        cluster.method)
  }

  res <- list(edgelist=edgelist, v.global.ids=v.global.ids,
              v.local.ids=v.local.ids, id.map=id.map, comm=local.comm$comm,
              rank=local.comm$rank)

  return(res)
}

get.communities.local <- function(con, p.id, range.id, id.map, cluster.method){
  g.id <- query.global.collab.con(con, p.id, range.id, cluster.method)
  ## get graph community
  cluster.ids <- query.cluster.ids.con(con, p.id, range.id, cluster.method)
  ## remove main graph cluster id
  cluster.ids   <- cluster.ids[cluster.ids!=g.id]
  cluster.data  <- lapply(cluster.ids,
                         function(c.id)
                           query.cluster.members(con, c.id,
                           prank=TRUE, technique=0))
  ## get the cluster members
  cluster.mem <- lapply(cluster.data, function(cluster) cluster$personId)
  ## reconstruct igraph style communities object 
  comm <- clusters.2.communities(cluster.mem, cluster.method, id.map)

  ## rank
  node.rank.db          <- ldply(cluster.data, data.frame)
  node.rank.db$personId <- map.ids(node.rank.db$personId, id.map)
  node.rank <- c()
  node.rank[node.rank.db$personId] <- node.rank.db$rankValue

  res <- list(comm=comm, rank=node.rank)
  return(res)
}

## Create an index mapping that maps unique person ids to consecutive numbers
## from 1 to N where N is the number of nodes in the graph
## Args:
##  ids: vector of integers
## Return:
##  map: environment (hash table) mapping ids to consecutive integers starting
##       from 1
get.index.map <- function(ids) {
  node.ids <- unique(ids)
  N        <- length(node.ids)
  map      <- new.env(size=N)
  for(i in 1:N) {
    map[[as.character(node.ids[i])]] <- i
  }
  return(map)
}


## Remap all ids in the given a mapping
## Args:
##  ids: id index vector (non-consecutive)
##  map: environment (hash table) mapping global index to consecutive local 
##       index
## Returns:
##  edgelist: edge list with remapped node index
map.ids <- function(ids, map){
  N       <- length(ids)
  new.ids <- c()

  ## Remap ids using the given mapping
  new.ids <- sapply(ids, function(id) map[[as.character(id)]])

  return(new.ids)
}


## Create communities object from clusters list
## Args:
##  clusters: list of global personId vectors mapping people to clusters
##  map: environment (hash table) to map non-consecutive global index to 
##       consecutive local index
## Returns:
##  comm: igraph-like communities object; NULL if there are no communities
clusters.2.communities <- function(cluster.list, cluster.method, map) {
  if (length(cluster.list) == 0) {
    return(NULL)
  }

  membership <- c()
  csize      <- c()

  ## Create membership vector from cluster.list
  for (i in 1:length(cluster.list)) {
    p.global.ids <- cluster.list[[i]]
    ## ids need to be consecutive, use global -> local index map
    p.local.ids  <- map.ids(p.global.ids, map)
    membership[p.local.ids] <- i
    csize[i] <- length(p.local.ids)
  }

  ## Build igraph-like communities object
  comm            <- list()
  class(comm)     <- "communities"
  comm$algorithm  <- cluster.method
  comm$membership <- membership
  comm$csize      <- csize

  return(comm)
}


## Write graph data into the database
write.graph.db <- function(conf, range.id, baselabel, edgelist, j) {
  ## Get a unique cluster id
  cluster.id <- get.clear.cluster.id(conf, range.id, baselabel, j)

  ## Get all ids and write the user cluster mapping
  users <- unique(c(edgelist$fromId, edgelist$toId))
  users.df <- data.frame(id=NA, personId=users, clusterId=cluster.id)
  dbWriteTable(conf$con, "cluster_user_mapping", users.df, append=TRUE, row.names=FALSE)

  ## Write edge list into database
  edgelist <- cbind(clusterId=cluster.id, edgelist)
  dbWriteTable(conf$con, "edgelist", edgelist, append=TRUE, row.names=FALSE)
  ## TODO: Insert the generated dot files into the database
}
