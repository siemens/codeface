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
## Copyright 2016 by Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## All Rights Reserved.

suppressPackageStartupMessages(library(data.table))
source("dependency_analysis.r")
source("semantic_dependency.r")

build.dev.net <- function(conf, start.date, end.date) {
  ## Generate a graph from common contribution to a given entity type

  ## conf: DB connection (must include type)
  limit <- 50

  commit.df <- query.dependency(conf, limit, start.date, end.date)

  edgelist <- commits.to.edgelist(commit.df)

  return(edgelist)
}


entity.group.to.edgelist <- function(entity.groups, cycle) {
  ## Generate the edges list based on contribution to common entity group
  edgelist <- generate.person.edgelist(entity.groups)

  ## Get all authors since some may be isolated and will then not appear in the
  ## edgelist
  author.id <- unique(rbindlist(entity.groups)$author)
  vertex.data <- data.frame(id=author.id)

  return(list(edgelist=edgelist, vertex.data=vertex.data,
              cycle=cycle))
}


build.dev.net.stream <- function(conf, dates.df,
                                 add.co.change.rel=FALSE,
                                 add.semantic.rel=FALSE) {
  ## Build a graph stream from a data frame of start and end dates,
  ## similar to build.dev.net except the edge lists for each date range
  ## are generated in parallel
  limit <- 50  ## Max allowable files edited by a single commit to include
  n.cores <- get.num.cores()
  n.groups <- ceiling(nrow(dates.df) / n.cores)
  g.indx <- as.vector(sapply(1:n.groups, function(i) rep(i, n.cores)))
  g.indx <- g.indx[1:nrow(dates.df)]
  df.sets <- split(dates.df, g.indx)

  network.stream <-
    lapply(df.sets,
      function(df) {
        ## Query db for commit sets, can't be done in parallel
        commit.lists <- apply(df, 1, function(r) {
        start.date <- r['start.date']
        end.date <- r['end.date']
        commit.df <- query.dependency(conf, limit,
                                      start.date, end.date, impl=add.semantic.rel)

        ## Co-change relationship needs a longer history of commits
        commit.df.hist <- list()
        if(add.co.change.rel) {
          ## Get co-change relationships for previous and current commits with
          ## a limit for the history to consider
          historical.limit <- ddays(365)
          start.date.hist <- as.Date(start.date) - historical.limit
          end.date.hist <- start.date
          commit.df.hist <- query.dependency(conf, type, limit,
                                             start.date.hist, end.date.hist)

          ## Add commit.df because that is relavent also but no point in querying
          ## the database again since we already have that date range
          commit.df.hist <- rbind(commit.df, commit.df.hist)
        }

        cycle <- paste(start.date, end.date, sep="-")

        res <- list(commit.df=commit.df, commit.df.hist=commit.df.hist,
                    cycle=cycle)

        return(res)})

      edgelist <-
        mclapply(commit.lists,
                 function(commit.list) construct.edgelist(commit.list,
                                                          add.co.change.rel,
                                                          add.semantic.rel),
                 mc.cores=n.cores)
      })

  network.stream <- unlist(network.stream, recursive=FALSE, use.names=FALSE)

  ## Remove failed ranges
  runs.failed <- sapply(network.stream, class) == "try-error"
  network.stream[runs.failed] <- NULL
  names(network.stream) <- sapply(network.stream, function(net) net$cycle)

  return(network.stream)
}


construct.edgelist <- function(commit.list, add.co.change.rel, add.semantic.rel) {
  ## Compute relation for developer contribution to common entity
  entity.groups <- aggregate.on.common.entity(commit.list$commit.df)

  ## Compute relation for developers contribution to co-change coupled
  ## entities
  if(add.co.change.rel) {
    ## Add the co-change relation to the entity grouping
    entity.groups <- add.entity.relation(commit.list$commit.df.hist,
                                         entity.groups,
                                         type="co.change")
  }

  ## Compute relation for developers contribution to semantically coupled
  ## entites
  if(add.semantic.rel) {
    entity.groups <- add.entity.relation(commit.list$commit.df, entity.groups,
                                         type="semantic")
  }

  ## Convert grouped entites into developer network, edges are
  ## placed between developers who have made a contribution to
  ## common entity group
  edgelist <- entity.group.to.edgelist(entity.groups, commit.list$cycle)

  return(edgelist)
}


aggregate.on.common.entity <- function(commit.df) {
    ## Group rows that make a change to a common entity
    commit.df <- as.data.table(commit.df)
    commit.df$impl <- NULL
    entity.factor <- as.factor(commit.df$entity)
    entity.group <- split(commit.df, entity.factor)

  return(entity.group)
}


add.entity.relation <- function(commit.df, entity.group, type) {
  ## Get list of currently changed entities, then we can use this information
  ## to eliminate the historical relationships that are not relavent to the current
  ## development
  relavent.entity.list <- unique(rbindlist(entity.group)$entity)
  names(relavent.entity.list) <- relavent.entity.list

  ## If the commit data frame is empty then there is no possibility to
  ## add to the entity group so just return the original
  if(nrow(commit.df) == 0) return(entity.group)

  if(type=="co.change") {
    ## Remove non-relavent entities from the historical commits which fall outside
    ## of the current development window
    commit.df <- commit.df[commit.df$entity %in% relavent.entity.list, ]

    ## Compute co-change relationship using frequent item set mining
    freq.item.sets <- compute.frequent.items(commit.df)
    ## Compute an edgelist from the frequent item sets
    edgelist <- compute.item.sets.edgelist(freq.item.sets)
  }
  else if(type=="semantic") {
    ## Compute the semantic coupling between entities in the commit.df
    semantic.rel <- computeSemanticCoupling(commit.df, threshold=0.7)

    ## Verify that the vertex index is congruent with the row index
    vertex.data <- semantic.rel$vertex.data
    if(!all(1:length(vertex.data$name) == vertex.data$name)) {
      error.str <- "Row index mismatch, terminating execution"
      logerror(error.str, logger="network_stream")
      stop(error.str)
    }

    ## Map vertex id to an entity name
    X1 <- vertex.data[semantic.rel$edgelist$X1, "id"]
    X2 <- vertex.data[semantic.rel$edgelist$X2, "id"]
    edgelist <- data.frame(X1=X1, X2=X2, stringsAsFactors=F)
  }
  else logerror("Incorrect parameter passed", logger="network_stream")

  if (nrow(edgelist)==0) {
    return(entity.group)
  }

  ## Compute entity neighbor list
  g <- graph.data.frame(edgelist, directed=F)
  entity.rel.map <- neighborhood(g, 1)
  names(entity.rel.map) <- V(g)$name

  ## Group entities that share a relation according to entity.rel.map
  entity.group <-
    lapply(relavent.entity.list,
           function(entity.name) {
             entity.rel.group <- entity.rel.map[[entity.name]]$name

             ## If the entity has no neighbors, null is returned
             ## but we still want to add to the entity group
             if(is.null(entity.rel.group)) entity.rel.group <- c(entity.name)

             df.list <- entity.group[entity.rel.group]

             ## Combine data frames
             res <- rbindlist(df.list)})

  names(entity.group) <- relavent.entity.list

  return(entity.group)
}


generate.person.edgelist <- function(entity.group) {
  loginfo("Computing person edgelist", logger="")
  ## Remove entity groups of size 1 since there could not
  ## possibly be collaboration on these entities
  keep.element <- sapply(entity.group, function(g) length(unique(g$author)) > 1)
  entity.group <- entity.group[unlist(keep.element)]

  edgelist.total <-
    mclapply(names(entity.group), mc.cores=2,
      function(entity.name) {
        ## Sort for optimization
        g <- entity.group[[entity.name]]
        g <- g[, list(size=sum(size)), by=list(author, entity)]
        num.rows <- nrow(g)
        edgelist.part <- list() #vector(mode="list", length=num.rows)

        cmt.idx <- which(g$entity==entity.name)

        ## Loop over data frame rows containing code contributions
        ## to connect developers making commits to related artifacts
        for (i in cmt.idx) {
          cmt.from <- g[i,]
          cmt.to <- g[cmt.from$author!=g$author,]
          weight <- cmt.from$size + cmt.to$size
          edgelist.part[[i]] <- data.table(from=cmt.from$author,
                                           to=cmt.to$author,
                                           weight=weight)
        }

        res <- rbindlist(edgelist.part)

        if (nrow(res)!=0) {
          res <- res[, list(weight=sum(weight)), by=list(from,to)]
        } else {
          res <- NULL
        }

        return(res)})

  edgelist <- data.table()
  if (length(edgelist.total) != 0) {
    edgelist <- rbindlist(edgelist.total)

    ## Aggregate edge multiplicity into a single edge
    edgelist <- edgelist[, list(weight=sum(weight)), by=list(from, to)]
    edgelist <- as.data.frame(edgelist)
  }

  return(edgelist)
}
