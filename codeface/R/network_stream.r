library(data.table)
source("dependency_analysis.r")
source("semantic_dependency.r")

build.dev.net <- function(con, project.id, type, start.date, end.date) {
  ## Generate a graph from common contribution to a given entity type

  ## con: DB connection
  ## project.id: unique identifier for the project
  ## type: code entity, eg. function, file, class on which the common
  ## contribution indicates a collaborative link
  limit <- 50

  commit.df <- query.dependency(con, project.id, type, limit, start.date,
                                end.date)

  edgelist <- commits.to.edgelist(commit.df)

  return(edgelist)
}


entity.group.to.edgelist <- function(entity.groups, cycle) {
  ## Generate the edges list based on contribution to common entity group
  edgelist <- generate.person.edgelist(entity.groups)

  ## Get all authors since some may be isolated and will then not appear in the
  ## edgelist
  author.id <- unique(do.call(rbind,entity.groups)$author)
  vertex.data <- data.frame(id=author.id)

  return(list(edgelist=edgelist, vertex.data=vertex.data,
              cycle=cycle))
}


build.dev.net.stream <- function(con, project.id, type, dates.df,
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
        commit.df <- query.dependency(con, project.id, type, limit,
                                      start.date, end.date, impl=add.semantic.rel)

        ## Co-change relationship needs a longer history of commits
        commit.df.hist <- list()
        if(add.co.change.rel) {
          ## Get co-change relationships for previous and current commits with
          ## a limit for the history to consider
          historical.limit <- ddays(365)
          start.date.hist <- as.Date(start.date) - historical.limit
          end.date.hist <- start.date
          commit.df.hist <- query.dependency(con, project.id, type, limit,
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
    entity.factor <- as.factor(commit.df$entity)
    entity.group <- split(commit.df, entity.factor)

  return(entity.group)
}


add.entity.relation <- function(commit.df, entity.group, type) {
  ## Get list of currently changed entities, then we can use this information
  ## to eliminate the historical relationships that are not relavent to the current
  ## development
  relavent.entity.list <- unique(do.call(rbind, entity.group)$entity)
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

  entity.rel.map <-
    lapply(relavent.entity.list,
           function(entity) {
             ## Find co-change relationships for this entity
             col.1.entity.match <- edgelist$X1 == entity
             col.2.entity.match <- edgelist$X2 == entity
             related.entities <- union(edgelist[col.1.entity.match, "X2"],
                                       edgelist[col.2.entity.match, "X1"])
             return(related.entities)})

  ## Group entity groups that have a co-change relationship
  entity.group <-
    lapply(names(entity.group),
           function(entity.name) {
             entity.rel.group <- entity.rel.map[[entity.name]]
             df.list <- entity.group[entity.rel.group]
             ## Combine data frames
             df.relations <- do.call(rbind, df.list)
             res <- rbind(df.relations, entity.group[[entity.name]])})

  return(entity.group)
}


generate.person.edgelist <- function(entity.group) {
  loginfo("Computing person edgelist", logger="")
  ## Remove entity groups of size 1 since there could not
  ## possibly be collaboration on these entities
  keep.element <- sapply(entity.group, function(g) length(unique(g$author)) > 1)
  entity.group <- entity.group[unlist(keep.element)]

  edgelist.total <-
    mclapply(entity.group, mc.cores = get.num.cores(),
      function(g) {
        ## Sort for optimization
        g <- g[order(g$commitDate, decreasing=T),]
        num.rows <- nrow(g)
        edgelist.part <- vector(mode="list", length=num.rows)

        ## Loop over data frame rows containing code contributions
        ## to connect developers making commits to related artifacts
        for (i in seq(from=1,to=num.rows)) {
          cmt.1 <- g[i,]
          row.idx <- seq(from=i, to=num.rows)
          cmt.1.size <- cmt.1$size
          to <- g[row.idx, 'author']
          from <- rep(cmt.1$author, length(to))
          weight <- g[row.idx, 'size'] + cmt.1.size
          edgelist.part[[i]] <- cbind(from, to, weight)
        }
        res <- do.call(rbind, edgelist.part)
        return(res)})

  edgelist <- data.frame()
  if (length(edgelist.total) != 0) {
    edgelist <- do.call(rbind, edgelist.total)

    ## Aggregate edge multiplicity into a single edge
    edgelist <- data.table(edgelist)
    edgelist <- edgelist[, list(weight=sum(weight)), by=.(from, to)]
    edgelist <- as.data.frame(edgelist)
  }

  return(edgelist)
}
