source("dependency_analysis.r")

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
                                 add.co.change.rel=FALSE) {
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
                                      start.date, end.date)

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
                                                          add.co.change.rel),
                 mc.cores=n.cores)
      })

  network.stream <- unlist(network.stream, recursive=FALSE, use.names=FALSE)
  names(network.stream) <- sapply(network.stream, function(net) net$cycle)

  return(network.stream)
}


construct.edgelist <- function(commit.list, add.co.change.rel) {
  ## Compute relation for developer contribution to common entity
  entity.groups <- aggregate.on.common.entity(commit.list$commit.df)

  ## Compute relation for developers contribution to co-change coupled
  ## entities
  if(add.co.change.rel) {
    ## Add the co-change relation to the entity grouping
    entity.groups <- add.co.change.aggregation(commit.list$commit.df.hist,
                                               entity.groups)
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


add.co.change.aggregation <- function(commit.df, entity.group) {
  ## Get list of currently changed entities, then we can use this information
  ## to eliminate the historical relationships that are not relavent to the current
  ## development
  relavent.entity.list <- unique(do.call(rbind, entity.group)$entity)
  names(relavent.entity.list) <- relavent.entity.list

  ## Remove non-relavent entities from the historical commits which fall outside
  ## of the current development window
  commit.df <- commit.df[commit.df$entity %in% relavent.entity.list, ]

  ## Compute co-change relationship using frequent item set mining
  freq.item.sets <- compute.frequent.items(commit.df)
  ## Compute an edgelist from the frequent item sets
  co.change.edgelist <- compute.item.sets.edgelist(freq.item.sets)

  ## If co-change relationships are identified then add them
  ## Create a mapping of entities to their co-changed entities
  co.change.map <-
    lapply(relavent.entity.list,
           function(entity) {
             ## Find co-change relationships for this entity
             col.1.entity.match <- co.change.edgelist$X1 == entity
             col.2.entity.match <- co.change.edgelist$X2 == entity
             co.change.entities <- union(co.change.edgelist[col.1.entity.match, "X2"],
                                         co.change.edgelist[col.2.entity.match, "X1"])
             return(co.change.entities)})

  ## Group entity groups that have a co-change relationship
  entity.group <-
    lapply(names(entity.group),
           function(entity.name) {
             entity.co.change.group <- co.change.map[[entity.name]]
             df.list <- entity.group[entity.co.change.group]
             ## Combine data frames
             df.co.change <- do.call(rbind, df.list)
             res <- rbind(df.co.change, entity.group[[entity.name]])})

  return(entity.group)
}


generate.person.edgelist <- function(entity.group) {
  edgelist <- data.frame()
  edgelist <-
    ldply(entity.group, function(g) {
      edgelist.group <- apply(g, 1, function(r) {
        edge <- r['commitDate'] > g$commitDate
        edge.df <- data.frame()

        if (any(edge)) {
          edge.df <- data.frame(from=as.numeric(r['author']), to=g$author[edge],
                                weight=g$size[edge], row.names=NULL)
        }

        return(edge.df)})

      res <- do.call(rbind,edgelist.group)
      return(res)})

  edgelist$.id <- NULL

  ## Aggregate edge multiplicity into a single edge
  edgelist.simplified <- ddply(edgelist, .(from, to),
                              function(r) data.frame(weight=sum(r['weight'])))

  return(edgelist.simplified)
}