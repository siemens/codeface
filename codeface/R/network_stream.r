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


commits.to.edgelist <- function(commit.df, cycle) {
  commit.df$author <- as.character(commit.df$author)

  ## Group all commits on a common entity
  entity.group <-aggregate.on.entity(commit.df)

  ## Generation the edges list base on contribution to common
  ## entity
  edgelist <- generate.person.edgelist(entity.group)
  author.id <- unique(commit.df$author)
  vertex.data <- data.frame(id=author.id)

  return(list(edgelist=edgelist, vertex.data=vertex.data,
              cycle=cycle))
}


build.dev.net.stream <- function(con, project.id, type, dates.df) {
  ## build a graph stream from a data frame of start and end dates,
  ## similar to build.dev.net except the edge lists for each date range
  ## are generated in parallel
  limit <- 50
  n.cores <- get.num.cores()
  n.groups <- ceiling(nrow(dates.df) / n.cores)
  g.indx <- as.vector(sapply(1:n.groups, function(i) rep(i, n.cores)))
  g.indx <- g.indx[1:nrow(dates.df)]
  df.sets <- split(dates.df, g.indx)

  network.stream <-
    lapply(df.sets,
      function(df) {
      # Query db for commit sets, can't be done in parallel
      commit.sets <- apply(df, 1, function(r) {
        start.date <- r['start.date']
        end.date <- r['end.date']
        query.dependency(con, project.id, type, limit, start.date,
                         end.date)})

      names(commit.sets) <- paste(df$start.date, df$end.date, sep="-")

      mclapply(names(commit.sets),
        function(cycle) commits.to.edgelist(commit.sets[[cycle]], cycle),
        mc.cores=n.cores)
      })

  network.stream <- unlist(network.stream, recursive=FALSE, use.names=FALSE)
  names(network.stream) <- sapply(network.stream, function(net) net$cycle)

  return(network.stream)
}


aggregate.on.entity <- function(commit.df) {
    ## Group rows that make a change to a common entity
    commit.df$entity <- as.factor(commit.df$entity)
    entity.group <- split(commit.df, commit.df$entity)

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
          edge.df <- data.frame(from=r['author'], to=g$author[edge],
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