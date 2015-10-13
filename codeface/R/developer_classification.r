suppressMessages(library(igraph))
suppressMessages(library(markovchain))

source("db.r")
source("query.r")

## Classify a set of developers based on the number of commits made withing a
## time range, core developers are those which are responsible for a given
## percentage of the work load with a default threshold set a 80% according
## to Ref: Terceiro A, Rios LR, Chavez C (2010) An empirical study on
##         the structural complexity introduced by core and peripheral
##         developers in free software projects.
get.developer.class.con <- function(con, project.id, start.date, end.date) {
  commit.df <- get.commits.by.date.con(con, project.id, start.date, end.date)
  developer.class <- get.developer.class(commit.df)

  return(developer.class)
}

## Low-level function to compute classification
get.developer.class <- function(commit.df, threshold=0.8) {
  developer.class <- count(commit.df, "author")
  developer.class <- developer.class[order(-developer.class$freq),]
  num.commits <- nrow(commit.df)
  commit.threshold <- round(threshold * num.commits)
  core.test <- cumsum(developer.class$freq) < commit.threshold
  developer.class[core.test, "class"] <- "core"
  developer.class[!core.test, "class"] <- "peripheral"

  return(developer.class)
}

## Determine developer class based on vertex centrality
get.developer.class.centrality <- function(edgelist, vertex.ids, threshold=0.8,
                                           FUN=igraph::degree) {
  graph <- graph.data.frame(edgelist, directed=TRUE,
                            vertices=data.frame(vertex.ids))
  centrality.vec <- sort(FUN(graph), decreasing=T)
  developer.class <- data.frame(author=names(centrality.vec),
                                centrality=as.vector(centrality.vec))
  centrality.threshold <- threshold * sum(centrality.vec)
  core.test <- cumsum(developer.class$centrality) < centrality.threshold
  developer.class[core.test, "class"] <- "core"
  developer.class[!core.test, "class"] <- "peripheral"

  return(developer.class)
}


## Compare aggreement between developer classes
compare.classification <- function(developer.class.1, developer.class.2) {
  classes.merged <- merge(developer.class.1, developer.class.2, by="author")
  classes.merged$match <- classes.merged$class.x == classes.merged$class.y
  percent.match <- nrow(subset(classes.merged, match==T)) / nrow(classes.merged)
  return(percent.match)
}


## Compute markov chain for developer class transitions
compute.class.markov.chain <- function(developer.class.list) {
  index <- names(developer.class.list)
  turnover <-
      lapply(index,
          function(t) {
            class.state.t <- developer.class.list[[t]]
            res <- class.state.t[, c("author", "class")]
            state.t <- paste("state.", t, sep="")
            colnames(res) <- c("id", state.t)
            return(res)
          })
  turnover.all <- join_all(turnover, by="id", type="full")
  turnover.all[is.na(turnover.all)] <- "absent"
  markov.chain <- markovchainFit(data=turnover.all[, -1], method="mle")$estimate
  return(markov.chain)
}