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
  commit.count.df <- get.commits.by.date.con(con, project.id, start.date, end.date,
                                             commit.count=TRUE)
  developer.class <- get.developer.class(commit.count.df)

  return(developer.class)
}

## Low-level function to compute classification
get.developer.class <- function(author.commit.count, threshold=0.8,
                                quantile.threshold=TRUE) {
  developer.class <- author.commit.count[order(-author.commit.count$freq),]
  num.commits <- sum(developer.class$freq)
  if (quantile.threshold) {
    commit.threshold <- quantile(developer.class$freq, probs=threshold)
    core.test <- developer.class$freq > commit.threshold
  } else {
    commit.threshold <- round(threshold * num.commits)
    core.test <- cumsum(developer.class$freq) < commit.threshold
  }

  developer.class[core.test, "class"] <- "core"
  developer.class[!core.test, "class"] <- "peripheral"
  developer.class$metric <- "commit.count"

  return(developer.class)
}

## Determine developer class based on vertex centrality
get.developer.class.centrality <- function(edgelist, vertex.ids, threshold=0.8,
                                           metric="degree",
                                           quantile.threshold=TRUE) {
  if (metric=="degree") FUN <- igraph::degree
  if (metric=="evcent") FUN <- evcent.named
  if (metric=="page.rank") FUN <- page.rank.named

  graph <- graph.data.frame(edgelist, directed=TRUE,
                            vertices=data.frame(vertex.ids))
  centrality.vec <- sort(FUN(graph), decreasing=T)
  developer.class <- data.frame(author=names(centrality.vec),
                                centrality=as.vector(centrality.vec))
  if (quantile.threshold) {
    centrality.threshold <- quantile(centrality.vec, probs=threshold)
    core.test <- developer.class$centrality > centrality.threshold
  } else{
    centrality.threshold <- threshold * sum(centrality.vec)
    core.test <- cumsum(developer.class$centrality) < centrality.threshold
  }

  developer.class[core.test, "class"] <- "core"
  developer.class[!core.test, "class"] <- "peripheral"
  developer.class$metric <- metric

  return(developer.class)
}


## Compare aggreement between developer classes
compare.classification <- function(developer.class.1, developer.class.2) {
  classes.merged <- merge(developer.class.1, developer.class.2, by="author")
  classes.merged$match <- classes.merged$class.x == classes.merged$class.y
  match.total <- nrow(subset(classes.merged, match==T)) / nrow(classes.merged)
  core.recall <- nrow(subset(classes.merged, match==T & class.x=="core")) /
                 nrow(subset(classes.merged, class.x=="core"))
  core.precision <- nrow(subset(classes.merged, match==T & class.x=="core")) /
                    nrow(subset(classes.merged, class.y=="core"))
  peripheral.recall <- nrow(subset(classes.merged, match==T & class.x=="peripheral")) /
                       nrow(subset(classes.merged, class.x=="peripheral"))
  peripheral.precision <- nrow(subset(classes.merged, match==T & class.x=="peripheral")) /
                          nrow(subset(classes.merged, class.y=="peripheral"))
  comparison <- list(total=match.total,
                     core.recall=core.recall,
                     core.precision=core.precision,
                     peripheral.recall=peripheral.recall,
                     peripheral.precision=peripheral.precision)
  return(comparison)
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

## Centrality metric wrappers to generate named vectors
page.rank.named <- function(g) {
  cent.vec <- page.rank(g)$vector
  names(cent.vec) <- V(g)$name
  return(cent.vec)
}

evcent.named <- function(g) {
  cent.vec <- evcent(g)$vector
  names(cent.vec) <- V(g)$name
  return(cent.vec)
}
