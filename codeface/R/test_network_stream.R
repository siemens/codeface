library(testthat)
library(igraph)
source("network_stream.r")


## Test that the edgelist construction functions correctly using both the mutual
## entity edit and co-change concept
test.construct.edgelist <- function() {
  entity.group.1 <- c("f1", "f2")
  author.group.1 <- c(1, 2)
  entity.group.2 <- c("f5", "f6")
  author.group.2 <- c(3, 4)

  ## Random sample entities and authors
  set.seed(1)
  commit.num.dev <- 20
  entity.dev <- lapply(list(entity.group.1, entity.group.2),
                       function(entity.group) sample(entity.group,
                                                     commit.num.dev,
                                                     replace=T))
  author.dev <- lapply(list(author.group.1, author.group.2),
                       function(author.group) sample(author.group,
                                                     commit.num.dev,
                                                     replace=T))

  commit.df.dev <- data.frame(author=unlist(author.dev),
                              entity=unlist(entity.dev),
                              size=1, stringsAsFactors=F)
  commit.df.dev$commitDate <- 1:nrow(commit.df.dev)

  ## Create the historical commit data frame
  commit.num.hist <- 10
  entity.hist <- c("f1", "f6") ## co-change relationship between these entities
  commit.id <- sort(rep(1:commit.num.hist, length(entity.hist)))
  commit.df.hist <- data.frame(author=6, entity=rep(entity.hist, commit.num.hist),
                               id=commit.id, CommitDate=1, size=1, stringsAsFactors=F)

  commit.list <- list(commit.df.hist=commit.df.hist, commit.df=commit.df.dev)

  ## Perform without co-change
  non.co.change.edgelist <- construct.edgelist(commit.list, add.co.change.rel=F)

  ## Check if any incorrect edges were creates, there should not be any edges
  ## between the two author groups, i.e., the graph should be disconnected
  non.co.change.test.res <- !is.connected(graph.data.frame(non.co.change.edgelist$edgelist))

  ## With co-change we should now see that the independent developer groups are
  ## connected because of a historical co-change coupling
  co.change.edgelist <- construct.edgelist(commit.list, add.co.change.rel=T)
  co.change.test.res <- is.connected(graph.data.frame(co.change.edgelist$edgelist))

  ## If both test true then pass test
  res <- non.co.change.test.res & co.change.test.res

  return(res)
}


test_that("Edgelist construction functions correctly", {
      expect_true(test.construct.edgelist())
    })