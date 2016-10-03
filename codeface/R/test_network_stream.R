library(testthat)
library(igraph)
source("network_stream.r")
source("utils.r")

## Test that the edgelist construction functions correctly using both the mutual
## entity edit, co-change, and semantic similarity concepts
test.construct.edgelist <- function() {
  ## simulate two developer groups and then we will vary the relation
  ## between the entities that the two groups work on to see if the
  ## two groups are correctly connected
  entity.group.1 <- c("f1", "f2")
  author.group.1 <- c(1, 2)
  entity.group.2 <- c("f5", "f6")
  author.group.2 <- c(3, 4)

  ## Random sample entities and authors such that author group 1 contributes
  ## to entity group 1 and author group 2 contributes to entity group 2
  ## keep in mind that the author groups and entity groups are disjoint sets
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

  ## Create the historical commit data frame that included a co-change relationship
  ## between the disjoint entity sets
  commit.num.hist <- 10
  entity.hist <- c("f1", "f6") ## co-change relationship between these entities
  commit.id <- sort(rep(1:commit.num.hist, length(entity.hist)))
  commit.df.hist <- data.frame(author=6, entity=rep(entity.hist, commit.num.hist),
                               id=commit.id, CommitDate=1, size=1, stringsAsFactors=F)

  commit.list <- list(commit.df.hist=commit.df.hist, commit.df=commit.df.dev)

  ## Perform without co-change
  non.co.change.edgelist <- construct.edgelist(commit.list, add.co.change.rel=F,
                                               add.semantic.rel=F)

  ## Check if any incorrect edges were creates, there should not be any edges
  ## between the two author groups, i.e., the graph should be disconnected
  non.co.change.test.res <- !is.connected(graph.data.frame(non.co.change.edgelist$edgelist))

  ## With co-change we should now see that the independent developer groups are
  ## connected because of a historical co-change coupling
  co.change.edgelist <- construct.edgelist(commit.list, add.co.change.rel=T,
                                           add.semantic.rel=F)
  co.change.test.res <- is.connected(graph.data.frame(co.change.edgelist$edgelist))

  ## Test semantic relation
  ## Construct a vocabulary that can be sampled to generate an implementation of
  ## an entity
  impl.vocab.1 <- c("database", "query", "row", "table", "sql", "relational",
                    "join", "unique", "index", "column", "schema", "select", "from")
  impl.vocab.2 <- c("gui", "window", "button", "color", "size", "view", "control",
                    "callback", "interupt", "action", "condition", "if", "for")

  ## Generate an implementation for f1 and f5 drawing from the same implementation
  ## vocabulary to make them semantically similar
  f1.impl <- paste(sample(impl.vocab.1, size=10, replace=TRUE), collapse=" ")
  f5.impl <- paste(sample(impl.vocab.1, size=10, replace=TRUE), collapse=" ")

  ## Again for f2 and f5 using another implementation vocabulary, again they
  ## will be semantically simlary since the sample from the same vocabulary
  f2.impl <- paste(sample(impl.vocab.2, size=10, replace=TRUE), collapse=" ")
  f6.impl <- paste(sample(impl.vocab.2, size=10, replace=TRUE), collapse=" ")
  impl.list <- list(f1=f1.impl, f2=f2.impl, f5=f5.impl, f6=f6.impl)

  ## Add the implementations to the commit data frame
  commit.df.impl <-
    ldply(names(impl.list),
          function(entity.name) {
            commit.df.dev[commit.df.dev$entity==entity.name, "impl"] <- impl.list[[entity.name]]
            return(commit.df.dev[commit.df.dev$entity==entity.name, ])
          })

  commit.list <- list(commit.df=commit.df.impl)
  semantic.connected.edgelist <- construct.edgelist(commit.list,
                                                    add.co.change.rel=FALSE,
                                                    add.semantic.rel=TRUE)

  ## The network should be connected because the two developer groups are
  ## working on semantically similar entities
  semantic.connected.test.res <-
    is.connected(graph.data.frame(semantic.connected.edgelist$edgelist))

  ## Now test the case where the semantic relation does not bridge the two
  ## developer groups
  commit.df.impl[commit.df.impl$entity== "f2", "impl"] <- f5.impl
  commit.df.impl[commit.df.impl$entity== "f5", "impl"] <- f2.impl
  commit.list <- list(commit.df=commit.df.impl)
  semantic.edgelist.disjoint <-
    construct.edgelist(commit.list, add.co.change.rel=FALSE, add.semantic.rel=TRUE)

  ## The network should not be connected because the two developer groups
  ## are working on semantically disjoint entities
  semantic.disjoint.test.res <-
    !is.connected(graph.data.frame(semantic.edgelist.disjoint$edgelist))

  res <- all(non.co.change.test.res, co.change.test.res,
             semantic.connected.test.res, semantic.disjoint.test.res)

  return(res)
}


test_that("Edgelist construction functions correctly", {
      expect_true(test.construct.edgelist())
    })