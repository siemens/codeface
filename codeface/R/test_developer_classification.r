library(testthat)
source("developer_classification.r")

get.developer.class.test <- function() {
  threshold <- 0.8
  sample.size <- 1000

  commit.df <- data.frame(author=sample(1:50, size=sample.size, replace=T))
  author.commit.count <- count(commit.df, "author")
  developer.class <- get.developer.class(author.commit.count, threshold)
  core.class <- subset(developer.class, class=="core")
  res <- sum(core.class$freq) < threshold*sample.size
  return(res)
}

get.developer.class.centrality.test <- function() {
  threshold <- 0.8
  g <- barabasi.game(300)
  edgelist <- get.data.frame(g)
  vertex.ids <- c(as.vector(V(g)), 301:305)
  developer.class <- get.developer.class.centrality(edgelist, vertex.ids,
                                                    threshold)
  core.class <- subset(developer.class, class=="core")
  res <- sum(core.class$centrality) < threshold*sum(igraph::degree(g))
  return(res)
}

compare.classification.test <- function() {
  g <- barabasi.game(300)
  edgelist <- get.data.frame(g)
  vertex.ids <- c(as.vector(V(g)))
  degree.vec <- igraph::degree(g)[sample(seq_along(vertex.ids), length(degree.vec))]
  commits <- unlist(sapply(vertex.ids, function(id) rep(id, degree.vec[id])))
  commit.df <- data.frame(author=commits)
  class.centrality <- get.developer.class.centrality(edgelist, vertex.ids)
  class.commit <- get.developer.class(commit.df)
  class.match <- compare.classification(class.centrality, class.commit)
  res <- class.match > 0 & class.match < 1
  return(res)
}

test_that("get.developer.class returns expected values", {
      expect_true(get.developer.class.test())
    })

test_that("get.developer.class.centrality returns expected values", {
      expect_true(get.developer.class.centrality.test())
    })

test_that("compare.classification returns expected values", {
      expect_true(compare.classification.test())
    })
