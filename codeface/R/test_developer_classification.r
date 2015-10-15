library(testthat)
source("developer_classification.r")

get.developer.class.test <- function() {
  threshold <- 0.8
  sample.size <- 1000

  commit.df <- data.frame(author=sample(1:50, size=sample.size, replace=T))
  author.commit.count <- count(commit.df, "author")
  developer.class <- get.developer.class(author.commit.count, threshold)
  res <- sum(developer.class$core$freq) < threshold*sample.size
  return(res)
}

get.developer.class.centrality.test <- function() {
  threshold <- 0.8
  g <- barabasi.game(300)
  edgelist <- get.data.frame(g)
  vertex.ids <- c(as.vector(V(g)), 301:305)
  developer.class <- get.developer.class.centrality(edgelist, vertex.ids,
                                                    threshold, degree)
  res <- sum(developer.class$core$centrality) < threshold*sum(degree(g))
  return(res)
}

test_that("get.developer.class returns expected values", {
      expect_true(get.developer.class.test())
    })

test_that("get.developer.class.centrality returns expected values", {
      expect_true(get.developer.class.centrality.test())
    })
