library(testthat)
library(igraph)

source("community_metrics.r")

test_that("Graph turnover analysis", {
  graph.1 <- graph.famous("zachary")
  g.ids.1 <- V(graph.1)$name <- as.vector(V(graph.1))
  graph.2 <- graph.1 - c(33,34)
  graph.2 <- add.vertices(graph.2 , 4)
  g.ids.2 <- V(graph.2)$name <- as.vector(V(graph.2))
  turnover <- graph.turnover(graph.1, graph.2, g.ids.1, g.ids.2, 1)

  ## Check result
  res <- c(turnover[33, "state.t.1"] == "core",
           turnover[33, "state.t.2"] == "isolated",
           turnover[35, "state.t.1"] == "absent",
           turnover[35, "state.t.2"] == "isolated")

  expect_true(all(res))
})