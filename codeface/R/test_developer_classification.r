## This file is part of Codeface. Codeface is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2015 by Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## All Rights Reserved.

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
  degree.vec <- igraph::degree(g)[sample(seq_along(vertex.ids), length(vertex.ids))]
  commits <- unlist(sapply(vertex.ids, function(id) rep(id, degree.vec[id])))
  commit.df <- data.frame(author=commits)
  class.centrality <- get.developer.class.centrality(edgelist, vertex.ids)
  class.commit <- get.developer.class(count(commit.df, "author"))
  class.match <- compare.classification(class.centrality, class.commit)
  res <- all(class.match$total > 0 & class.match < 1)
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
