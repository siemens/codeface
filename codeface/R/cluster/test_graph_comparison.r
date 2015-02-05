# Tests for graph_comparison.r

# This file is part of Codeface. Codeface is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2013 by Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>

library(testthat)
source("graph_comparison.r")

##---------------------------
## Test Function Definitions
##---------------------------
graph.comparison.test <- function () {
  ##       1,2,3,4,5,6,7,8
  r.1 <- c(0,0,1,0,0,0,0,0)
  r.2 <- c(0,0,1,0,0,0,0,0)
  r.3 <- c(0,0,0,0,1,0,0,0)
  r.4 <- c(1,0,0,0,1,0,0,0)
  r.5 <- c(0,0,0,0,0,0,0,0)
  r.6 <- c(0,0,0,0,0,0,1,1)
  r.7 <- c(0,0,0,0,0,0,0,0)
  r.8 <- c(0,0,0,0,1,0,1,0)

  adj.matrix <- t(matrix(data = c(r.1,r.2,r.3,r.4,r.5,r.6,r.7,r.8), ncol = 8, nrow = 8))
  g.1 <- graph.adjacency(adj.matrix)
  names <- c("joe", "bill", "jill", "mike", "ben", "chris", "meg", "Dee")
  V(g.1)$name <- names
  idx <- 1:vcount(g.1)
  rand.idx <- sample(idx,size=length(idx))
  g.2 <- graph.adjacency(g.1[rand.idx,rand.idx])
  res <- graph.comparison(g.1, g.2)
  if(all(res==0)){
    print("Test Passed")
    return(0)
  }
  else {
    print("Test Failed")
    return(1)
  }
}

graph.comparison.subgraph <- function () {
  vertices.1 = data.frame(
    id1 =  c(      3,        6,       1,       9,       10),
    name = c('vert_3', 'vert_6','vert_1','vert_9','vert_10'))

  edgelist.1 = data.frame(
    from =   c(3,6,1,9,10),
    to =     c(6,1,9,10,3),
    weight = c(2,2,2,2,2))

  vertices.2 = data.frame(
    id2 =  c(      3,        7,       1,       9,       10),
    name = c('vert_3', 'vert_7','vert_1','vert_9','vert_10'))

  edgelist.2 = data.frame(
    from =   c(3,7,1,9,10,10),
    to =     c(7,1,9,10,3,9),
    weight = c(2,2,2,2,2,2))

  merged.vertex <- data.frame(
    id =   c(      3,        7,       1,       9,       10,        6),
    name = c('vert_3', 'vert_7','vert_1','vert_9','vert_10', 'vert_6'))

  merged.g.1 <- graph.data.frame(edgelist.1, vertices=merged.vertex)
  merged.g.2 <- graph.data.frame(edgelist.2, vertices=merged.vertex)

  # res contains all edges
  res.merged <- graph.comparison(merged.g.1, merged.g.2)

  if(all(abs(res.merged$graph.diff - c(2/3, 1, 2/3, 1/3, 1/3, 1)) < 1e-5) &
       all(res.merged$vertex.names == merged.vertex$name)){
  }
  else {
    print("Test Failed (res.merged)")
    return(1)
  }

  g.1 <- graph.data.frame(edgelist.1, vertices=vertices.1)
  g.2 <- graph.data.frame(edgelist.2, vertices=vertices.2)

  res <- graph.comparison(g.1, g.2)

  # res contains vert_3, vert_1, vert_9, vert_10
  if(all(abs(res$graph.diff - c(0, 0, 1/3, 1/3)) < 1e-5) &
     all(res$vertex.names == c("vert_3", "vert_1", "vert_9", "vert_10"))){
    print("Test Passed")
    return(0)
  }
  else {
    print("Test Failed (res)")
    return(1)
  }
}

##----------------------
##  Executed Statements
##----------------------
test_that("graph.comparison returns expected values", {
      expect_that(graph.comparison.test(), equals(0))
    })
