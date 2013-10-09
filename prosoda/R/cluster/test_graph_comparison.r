# Tests for graph_comparison.r

# This file is part of prosoda.  prosoda is free software: you can
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
  V(g.1)$Id <- names
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

##----------------------
##  Executed Statements
##----------------------
test_that("graph.comparison returns expected values", {
      expect_that(graph.comparison.test(), equals(0))
    })
