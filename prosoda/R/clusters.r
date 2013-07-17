## This file is part of prosoda.  prosoda is free software: you can
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
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Definitions and utility function for working with clusters

cluster.methods <- c("Spin Glass Community", "Random Walk Community")
pagerank.types <- c(0, 1) ## 0 mean regular, 1 means transposed adjacency matrix

cluster.method.valid <- function(cluster.method) {
  return(any(cluster.method %in% cluster.methods))
}

pagerank.type.valid <- function(pr.type) {
  return(any(pr.method %in% pr.type))
}

annotate.cluster <- function(g) {
  V(g)$size <- sqrt(V(g)$rankValue*5000)
  E(g)$width <- sqrt(E(g)$weight)

  ## We store the global properties as attributes of the graph
  ## to eliminate the need for a second data structure

  ## Reciprocity computes the amount of reciprocal connections. The
  ## larger the score, the better is bidirectional connection in a graph
  ## (low score ones tend to be central developer communities with numerous
  ## unconnected contributors)
  g$rec <- round(reciprocity(g, ignore.loops=FALSE), digits=3)

  ## TODO: Document the purpose of graph strength
  g$strength <- mean(graph.strength(g, mode="all"))
  g$strength <- round(g$strength/vcount(g), digits=3)

  ## Select the most important developers (as per page rank)
  prank.sorted <- sort(V(g)$rankValue, index.return=TRUE, decreasing=TRUE)

  ## We compute the degree for the three most important developers
  ## (for large graphs) or of the most important developer for
  ## small graphs.
  ## TODO: The threshold is arbitrary
  if (vcount(g) > 10) {
    vertex.idx <- prank.sorted$ix[1:3]
  } else {
    vertex.idx <- prank.sorted$ix
  }

  g$deg.graph <- round(mean(degree(g, vertex.idx, normalize=TRUE)), digits=3)
  g$size <- vcount(g)

  ## TODO: IN the analysis/the clustering phase, also consider the
  ## page rank distribution and mean page rank.
  return(g)
}
