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
# Copyright 2013 by Siemens AG, Mitchell Joblin <johannes.ebke.ext@siemens.com>


## TODO: this should be changed to use the Tanimoto coefficient, a more theoriticaly
##       sound and established similairty measure for weighted graphs
vertex.edge.weight.difference <- function(g1, v1, g2, v2) {
  ##  -- To be used only on weighted graphs --
  ## computes the percent difference between two verteces from two different
  ## graphs. The percent difference is calculated based on the difference
  ## between the weights of common edges divided by the average edge weight
  ## -- Output --
  ## percent.difference: pertage DIFFERENCE for the matching edges
  
  g1.adjMat <- get.adjacency(g1)
  g2.adjMat <- get.adjacency(g2)
  
  in.v1  <- neighbors(g1, v1, mode="in")
  out.v1 <- neighbors(g1, v1, mode="out")
  in.v2  <- neighbors(g2, v2, mode="in")
  out.v2 <- neighbors(g2, v2, mode="out")
  
  in.union  = union(in.v1,in.v2)
  out.union = union(out.v1, out.v2)
  in.inter  = intersect(in.v1, in.v2)
  out.inter = intersect(out.v1,out.v2)
  
  if (is.weighted(g1) && is.weighted(g2)){
    
    out.percent.diff <- 0
    in.percent.diff  <- 0
    
    if (length(out.union) != 0){
      out.diff <- abs(g1.adjMat[v1, out.union] - g2.adjMat[v2, out.union])
      out.avg <- 0.5 * (g1.adjMat[v1, out.union] +
            g2.adjMat[v2, out.union])
      out.percent.diff <- mean(out.diff / out.avg)
    }
    if (length(in.union) != 0){
      in.diff <- abs(g1.adjMat[in.union, v1] - g2.adjMat[in.union, v2])
      in.avg  <- 0.5 * (g1.adjMat[in.union, v1]  +
            g2.adjMat[in.union, v2])
      in.percent.diff  <- mean(in.diff / in.avg)
    }
    percent.difference <- mean(c(in.percent.diff, out.percent.diff))
  }
  else{
    e <- simpleError("difference comparison not possible for unweighted
            graphs")
    stop(e)
  }
  
  return(percent.difference)
}


vertex.neighborhood.difference <- function(g1, v1, g2, v2) {
  ## Calculates the percent similarity using the Jaccard index concept from
  ## set theory. This considers the neighbour hoods of matching
  ## verteces from two different graphs and does not consider the edge weights
  ## rather only the existence of edges.
  ## -- Output --
  ## similarity: a percentage of how SIMILAR the neightboods are
  
  in.v1  <- neighbors(g1, v1, mode="in")
  out.v1 <- neighbors(g1, v1, mode="out")
  in.v2  <- neighbors(g2, v2, mode="in")
  out.v2 <- neighbors(g2, v2, mode="out")
  
  totalEdges = length(union(in.v1,in.v2)) + length(union(out.v1,out.v2))
  matchEdges = length(intersect(in.v1,in.v2)) + length(intersect(out.v1,out.v2))
  
  if (totalEdges != 0) {
    difference = 1 - (matchEdges / totalEdges)
  } else {
    difference = 0
  }
  
  return(difference)
}


graph.difference <- function(g1,g2, weighted=FALSE) {
  ## two graphs on the same vertex set can be compared by considering
  ## the percent at which the two graphs agree on an edge
  
  ## compares to graphs that have match indexing, meaning that vertex 1 in g1
  ## is the same person in as vertex 1 in g2. If the graph is weighted then
  ## a difference is considered by calculating a percent difference on matching
  ## edges. If the graph is not weigthed then only the existance of edges
  ## is used to calculated how different the two graphs are.
  
  vertexList1 <- V(g1)
  vertexList2 <- V(g2)
  
  if (!all(vertexList1 == vertexList2)) {
    e <- simpleError("graphs not compatible!")
    stop(e)
  } else {
    vertexList <- vertexList1
  }
  
  vert.diff <- numeric(length(vertexList))
  for (v in vertexList) {
    if (weighted){
      vert.diff[v] <- vertex.edge.weight.difference(g1, v, g2, v)
    } else {
      vert.diff[v] <- vertex.neighborhood.difference(g1, v, g2, v)
    }
  }
  return(vert.diff)
}


## Compare the results of the tag and non tag based graphs
graph.comparison <- function(g.1, g.2) {
  ## Normalize graphs to have binary edge weight
  E(g.1)$weight <- ceiling( scale.data(E(g.1)$weight, 0, 1) )
  E(g.2)$weight <- ceiling( scale.data(E(g.2)$weight, 0, 1) )

  intersectNames <- intersect(V(g.1)$Id, V(g.2)$Id)
  idx.1 <- match(intersectNames, V(g.1)$Id)
  idx.2 <- match(intersectNames, V(g.2)$Id)

  ## Build adjacency matrix of interesecting ids
  adj.matrix.1.intersect <- g.1[idx.1, idx.1]
  adj.matrix.2.intersect <- g.2[idx.2, idx.2]

  ## Build igraph graph objects
  g.1.intersect <- graph.adjacency(adj.matrix.1.intersect, mode = "directed")
  g.2.intersect <- graph.adjacency(adj.matrix.2.intersect, mode = "directed")

  graph.diff <- graph.difference(g.1.intersect, g.2.intersect)

  return(graph.diff)
}

get.index.map1 <- function(ids) {
  N        <- nrow(ids)
  map      <- new.env(size=N)
  for(i in 1:N) {
    map[[as.character(ids$id1[i])]] <- ids$id[i]
  }
  return(map)
}

get.index.map2 <- function(ids) {
  N        <- nrow(ids)
  map      <- new.env(size=N)
  for(i in 1:N) {
    map[[as.character(ids$id2[i])]] <- ids$id[i]
  }
  return(map)
}
################################################################################
## High Level Functions
################################################################################
run.graph.comparison <- function(con, pid.1, range.id.1, pid.2, range.id.2) {
  ## TODO: cluster method should not actually be required, we only need the main
  ##       graph, unfortunatley the database scheme currently marries the graph
  ##       to a clustering method
  cluster.method="Spin Glass Community"
  graph.data.1 <- get.graph.data.local(con, pid.1, range.id.1, cluster.method)
  graph.data.2 <- get.graph.data.local(con, pid.2, range.id.2, cluster.method)
  # get the raw edgelists with the ids from the database
  edgelist.1   <- graph.data.1$edgelist.db
  edgelist.2   <- graph.data.2$edgelist.db

  # Get the database ids and resolve them to names
  global.ids.1 <- graph.data.1$v.global.ids
  node.label.1 <- sapply(global.ids.1, function(id)
        query.person.name(con, id))
  global.ids.2 <- graph.data.2$v.global.ids
  node.label.2 <- sapply(global.ids.2, function(id)
        query.person.name(con, id))

  # Create two dataframes to be merged by developer name
  temp.vertex.df.1 <- data.frame(id1=global.ids.1, name=node.label.1)
  temp.vertex.df.2 <- data.frame(id2=global.ids.2, name=node.label.2)

  # Merge the graphs by developers and create a mapping to local ids
  temp.merged <- merge (temp.vertex.df.1, temp.vertex.df.2, all=T, by="name")
  merged.vertex <- data.frame(id=1:nrow(temp.merged), name=temp.merged$name, id1=temp.merged$id1, id2=temp.merged$id2)
  map1 <- get.index.map1(merged.vertex)
  map2 <- get.index.map2(merged.vertex)

  # Use the mappings to create a local edge and vertex list for graph1.
  local.ids.1 <- map.ids(global.ids.1, map1)
  edgelist.local.1 <- data.frame(from=map.ids(edgelist.1$fromId, map1),
                         to=map.ids(edgelist.1$toId, map1),
                         weight=edgelist.1$weight)
  vertex.df.1 <- data.frame(id1=local.ids.1, name=node.label.1)

  # Use the mappings to create a local edge and vertex list for graph2.
  local.ids.2 <- map.ids(global.ids.2, map2)
  edgelist.local.2 <- data.frame(from=map.ids(edgelist.2$fromId, map2),
                                 to=map.ids(edgelist.2$toId, map2),
                                 weight=edgelist.2$weight)
  vertex.df.2 <- data.frame(id2=local.ids.2, name=node.label.2)

  ## create graph instances and run the graph comparison.
  g.1 <- graph.data.frame(edgelist.local.1, vertices=vertex.df.1, directed=TRUE)
  g.2 <- graph.data.frame(edgelist.local.2, vertices=vertex.df.2, directed=TRUE)
  V(g.1)$Id <- vertex.df.1$name
  V(g.2)$Id <- vertex.df.2$name

  res <- graph.comparison(g.1, g.2)
  return(res)
}