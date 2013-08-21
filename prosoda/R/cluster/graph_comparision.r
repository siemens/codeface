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
graphComparison <- function(adjMatrix1, ids1, adjMatrix2, ids2,
    outputFileName) {
  ## Normalize graphs to have edge weight between 0-1
  nonTagAdjMatrix.weighted <- scale.data(adjMatrix1, 0, 1000)
  tagAdjMatrix.weighted    <- scale.data(adjMatrix2, 0, 1000)
  ## Normalize graphs to have binary edge weight
  nonTagAdjMatrix <- ceiling( scale.data(adjMatrix1, 0, 1) )
  tagAdjMatrix    <- ceiling( scale.data(adjMatrix2, 0, 1) )

  ## Create igraph objects
  g.nonTag <- graph.adjacency(nonTagAdjMatrix, mode="directed")
  g.Tag    <- graph.adjacency(tagAdjMatrix   , mode="directed")

  ## Get largest connected cluster
  idx.nonTag.connected <- largest.subgraph.idx(g.nonTag)
  idx.Tag.connected    <- largest.subgraph.idx(g.Tag   )
  ids.nonTag.connected <- ids1[idx.nonTag.connected,]
  ids.Tag.connected    <- ids2[idx.Tag.connected,]

  nonTagAdj.connected <- nonTagAdjMatrix[idx.nonTag.connected,
      idx.nonTag.connected]
  TagAdj.connected <- tagAdjMatrix[idx.Tag.connected, idx.Tag.connected]

  nonTagAdj.connected.weighted <-
      nonTagAdjMatrix.weighted[idx.nonTag.connected, idx.nonTag.connected]
  tagAdj.connected.weighted <- tagAdjMatrix.weighted[idx.Tag.connected,
      idx.Tag.connected]

  g.nonTag.connected <- graph.adjacency(nonTagAdj.connected,
      mode="directed", weighted=TRUE)
  g.Tag.connected    <- graph.adjacency(TagAdj.connected,
      mode="directed")

  g.nonTag.connected.weighted <- graph.adjacency(nonTagAdj.connected.weighted,
      mode="directed", weighted=TRUE)
  g.Tag.connected.weighted <- graph.adjacency(tagAdj.connected.weighted,
      mode="directed", weighted=TRUE)
  
  ## Compute pagerank
  pr.nonTag <- page.rank(g.nonTag.connected.weighted, damping=0.85,
      directed=TRUE)
  pr.Tag <- page.rank(g.Tag.connected.weighted, damping=0.85, directed=TRUE)
  
  ## Match up ids between the two adjacency matrices
  ## Don't use email address, 1 person with multiple email addresses gets
  ## mapped to a single name except the arbitration rule for which email is
  ## noted doesn't provide consistent results between the tagged and
  ## non-tagged analysis
  intersectNames <- intersect(ids.nonTag.connected$Name, ids.Tag.connected$Name)
  ## Get index of names from both ids sets
  idx.nonTag <- match(intersectNames, ids.nonTag.connected$Name)
  idx.Tag    <- match(intersectNames, ids.Tag.connected$Name   )
  
  
  ## Now the ids indecies should be unified between the
  ## tag and non-tag graphs
  ids.nonTag.intersect <- ids.nonTag.connected[idx.nonTag,]
  pr.nonTag.intersect  <- pr.nonTag[idx.nonTag]
  
  ids.Tag.intersect <- ids.Tag.connected[idx.Tag,]
  pr.Tag.intersect <- pr.Tag[idx.Tag]
  
  if (all( ids.nonTag.intersect$Name == ids.Tag.intersect$Name )) {
    ids.intersect <- ids.nonTag.intersect
    ids.intersect$ID <- seq(1, length(ids.nonTag.intersect$eMail))
    ids.intersect$pr.NonTag <- pr.nonTag$vector[idx.nonTag]
    ids.intersect$pr.Tag <- pr.Tag$vector[idx.Tag]
  } else {
    e <- simpleError("id systems don't match!")
    stop(e)
  }
  
  
  ## Build adjacency matrix of interesecting ids
  nonTagAdj.intersect <- nonTagAdj.connected[idx.nonTag, idx.nonTag]
  tagAdj.intersect <- TagAdj.connected[idx.Tag, idx.Tag]

  ## Build igraph graph objects
  g.nonTag <- graph.adjacency(nonTagAdj.intersect, mode = "directed",
      weighted=TRUE)
  g.Tag <- graph.adjacency(tagAdj.intersect, mode = "directed")

  ##sum(get.adjacency(g.nonTag) != get.adjacency(g.Tag))
  similarity.adjMatrix <- nonTagAdj.intersect * tagAdj.intersect

  ## Take the average weights from the two adjacency matrix to assign
  ## the weight for the graph that agrees between the two
  similarity.adjMatrix.weighted <- (nonTagAdj.connected.weighted[idx.nonTag, idx.nonTag] + tagAdj.connected.weighted [idx.Tag, idx.Tag]) / 2

  #get average weighted agreed upon connections
  similiarityAdjMatrix.weighted <- similarity.adjMatrix * similarity.adjMatrix.weighted

  g.similarity <- graph.adjacency(similarity.adjMatrix.weighted, mode="directed", weighted=TRUE)
  
  graphSim <- graph.similarity(g.Tag, g.nonTag)
  
  ## Plot results/save results
  pdf(outputFileName)
  plot(sort(graphSim))
  plot(graphSim, log(ids.intersect$pr.NonTag))
  plot(graphSim, log(ids.intersect$pr.Tag))
  plot(log(ids.intersect$pr.Tag), log(ids.intersect$pr.NonTag))
  dev.off()
  
  performGraphAnalysis(conf, similarity.adjMatrix.weighted, ids.intersect,
      "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/experiments")
  
  ##write.graph.2.file("/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/experiments/similarityGraph.dot", g.similarity, ids.intersect, ids.intersect$ID)
}