#! /usr/bin/env Rscript
## Analyse the developer connections

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
## Copyright 2012, 2013, Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## All Rights Reserved.
##
## File Description:
## 	Functions are responsible for handling the visualization of network based
##	structures. These function make extensive use of igraph and persons.r
##  to support the heavy manipulation as a precursor to visualization.
suppressPackageStartupMessages(library(Rgraphviz))
suppressPackageStartupMessages(library(colorspace))
suppressPackageStartupMessages(library(Matrix))
suppressPackageStartupMessages(library(lattice))
################################################################################
## Low Level Functions
################################################################################
## Find the index of the most important person in each community according to
## a rank vector
## Args:
##  comm: igraph communities object
##  rank: vector, each entry corresponds to the rank of the node with the index
##        id
## Returns:
##  important.comm.nodes: vector, each entry corresponds to the community with
##                        same index id. The value of each entry is a string
##                        with the name of the most important person for that
##                        given community
important.community.nodes <- function(comm, rank) {
  community.ids <- sort(unique(comm$membership))
  important.nodes <- sapply(community.ids,
      function(comm.id) {
        node.ids.in.comm.id <- which(comm$membership == comm.id)
        node.ids.in.comm.id[which.max(rank[node.ids.in.comm.id])]
      })
  return (important.nodes)
}


## Reduces a graph to single edges between them most important edge in a
## a community according to a rank

## Args:
##  g: igraph graph object
##  comm: igraph communities object
##  rank: vector of values indicating a rank value of each vertex in the graph
##          eg. page rank, betweenness centrality
## Returns:
##  g.min.edges: igraph graph object with only single weighted edges between
##               communities
min.edge.count <- function(g, comm, rank) {
  ## Determine the most single most vertex in a community
  community.idx <- sort(unique(comm$membership))
  important.comm.verts <- important.community.nodes(comm, rank)

  ## Find all edges that cross communities and remove them
  cross.comm.edges     <- E(g)[crossing(comm,g)]
  g.inter.comm.removed <- delete.edges(g, cross.comm.edges)

  ## Find edge weight of combined cross community edges, the multiple edges
  ## between communities are summed by the simplify(..) function
  g.contracted <- simplify(contract.vertices(g, comm$membership))

  ## Assign the edge weight between the important people of each community
  num.comms <- vcount(g.contracted)
  if(ecount(g.contracted) > 0) {
    for (i in 1:num.comms) {
      vert.1 <- important.comm.verts[i]
      for(j in 1:num.comms) {
        vert.2 <- important.comm.verts[j]
        out.edge  <- g.contracted[i, j]
        in.edge   <- g.contracted[j, i]
        if(out.edge != 0) {
          g.inter.comm.removed[vert.1, vert.2] <- out.edge
        }
        if(in.edge != 0) {
          g.inter.comm.removed[vert.2, vert.1] <- in.edge
        }
      }
    }
  }

  return(g.inter.comm.removed)
}


layoutCommunity <- function(g, comm, grid.layout=FALSE) {
  ## calculates a layout that collectes nodes belonging to a community into
  ## a consolodated region

  ## Args:
  ## 	g: igraph graph object
  ## 	comm: igraph communities object
  ## 	grid.layout: flag to layout the communities as a grid or freely
  ## 							organized according to density of edges between communities
  ##							with very large graphs with many communities grid layout
  ##							is necessary to make full use of the given layout space
  ## Returns:
  ## 	layout: a layout matrix of x,y coordinates for each vertex of the graph

  ## collapse communities to single node
  if(!grid.layout) {
    g.contracted      <- simplify(contract.vertices(g,membership(comm)))
    layout.contracted <- layout.fruchterman.reingold(g.contracted)
  }
  g$layout <- matrix(nrow=vcount(g), ncol=2)
  nm <- length(levels(as.factor(comm$membership)))
  gr <- 2
  while(gr^2<nm) {
    gr <- gr+1
  }
  i <- j <- 0

  for(cc in levels(as.factor(comm$membership))) {
    f <- delete.vertices(g,comm$membership!=cc)
    f$layout <- layout.fruchterman.reingold(f)
    if(!grid.layout) {
      i <- i+1
      x <- layout.contracted[i,1]
      y <- layout.contracted[i,2]
      f$layout <- layout.norm(f$layout, x - 1.0, x + 1.0, y - 1.0, y + 1.0)
    }
    else {
      f$layout <- layout.norm(f$layout, i, i + 0.5, j, j + 0.5)
      if(i == gr) {
        i <- 0
        if(j == gr) {
          j <- 0
        }else {
          j <- j + 1
        }
      }else {
        i <- i + 1
      }
    }
    g$layout[comm$membership==cc, ] <- f$layout

  }
  return(g$layout)
}


computeVertCommFrac <- function (graph, comm) {
  ## determine fractional edges to communities to find a fractional participation
  ## across the various communities

  ## Args:
  ## 	graph: igraph graph object
  ## 	comm:  igraph communities object
  ## Returns:
  ## 	verts.frac: List of lists, each entry of the list contains a list which
  ##						 	signifies the percentage of a vertex's participation in the
  ##						 	various communities based on edges. Only the top 4 largest
  ##					   	percentages are captured.
  verts.frac <- list()
  for (i in V(graph)) {
	## Get neighbors of vertex i, mode=all will return both in and out
	## directions, multiple edges are listed multiple times however we want
	## the unique vertex index
	total           <- 0
	comm.frac  <- list()
	vert.neigh   <- neighbors(graph, i, mode='all')
	if (length(vert.neigh) == 0) {
	  key <- toString(comm$membership[i])
	  comm.frac[[key]] <- 1
    }
	else{
      for (j in 1:length(vert.neigh)) {
	    key <- toString(comm$membership[vert.neigh[j]])
        if(length(comm.frac[[key]]) == 0) {
          comm.frac[[key]] <- 0
	    }
        ## get edge weight and sum directions (in weight + out weight)
        edge.weight <- sum(E(graph)[i %--% vert.neigh[j]]$weight)
        comm.frac[[key]] <- comm.frac[[key]] + edge.weight
	    }
	}
    ## only select the top 4 largest community fractions
    comm.top.4 <- comm.frac[sort(unlist(comm.frac),
                  decreasing=TRUE, index.return=TRUE)$ix[1:4]]
    total <- sum(unlist(comm.top.4, use.names=FALSE))
    ## calculate a percentage of participation in the top 4 communities
    verts.frac[[i]] <- llply(comm.top.4,  function(x) x / total)
  }
  return(verts.frac)
}


assignCommCol <- function(graph, comm) {
  ## compute color values for vertex pie chart style visualization

  ## Description:
  ## For each vertex in a graph, the participation across the various
  ## communities is computed. This fractional participation is based on
  ## edges and is visualizaed using pie charts. The pie.frac indicates the
  ## fraction of the pies, pie.color indicates the color for each fractional
  ## pie piece. comm.col notes the color of the community which the vertex
  ## official belongs to according to the clustering.
  ##
  ## Args:
  ## graph: igraph graph object
  ## comm:  igraph communities object
  ##
  ## Returns:
  ## res$comm.col: color mapping from community id to a unique color
  ## res$colors: vector of colors for a vertex's pie chart
  ## res$fracs:  vector of fractions indicating pie partitioning
  ## res$vert.comm.col: vertex official community color
  ##										(according to clustering)
  res <- list(colors=list(), fracs=list(), comm.col=c())
  numComms <- length(comm$csize)
  ## map unique color to each community
  col <- rainbow_hcl(numComms)
  verts.frac <- computeVertCommFrac(graph, comm)

  for (i in 1:vcount(graph)) {
    vert.frac <- verts.frac[[i]]
    pie.color <- col[as.numeric(names(vert.frac))]
    pie.frac   <- unlist(vert.frac, use.names=FALSE)
    res$colors[[i]] <- as.vector(na.omit(pie.color))
    res$fracs[[i]]  <- pie.frac
    res$vert.comm.col[i] <- col[comm$membership[i]]
  }
  for (i in 1:length(comm$csize)) {
    res$comm.col[i] <- col[i]
  }
  return(res)
}


addAlpha <- function(col, alpha=0.5) {
  ## add an alpha channel to hex based color through rgb conversion
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
      function(x)
        rgb(x[1], x[2], x[3], alpha=alpha))
}


mapCommSig2Color <- function(commSig) {
  ## converts the community significance value into a color on a graident map

  ## Args:
  ## 	commSig: a vector of floating point numbers that represents the
  ##					 communities significance according to some metric
  ##					 (eg. conductance)
  ## Returns:
  ## 	sig.color.vec: a vector of values representing the color code for each
  ##						     community
  commSig <- unlist(commSig)
  color.palette  <- colorRampPalette(c("#4DFF4D","#8FFFA9", "#FFFF6B", "#FFFF2E", "#FFB3B3","#FFB3B3"))
  color.gradient <- color.palette(256)
  commSig.scaled <- round(commSig*255) + 1
  sig.color.vec  <- addAlpha(color.gradient[commSig.scaled])
  return(list(value=sig.color.vec, map=color.gradient))
}


showColorMap <- function(colors, description) {
  ## plot a community color map gradient
  count <- length(colors$map)
  m <- matrix(1:count, count, 1)
  image(m, col=colors$map, ylab="", ylim=c(0,0.25),axes=FALSE)
  mtext(description, pos=1, adj=0.5, line=0.5)
  mtext("strong community", pos=2, adj=0, line=0)
  mtext("weak community", pos=4, adj=1, line=0)
}


membership2markGroup <- function(membership) {
  ## converts the membership vector in the community class of igraph

  ## the index of every vertex is collected into a single vector, then vectors
  ## of each community are collected into a list
  groups <- list()
  for (i in 1:length(unique(membership))) {
    groups[[i]] <- which(membership == i)
  }
  return (groups)
}


compute.subgraph.list <- function (g, comm){
  ## Builds a list of subgraphs from a given community membership
  ## Args:
  ##  g: Rgraphviz graph object
  ##	comm: igraph communities object
  ## Returns:
  ##  subgraph.list: list of Rgraphviz subgraph objects
  community.id <- sort(unique(comm$membership))
  subgraphL <- list()
  subgraphL <- lapply(community.id,
    function(x) {
      return(list(graph=subGraph(as.character(which(comm$membership==x)), g)))
	})
	return(subgraphL)
}


format.color.weight <- function(colorL, weightL) {
  ## constructs a vector of strings that contain the color and weight data
  ## for the pie chart style node. The graphviz library demands the following
  ## color format WC:WC:WC:WC where WC = color;weight
  color.weight.list <- mapply(
    function(colors, weights) paste(paste(colors, as.character(weights), sep=";"),
                                                     collapse=":"),
    colorL, weightL, SIMPLIFY=FALSE)

  ## Add ":" to the end of every string, for some reason if the weight is 1
  ## and there is no ":" after, graphviz will not fill the node with color
  color.weight.vec <- paste(unlist(color.weight.list), ":", sep="")

  return(color.weight.vec)
}


################################################################################
## High Level Functions
################################################################################
save.graph.igraph <- function(g, comm, filename, plot.size=7, format="png") {
  ## Generate a community graph structure using pie chart style vertex visualization
  ## Args:
  ##  g: igraph graph object
  ##  comm: igraph communities object
  ##  filename: output filename to store graph image
  ##  plot.size: string that specifies the size in inch of the resulting image
  ##             if not specified the 7x7 inch is used
  ##  format: One of the supported output formats of base graphics
  ## Output:
  ##  igraph plot of the graphs community structure
  cluster.conductance <- community.metric(g, comm, "conductance")
  pie.vertex      <- assignCommCol(g, comm)
  g               <- min.edge.count(g, comm, page.rank(g))
  g               <- simplify(g, remove.multiple=TRUE,remove.loops=TRUE)
  comm.domain.col <- mapCommSig2Color(cluster.conductance)$value
  comm.layout     <- layoutCommunity(g,comm,FALSE)
  edge.width      <- scale.data(log(E(g)$weight),1,3)

  select.graphics.dev <- function(filename, size, format="png")

  plot(g,
       mark.group        = membership2markGroup(comm$membership),
       layout            = comm.layout,
       vertex.size       = 2 ,
       vertex.label      = NA,
       edge.width        = 0.02,
       edge.arrow.size   = 0.05,
       edge.color        = c("black","red")[crossing(comm,g)+1],
       mark.expand       = 2,
       mark.shape        = 1,
       mark.col          = comm.domain.col,
       mark.border       = pie.vertex$comm.col,
       vertex.shape      = "pie",
       vertex.pie        = pie.vertex$fracs,
       vertex.pie.color  = pie.vertex$colors,
       vertex.pie.border = pie.vertex$vert.comm.col,
       vertex.pie.lty    = 0)
  dev.off()
}


save.graph.graphviz <- function(con, pid, range.id, cluster.method, filename,
                                plot.size=7) {
  ## Generate pie graph plot using graphviz package
  ## Args:
  ## 	g: igraph graph object
  ## 	comm: igraph communities object
  ##  filename: output filename to store graph image
  ##  plot.size: string that specifies the size in inches of the resulting image
  ##             if not specified the default 7x7 inches is used
  ## Output:
  ##   saves an SVG image of the graph to the specified filename

  ## Query Database
  ## Graph
  graph.data      <- get.graph.data.local(con, pid, range.id, cluster.method)
  node.global.ids <- graph.data$v.global.ids
  node.label      <- lapply(node.global.ids, function(id)
                       query.person.name(con, id))
  edgelist        <- graph.data$edgelist
  p.id.map        <- graph.data$id.map
  node.local.ids  <- graph.data$v.local.ids
  comm            <- graph.data$comm
  node.rank       <- graph.data$rank

  if (nrow(edgelist) < 1) {
    logwarn(paste("No edges to export into", filename))
    return(NULL)
  }

  ## Create igraph object and perform manipulations
  g <- graph.data.frame(edgelist, directed=TRUE,
                        vertices=data.frame(node.local.ids))
  V(g)$Id <- 1:vcount(g)
  ## remove communities of size 1
  min.comm <- minCommGraph(g, comm, min=2)
  g    <- min.comm$graph
  comm <- min.comm$community
  if (is.null(comm)) {
    ## No valid communities were found
    return(NULL)
  }
  node.rank <- node.rank[V(g)$Id]
  V(g)$name <- 1:vcount(g)
  cluster.conductance <- community.metric(g, comm, "conductance")
  g.min      <- min.edge.count(g, comm, node.rank)
  g.min.simp <- simplify(g.min, remove.multiple=TRUE,remove.loops=TRUE)

  ## Convert to Rgraph object via graph object
  From    <- as.character(get.edgelist(g.min.simp)[,1])
  To      <- as.character(get.edgelist(g.min.simp)[,2])
  edge.df <- data.frame(From=From, To=To)
  weights <- E(g.min.simp)$weight
  g.NEL <- ftM2graphNEL(as.matrix(edge.df), W=weights, V=V(g.min.simp)$name,
                        edgemode="directed")
  subgraph.list <- compute.subgraph.list(g.NEL, comm)
  g.viz <- agopen(g.NEL, "pieGraph", subGList=subgraph.list)

  ## Compute graph node and cluster colors
  pie.vertex  <- assignCommCol(g, comm)
  comm.col    <- mapCommSig2Color(cluster.conductance)$value

  ## Cluster Attributes
  cluster.id <- sort(unique(comm$membership))-1
  ## border thickness
  clusterData(g.viz, cluster.id, "penwidth") <- "75"
  ## background color
  clusterData(g.viz, cluster.id,"bgcolor") <- comm.col
  ## border style
  clusterData(g.viz, cluster.id, "style") <- "bold"
  ## border color
  clusterData(g.viz, cluster.id, "color") <- pie.vertex$comm.col
  ## cluster label
  central.node.idx <- important.community.nodes(comm, node.rank)
  clusterData(g.viz, cluster.id, "label") <- paste("ID ",cluster.id + 1, sep="")
  clusterData(g.viz, cluster.id, "fontsize") <- "120"

  ## Node Attributes
  n.idx <-  as.character(1:vcount(g))
  nodeDataDefaults(g.viz, c("style","shape")) <- c("wedged", "ellipse")
  nodeData(g.viz, n.idx, "label")     <- ""
  ## pie chart color
  nodeData(g.viz, n.idx, "fillcolor") <-
    format.color.weight(pie.vertex$color, pie.vertex$fracs)
  ## node size
  nodeData(g.viz, n.idx, "width") <- as.character(
		  scale.data(node.rank, 0.75, 5))
  nodeData(g.viz, n.idx, "height") <- as.character(
		  scale.data(node.rank, 0.75, 5))
  ## node label
  nodeData(g.viz, n.idx, "label") <- as.character(node.global.ids)

  ## Require at least two edges, otherwise there will be errors down the line
  N <- nrow(edge.df)
  if (N <= 1) {
    logwarn(paste("Only one edge to export into", filename))
    return(NULL)
  }
  rmv <- removedEdges(g.NEL)
  keep.edge        <- c()
  keep.edge[1:N]   <- TRUE
  keep.edge[rmv]   <- FALSE
  edge.weights.viz <- weights[keep.edge]
  inter.comm.edges <- crossing(comm,g.min.simp)[keep.edge]
  from.viz         <- From[keep.edge]
  to.viz           <- To  [keep.edge]
  from.inter.comm  <- from.viz[inter.comm.edges]
  to.inter.comm    <- to.viz  [inter.comm.edges]
  from.intra.comm  <- from.viz[!inter.comm.edges]
  to.intra.comm    <- to.viz  [!inter.comm.edges]
  inter.comm.weights <- edge.weights.viz[inter.comm.edges]
  intra.comm.weights <- edge.weights.viz[!inter.comm.edges]
  ## color inter-community edges different than intra-community edges
  if(any(inter.comm.edges)) {
    edgeData(g.viz, from=from.inter.comm, to=to.inter.comm, "color") <- "red"
    edgeData(g.viz, from=from.inter.comm, to=to.inter.comm, "style") <- "bold"
  }
  ## edge thickness
  edgeDataDefaults(g.viz, "penwidth") <- "1.0"
  ## scale inter-community edges seperately from intra-community edges
  if(any(inter.comm.edges)) {
    edgeData(g.viz ,from=from.inter.comm, to=to.inter.comm, "penwidth") <-
      as.character(scale.data((inter.comm.weights)+1, 5, 25))
    edgeData(g.viz ,from=from.inter.comm, to=to.inter.comm, "arrowsize") <-
		  as.character(scale.data((inter.comm.weights)+1, 0.1, 20) / 3)
  }
  if(any(!inter.comm.edges)){
    ## scale intra-community edges
    edgeData(g.viz, from=from.intra.comm, to=to.intra.comm, "penwidth") <-
             as.character(scale.data(log(intra.comm.weights)+1, 1, 10))
    edgeData(g.viz, from=from.intra.comm, to=to.intra.comm, "arrowsize") <-
             as.character(scale.data(log(intra.comm.weights)+1, 1, 10) / 3)
    edgeData(g.viz, from=from.intra.comm, to=to.intra.comm, "color") <- "#00000040"
  }
  ## edge weights are used in the layout algorithm
  edgeData(g.viz, from=from.viz, to=to.viz, "weight") <-
		  as.character(edge.weights.viz)

  ## Graph Attributes
  graphDataDefaults(g.viz, "size") <- plot.size
  graphDataDefaults(g.viz, "overlap") <- "prism"

  ## Write dot file
  agwrite(g.viz, filename)
}

