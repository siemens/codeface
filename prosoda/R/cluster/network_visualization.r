#! /usr/bin/env Rscript
## Analyse the developer connections

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
## Copyright 2012, 2013, Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## All Rights Reserved.
##
## File Description:
## 	Functions are responsible for handling the visualization of network based
##	structures. These function make extensive use of igraph and persons.r
##  to support the heavy manipulation as a precursor to visualization.
source("persons.r")
################################################################################
## Low Level Functions
################################################################################
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
  for (i in 1:vcount(graph)) {
    vert.neigh <- neighbors(graph, i, mode='all')
    comm.frac  <- list()
    total      <- 0
    for (j in 1:length(vert.neigh)) {
      key <- toString(comm$membership[vert.neigh[j]])
      if(length(comm.frac[[key]]) == 0) {
        comm.frac[[key]] <- 0
      }
      comm.frac[[key]] <- comm.frac[[key]] + 1
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
  col=rainbow(numComms)
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
  color.palette  <- colorRampPalette(c("red", "white", "blue"))
  color.gradient <- color.palette(256)
  commSig.scaled <- round(scale.data(commSig, 1, 256))
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
################################################################################
## High Level Functions
################################################################################
plotPieGraph <- function(g, comm) {
  ## plot a community graph structure using pie chart style vertex visualization

  ## Args:
  ##	g: igraph graph object
  ##	comm: igraph communities object
  ## Returns:
  ## 	igraph plot of the graphs community structure
  cluster.conductance <- compute.all.community.quality(g, comm, "conductance")
  pie.vertex      <- assignCommCol(g, comm)
  g               <- simplify(g, remove.multiple=TRUE,remove.loops=TRUE)
  comm.domain.col <- mapCommSig2Color(cluster.conductance)$value

  plot(g,
       mark.group        = membership2markGroup(comm$membership),
       layout            = layoutCommunity(g,comm,TRUE),
       vertex.size       = 2 ,
       vertex.label      = NA,
       edge.width        = 0.02,
       edge.arrow.size   = 0.05,
       edge.color			   = c("black","red")[crossing(comm,g)+1],
       mark.expand			 = 2,
       mark.shape			   = 1,
       mark.col				   = comm.domain.col,
       mark.border			 = pie.vertex$comm.col,
       vertex.shape		   = "pie",
       vertex.pie        = pie.vertex$fracs,
       vertex.pie.color  = pie.vertex$colors,
       vertex.pie.border = pie.vertex$vert.comm.col,
       vertex.pie.lty    = 0)
}
################################################################################
## Executed
################################################################################