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
## Copyright 2010, 2011 by Wolfgang Mauerer <wm@linux-kernel.net>
## Copyright 2012, 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Generic helper functions that deal with communities


## Select communities with more than .min members
select.communities.more <- function(.comm, .min) {

	if (class(.comm) == "communities"){
		N <- length(unique(.comm$membership))
		num.members <- sapply(1:(N),
				function(x) { return(length(which(.comm$membership==x))) })
		elems <- which(num.members > .min)
	}
	else if (class(.comm) == "overlapComm"){
		N <- length(.comm$csize)
		elems <- which(sapply(1:(N),
						function(x) { return (length(.comm[[x]]) > .min)  }) == TRUE)
	}

  return(elems)
}


## For large projects like the Linux kernel, the default setting of
## 25 communities necessarily leads to some very large contributions.
## Compute a more reasonable upper bound that gives the algorithm a
## change to detect reasonably small communities
compute.num.spins <- function(g) {
  AVG.SIZE <- 5

  num.spins <- as.integer(vcount(g)/AVG.SIZE)

  if (num.spins < 25) {
    num.spins <- 25
  }

  if (num.spins > 500) {
    num.spins <- 500
  }

  return(num.spins)
}


######
## Community detection algorithms require the input graph to be connected,
## here the graph is decomposed into connected components and the cluster
## algorithm is applied to each connected component, the results are then
## collected into a single membership vector
## Args:
##  g: igraph graph object
##  cluster.algo: clustering algorithm with an interface equivalent to igraph
##                clustering algorithms
## Returns:
##  community: igraph communities object, includes membership vector, size of
##             communities and number of communities
community.detection.disconnected <- function(g, cluster.algo) {
  ## Global community
  membership.global <- c()
  csize.global      <- c()

  ## Find all connected subgraphs
  g.conn.clust    <- clusters(g)
  g.conn.mem      <- g.conn.clust$membership
  g.conn.clust.no <- g.conn.clust$no ## number of clusters

  ## Perform clustering on each connected subgraph and aggregate results
  ## into single data structure
  global.idx.start <- global.idx.end <- 0
  algorithm.name <- "None"
  for (sub.g.id in 1:g.conn.clust.no) {
    ## Get all vertices for subgraph
    sub.g.verts <- which(g.conn.mem == sub.g.id)

    ## Computed connected graph
    g.conn <- induced.subgraph(g, sub.g.verts)

    if (vcount(g.conn) != 1) {
      ## Perform clustering
      g.comm <- cluster.algo(g.conn)
      comm.membership <- g.comm$membership
      algorithm.name  <- g.comm$algorithm
      csize           <- g.comm$csize
    }
    else {
      ## Singleton clusters don't require clustering
      comm.membership <- c(1)
      csize <- c(1)
    }
    comm.no <- length(unique(comm.membership))

    ## Map local cluster index system to global index system
    global.idx.start <- global.idx.end + 1
    global.idx.end   <- global.idx.end + comm.no
    comm.global.map  <- global.idx.start:global.idx.end

    ## Map local membership ids to global ids
    membership.global[sub.g.verts] <- comm.global.map[comm.membership]

    ## compute community sizes
    for (comm.id in 1:comm.no) {
      csize.global[comm.global.map[comm.id]] <- length(which(comm.membership ==
                                                             comm.id))
    }
  }
  ## Calculate other communities class attributes 
  community            <- list(membership=c(), csize=c(), modularity=0, no=0)
  community$membership <- membership.global
  community$no         <- global.idx.end
  community$csize      <- csize.global
  community$algorithm  <- algorithm.name
  class(community)     <- "communities"

  return(community)
 }


spinglass.community.connected <- function(graph, spins=compute.num.spins(graph)) {
	## wrapper for spinglass clustering algorithm

	## Description:
	## 	Spinglass will detect clusters that are disjoint and place them in one
	## 	cluster. Our definition of a community requires a connected graph. This
	## 	wrapper splits a disjoint cluster into multiple communities
	## Args:
	## 	graph: igraph graph object
	## Returns:
	## 	comms.new: an igraph communities object with connected communities
	comms.new <- list(membership=c(), csize=c(), modularity=0, no=0,
			algorithm="spinglass")
	class(comms.new) <- "communities"

	## perform normal spinglass clustering
	comms <- spinglass.community(graph, spins=spins, update.rule="config")
	## construct new communities instance
	comms.new$modularity <- comms$modularity
	## check if any communities are not disjoint
	numComms <- length(comms$csize)
	for (comm.indx in 1:numComms){
		## get vertex set corresponding to the comm.indx-th cluster
		vert.set  <- which(comms$membership == comm.indx)
		g.induced <- induced.subgraph(graph, vert.set)
		clust     <- clusters(g.induced, mode="weak")

		## if more than one cluster is found we need to split into two communities
		for (i in 1:clust$no) {
			comms.new$no <- comms.new$no + 1
			vert.set.sub <- which(clust$membership == i)
			comms.new$membership[vert.set[vert.set.sub]] <- comms.new$no
			comms.new$csize[comms.new$no] <- length(vert.set.sub)
		}
	}
	return(comms.new)
}


minCommGraph <- function(graph, comm, min=10){
	## create a graph and the associated communities object only consisting of
	## vertices that belong to communities large than a minimum size

	## Args:
	## 	graph: igraph graph object
	## 	comm:  igraph communities object
	## 	min: minimum number of vertices that must exist in a community to be
	##      kept
	##
	## Returns:
	## 	res$graph: resulting igraph graph object onces insignificant vertices
	##						are removed
	## 	res$community: resulting igraph communities object after small communities
	##								have been removed
	comm.idx <- which(comm$csize > min)
  if(length(comm.idx) == 0) {
    res <- list(graph=NULL, community=NULL)
  }
  else{
    verts <- rle(unlist(as.vector(sapply(comm.idx,
           function(x) { return(which(comm$membership==x)) }))))$values

	  V(graph)$key <- 1:vcount(graph)
	  graph.comm <- induced.subgraph(graph, verts)
	  ## use the unique key to determine the mapping of community membership to the
	  ## new graph index
	  comm$membership <- comm$membership[V(graph.comm)$key]
	  comm$csize <- sapply(1:length(comm.idx),
			  function(x) {return(length(which(comm$membership == comm.idx[x])))})
	  comm$membership <- remap.consec.seq(comm$membership)
	  res <- list(graph=graph.comm, community=comm)
  }
  return(res)
}


remap.consec.seq <- function(values) {
  ## Map an arbitrary number sequence to increase from 1
  ## For instance, map the sequence 2,2,2,3,3,4,3 to 1,1,1,2,2,3,2

  return(mapvalues(values, unique(values), 1:length(unique(values))))
}


## Select communities with less or equal than .max members
select.communities.less.equal <- function(.comm, .max) {
  N <- length(unique(.comm$membership))
  num.members <- sapply(1:(N),
			function(x) { return(length(which(.comm$membership==x))) })

  elems <- which(num.members <= .max)

  return(elems)
}

## Select communities with size between a certain range inclusive
select.communitiy.size.range <- function(.comm, bound1, bound2) {

  ## Determine which bound is the upper which is the lower
  if (bound1 <= bound2){
    lowBound = bound1
    upBound   = bound2
  }
  else{
    lowBound = bound2
    upBound  = bound1
  }

  ## Locate elements that suit the appropriate range
  N <- length(unique(.comm$membership))
  num.members <- sapply(1:(N),
			function(x) { return(length(which(.comm$membership==x))) })

  elems <- which(num.members <= upBound & num.members >= lowBound)

  return(elems)

}

## Helper function for select.communities below: Determine the largest possible
## community size that can be removed so that min.size contributors remain
select.threshold <- function(cmts, min.size) {
  cut.size <- 0

  for (i in sort(unique(cmts$size))) {
    tmp <- cmts[cmts$size > i,]$size.csum
    if (length(tmp) > 0) {
      remaining <- tmp[length(tmp)]

      if (remaining < min.size) {
        return(cut.size)
      } else {
        cut.size <- i
      }
    }
    else {
      ## The current cut would leave no contributors
      return(cut.size)
    }
  }

  return(cut.size)
}

## Remove small communities, but make sure that the fraction of
## surviving contributors on the reduced set is as least min.fract.
## Optionally, an upper bound on the community size that is deemed appropriate
## to be removed can be specified.
select.communities <- function(comm, min.fract=0.95, upper.bound=NA) {
  ## This function needs to work with community objects generated
  ## by walktrap and spinglass (the former does not produce a vsize
  ## member, so we cannot use it)
  min.size <- round(min.fract*length(comm$membership))

  comm.idx <- sort(unique(comm$membership))
  ## Provide a mapping between community labels and their size
  cmts <- data.frame(id=comm.idx,
                     size=sapply(comm.idx, function(i) {
                       sum(comm$membership==i)}))
  ## ... and sort the communities by size (they are still identifiable
  ## by their id)
  cmts <- cmts[sort(cmts$size, index.return=TRUE, decreasing=TRUE)$ix,]

  cmts$size.csum <- cumsum(cmts$size)

  cut.size <- select.threshold(cmts, min.size)

  if(!is.na(upper.bound)) {
    if (cut.size > upper.bound)
      cut.size <- upper.bound
  }

  return(cmts[cmts$size > cut.size,]$id)
}
