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
## Copyright 2010, 2011 by Wolfgang Mauerer <wm@linux-kernel.net>
## Copyright 2012, 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## TODO: Can we deploy the page rank idea to patches as well?
## For instance, we could interpret that patches are "touched" by
## files, so patches involving more heaviliy patched files would
## receive higher scores. Or we could also determine how many
## people touched a particular piece of code (to simplify things,
## this computation could be done on the file level in a first stage),
## and use this as basis for the patch evaluation.
## NOTE: To compare the results to traditional metric, we could
## also use anonymised names to not insult anyone.
## TODO: Could we use graph cohesion as a more mature (and
## mathematically sound) replacement to OOP cohesion?
## TODO: Another idea is also categorisation by subsystems.
##       (i.e., use a known/suspected distribution into subsystems
##        for learning, and then apply the inferred clustering on
##        the complete data set to infer subsystems in a quasi-Bayesian
##        way)
library(igraph)
library(stringr)
library(xtable)
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(optparse))
source("utils.r")
source("config.r")
source("db.r")


#######################################################################
##		Lower Level Functions
#######################################################################

##========================================================================
##		Utility 
##========================================================================

## Given an eMail address like "Name N. Surname <name.surname@domain.com>",
## extract the person name without the electronic address
name.of.email <- function(str) {
  return(str_sub(str, 1, str_locate(str, "<")[1]-2))
}

## NOTE: We rely that the ids are already sorted numerically. To ensure
## this is really the case, a check could do no harm
ID.to.name <- function(.iddb, .id) {
  return(.iddb[which(.iddb$ID==.id),]$Name)
}

IDs.to.names <- function(.iddb, .ids) {
  return(sapply(.ids,
                function(.id) { .iddb[which(.iddb$ID==.id),]$Name }))
}

name.to.ID <- function(.iddb, name) {
  return(which(.iddb$Name==name))
}

rotate.label <- function(x) {
  return(paste("\\rotatebox{60}{", x, "}", sep=""))
}

rotate.label.30 <- function(x) {
  return(paste("\\rotatebox{30}{", x, "}", sep=""))
}

txt.comm.subsys <- function(.comm, .id.subsys, i) {
  idx <- which(.comm$membership==i)

  ##  return(summary(.id.subsys[idx,2:dim(.id.subsys)[2]]))
  res <- apply(.id.subsys[idx,2:dim(.id.subsys)[2]], 2, fivenum)
  rownames(res) <- c("lw", "lh", "med", "uh", "uw")
  return(res)
}

get.rank.by.field <- function(.iddb, .field, N=dim(.iddb)[1]) {
  res <- .iddb[c("ID", "Name", .field)]
  res <- res[order(res[c(.field)], decreasing=T),]
  s <- sum(res[,3])
  res <- cbind(res, data.frame(percent=res[,3]/s*100))
  res <- cbind(res, data.frame(norm=scale.data(res[,3], 0, 1)))
  
  return(res[1:N,])
}

int.to.hex <- function(n, fill=2) {
  loc <- n
  class(loc) <- "hexmode"
  loc <- as.character(loc)
  if (nchar(loc) != fill) {
    loc <- paste(rep("0", fill - nchar(loc)), loc, sep="") 
  }
  return(as.character(loc))
}

three.digit <- function(n) {
  loc <- as.character(n)
  
  if (nchar(loc) != 3) {
    loc <- paste(paste(rep("0", 3-nchar(loc)), collapse=''), loc, sep="")
  }
  
  return(loc)
}

col.to.hex <- function(prefix="0x", r,g,b) {
  return (paste(prefix, int.to.hex(r), int.to.hex(g), int.to.hex(b), sep=""))
}


largestConnectedSubgraphIndices <- function(graph) {
  ## Returns the indecies of vertices that are in the largest connected subgraph
  
  ## Find all connected subgraphs
  g.clust <- clusters(graph)
  # Get index of the largest connected cluster 
  largestClustMembership = which(g.clust$csize == max(g.clust$csize))
  ## Get all indecies of connected developers for the largest cluster
  idx <- which(g.clust$membership==largestClustMembership) 
  
  return(idx)
} 


largestConnectedSubgraph <- function(graph) {
  ##############################################################################
  ## Returns a graph composed only of the largest connected component
  ## - Input - 
  ## graph: igraph object
  ## - Ouput -
  ## graph.connected: igraph object, composed of the largest connected component
  ##                  provided by the input graph
  ##############################################################################
  ## find all connected subgraphs
  g.clust <- clusters(graph)
  ## get index of the largest connected cluster 
  largestClustMembership = which(g.clust$csize == max(g.clust$csize))
  ## get all indecies of not connected to the largest cluster
  idx <- which(g.clust$membership!=largestClustMembership) 
  ## remove the vertices not in the largest connected component
  graph.connected <- delete.vertices(graph,idx)
		
  return(graph.connected)
}
##========================================================================
##    						Edge Functons
##========================================================================


## These two function return the absolute number of tags received and given.
## If two persons exchange tags multiple times, this is counted repeatedly
tags.received <- function(.id, .tags) {
  return(sum(.tags[,.id]))
}

tags.given <- function(.id, .tags) {
  return(sum(.tags[.id,]))
}

## These functions, in turn, compute from/to how many _different_ developers
## a tag was given to/received from
tags.received.norep <- function(.id, .tags) {
  return(length(which(.tags[,.id]>0)))
}

tags.given.norep <- function(.id, .tags) {
  return(length(which(.tags[.id,]>0)))
}



##========================================================================
##		Community Detection
##========================================================================
## Clique percolation. Stolen from http://igraph.wikidot.com/community-detection-in-
## Does not work on our graph. Maybe it works in an undirected version
clique.community <- function(graph, k) {
  clq <- cliques(graph, min=k, max=k)
  edges <- c()
  for (i in seq_along(clq)) {
    for (j in seq_along(clq)) {
      if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
        edges <- c(edges, c(i,j)-1)
      }
    }
  }
  clq.graph <- simplify(graph(edges))
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  comps <- decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[ V(x)$name ]))
  })
}

## Was not particularly useful on some test graphs (essentially, the graph
## is split into two halves, with some very small pieces in addition))
largeScaleCommunity <- function(g,mode="all"){
  V(g)$group <- as.character(V(g))
  thisOrder <- sample(vcount(g),vcount(g))-1
  t <- 0
  done <- FALSE
  while(!done){
    t <- t+1
    cat("\rtick:",t)
    done <- TRUE ## change to FALSE whenever a node changes groups              
    for(i in thisOrder){
      ## get the neighbor group frequencies:                                    
      groupFreq <- table(V(g)[nei(i,mode=mode)]$group)
      ## pick one of the most frequent:                                         
      newGroup <- sample(names(groupFreq) [groupFreq==max(groupFreq)],1)
      if(done){done <- newGroup==V(g)[i]$group}
      V(g)[i]$group <- newGroup
    }
  }
  ## now fix any distinct groups with same labels:                              
  for(i in unique(V(g)$group)){
    ## only bother for connected groups                                         
    if(!is.connected(subgraph(g,V(g)[group==i]))){
      theseNodes <- V(g)[group==i]
      theseClusters <- clusters(subgraph(g,theseNodes))
      ## iterate through the clusters and append their names                    
      for(j in unique(theseClusters$membership)){
        V(g)[theseNodes[theseClusters$membership==j]]$group <- paste(i,j,sep=".")
      }
    }
  }
  return(g)
}


## Select communities with more than .min members
select.communities.more <- function(.comm, .min) {
  N <- length(unique(.comm$membership))
  num.members <- sapply(1:(N),
			function(x) { return(length(which(.comm$membership==x))) })
  
  elems <- which(num.members > .min)
  
  return(elems)
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


## Summarise in which subsystems the authors of a given community are active
comm.subsys <- function(.comm, .id.subsys, N) {
  idx <- which(.comm$membership==N)
  suppressMessages(molten <- melt(.id.subsys[idx,2:dim(.id.subsys)[2]]))
  colnames(molten) <- c("Subsystem", "Fraction")
  
  return(data.frame(Community=N, molten))
}



## This function allows to do a sanity check for the plot
##N <- 3
##summary(id.subsys.connected[which(g.spin.community$membership==N), 2:dim(id.subsys.connected)[2]])
plot.comm.subsys <- function(.comm, .id.subsys, filename, .alg,
                             elems=1:(length(unique(.comm$membership))),
                             .height=8, .width=14) {
  comb <- vector("list", length(elems))
  for (i in 1:length(elems)) {
    comb[[i]] <- comm.subsys(.comm, .id.subsys, elems[i])
  }
  
  comb <- do.call(rbind, comb)
  
  ggsave(filename, 
         ggplot(comb, aes(Subsystem, Fraction)) + geom_boxplot(outlier.colour="blue",
                                                               outlier.size=1.5, alpha=0.5) +
         facet_wrap(~Community) + geom_jitter(size=1, alpha=0.5) +
         opts(axis.text.x=theme_text(angle=-90, hjust=0),
              title=paste("Subsystem distribution for community clusters (algorithm: ", .alg,
                ")", sep="")),
         height=.height, width=.width)
}



## TODO: Dunno if that tells us something. Most likely not.
##densityplot(~pranks|group, data=construct.pr.info(g.spin.community, pr.for.all),
##            plot.points="rug")

## NOTE: When raw IDs are used as labels, they are zero-based, not
## 1-based as in the ID mapping table
## NOTE: This works for both, communities derived from walktrap and
## spinglass.
plot.group <- function(N, .tags, .iddb, .comm) {
  s <- which(.comm$membership==N)
  g <- graph.adjacency(.tags[s,s], mode="directed")
  V(g)$name <- IDs.to.names(.iddb, V(g)$name)
  plot(g, vertex.label=IDs.to.names(.iddb, s))
}


save.group <- function(.tags, .iddb, idx, .prank, .filename=NULL, label=NA) {
  g <- graph.adjacency(.tags[idx,idx], mode="directed")
  ## as.character is important. The igraph C export routines bark
  ## otherwise (not sure what the actual issue is)
  ## NOTE: V(g)$name as label index does NOT work because the name attribute
  ## is _not_ stable.
  V(g)$label <- as.character(IDs.to.names(.iddb, idx))
  
  ## We also use the page rank to specify the font size of the vertex
  V(g)$fontsize <- scale.data(.prank$vector, 15, 50)[idx]
  
  ## The amount of changed lines is visualised by the nodes background colour:
  ## The darker, the more changes.
  fc <- as.character(as.integer(100-scale.data(log(.iddb$total+1),0,50)[idx]))
  V(g)$fillcolor <- paste("grey", fc, sep="")
  V(g)$style="filled"
  
  ## And one more bit: The width of the bounding box changes from thin
  ## to thick with the number of commits
  V(g)$penwidth <- as.character(scale.data(log(.iddb$numcommits+1),1,5)[idx])
  
  if(!is.na(label)) {
    g$label <- label
    g$fontsize <- 30
  }
  
  if (!is.null(.filename)) {
    write.graph(g, .filename, format="dot")
  }
  
  return(g)
}

## save.group.fn is the function responsible for saving a single group.
## Can either be save.group or save.group.NonTag
save.groups <- function(.tags, .iddb, .comm, .prank, .basedir, .prefix, .which,
                        save.group.fn, label=NA) {
  baselabel <- label
  for (i in .which) {
    filename <- paste(.basedir, "/", .prefix, "group_", three.digit(i), ".dot", sep="")
    ##		status(paste("Saving", filename))
    idx <- as.vector(which(.comm$membership==i))
    if (!is.na(baselabel)) {
      label <- paste(baselabel, i, sep=" ")
    }
    save.group.fn(.tags, .iddb, idx, .prank, filename, label)
  }
}


## Determine which persons are in a community detection algorithm
## determined sub-community N
persons.in.group <- function(N, .comm, .iddb) {
  idlist <- which(.comm$membership==N)
  return(data.frame(ID=idlist, name=IDs.to.names(.iddb, idlist)))
}

##print("Persons in community a as determined by the spinglass algorithm")
##print(persons.in.group(7, g.spin.community, ids.connected))

## Determine the page rank factors of persons in community N,
## and merge them with the metrics information
pr.in.group <- function(N, .comm, .iddb, .pr) {
  ## Select all IDs that are contained in cluster N. Then, select all
  ## the page ranks, and combine them with the traditional metrics
  idx <- .iddb$ID %in% which(.comm$membership==N)
  return(cbind(.iddb[idx,], group=N, prank=.pr$vector[idx],
               num.members=length(which(.comm$membership==N))))
}

## Collect page rank and developer metrics for each cluster
construct.group.info <- function(.comm, .pr, .iddb, .elems) {
  res <- vector("list", length(.elems))

  ## Clusters can be empty, skip processing in this case
  if (length(.elems) == 0)
    return (res)

  for (i in 1:length(.elems)){
    res[[i]] <- pr.in.group(.elems[[i]], .comm, .iddb, .pr)
  }

  res <- do.call(rbind, res)
  
  res$group=as.factor(res$group)
  return (res)
}

save.cluster.stats.subsys <- function(.comm, .id.subsys, .elems,
                                      .outdir, .basename) {
  for (i in .elems) {
    print(xtable(txt.comm.subsys(.comm, .id.subsys, i)), type="latex",
          floating=FALSE, file=paste(.outdir, "/", .basename, three.digit(i), ".tex", sep=""))
  }
}

## .comm is the decomposition into clusters
## .iddb is the id-to-name database
## .elems contains the cluster identifiers we're interested in
## .pr contains the page ranks of the developers
## Save information about all clusters.
save.cluster.stats <- function(.comm, .iddb, .elems, .pr, .outdir, .basename) {
  dat <- construct.group.info(.comm, .pr, .iddb, .elems)
  write.table(dat, file=paste(.outdir, "/", .basename, "stats.txt", sep=""),
              sep="\t")
}

## save.group.fn can either be save.group or save.group.NonTag
save.all <- function(.tags, .iddb, .prank, .comm, save.group.fn, .filename=NULL,
                     label=NA) {
  g.all <- save.group.fn(.tags, .iddb, .iddb$ID, .prank, .filename=NULL)
  V(g.all)$label <- .iddb$ID
  V(g.all)$pencolor <- V(g.all)$fillcolor
  
  elems <- select.communities.more(.comm, 10) # Communities with at least 11 members
  red <- as.integer(scale.data(0:(length(elems)+1), 0, 255))
  ##  grey <- as.integer(scale.data(0:(length(elems)+1), 0, 99))
  for (i in elems) {
    idx <- as.vector(which(.comm$membership==i))
    V(g.all)[idx]$fillcolor <- col.to.hex("#", red[i+1], 0, 0)
  }
  
  if (!is.na(label)) {
    g.all$label = label
  }
  
  if (!is.null(.filename)) {
    write.graph(g.all, .filename, format="dot")
  }
  
  return(g.all)
}


## TODO: Investigate the results of inner.links and outer.links (maybe compute
## averages and min/max for all elements of a given community to see how
## well "closed" the communities are)
## TODO: Together with a subsystem distribution of the authors, this should be
## a good basis for a nice ggplot graph.
compute.community.links <- function(g, .comm, N) {
  ## TODO: Continue working here. Compute averages for inner.link and
  ## outer.link over all vertices of community N
  idx <- which(.comm$membership==N)
  function(i) {
    subspin <- spinglass.community(g.connected, vertex=V(g)[idx[i]])
    return(c(subspin$inner.links, subspin$outer.links))
  }
}

##========================================================================
##     				 Community Significance 
##========================================================================
community.quality.modularization <- function(graph, community.vertices, membership.vec){
##############################################
  ## Based on the idea of modularization
  ## Reference: Social network clustering and visualization using hierarchical
  ##			edge bundles (Jia, Garland, Hart)
###############################################
  ## Get graph composed solely of vertices belonging to the community
  subgraph <- induced.subgraph(graph, community.vertices)
  
  ## Get all vertices not belonging to the community 
  not.community.vertices <- setdiff(V(graph), community.vertices)
  
  ## Measure the degree for intra-community edges
  intra.degree.sum <- sum(degree(subgraph))
  
  ## Get number of vertices within the subgraphs
  cluster.vcount     <- vcount(subgraph)
  ##not.cluster.vcount <- length(not.community.vertices)
  
  ## Degree of vertices in community
  community.vertices.degree <- degree(graph, community.vertices) 
  
  ## Degree of vertices external to community
  community.vertices.degree.sum <- sum( degree(graph, community.vertices) )
  
  extern.edges.sum <- community.vertices.degree.sum - intra.degree.sum 	
  
  ## Find membership of other vertices
  other.clusters <- membership.vec[not.community.vertices]
  
  cluster.id <- unique(other.clusters)
  
  cluster.size <- sapply(cluster.id,
                         function(x) {return(length(which(x == other.clusters)))})
  
  edges.count <- sapply(cluster.id,
                        function(x){return( sum(graph[community.vertices,which(x == membership.vec)]) + sum(graph[which(x == membership.vec),community.vertices]) )})
  
  edge.count.div <- edges.count / (2*(cluster.size*cluster.vcount))		
  ## Divide by two to get the value of the intra edges, 
  ## degree will count edges twice, once for each node connected 
  ##  by the edge
  intra.edges.sum <- intra.degree.sum / 2  
  
  ## Get number of clusters
  num.of.clusters <- length(unique(membership.vec))
  
  f.1 <- (1/num.of.clusters) * intra.edges.sum / (cluster.vcount)^2
  f.2 <- (1/( ((num.of.clusters)*(num.of.clusters-1)) / 2)) * (sum(edge.count.div) )
  
  return (f.1 - f.2)
  
}




community.quality.conductance <- function(graph, community.vertices){
#######################################
  ## Finds the conductance of a cluster
#######################################
  ## Get graph composed solely of vertices belonging to the community
  subgraph <- induced.subgraph(graph, community.vertices)
  
  ## Get all vertices not belonging to the community 
  not.community.vertices <- setdiff(V(graph), community.vertices)
  
  ## Measure the degree for intra-community edges
  intra.degree <- degree(subgraph)
  
  ## Degree of vertices in community
  community.vertices.degree <- degree(graph, community.vertices) 

  ## Degree of vertices external to community
  not.community.vertices.degree <- degree(graph, not.community.vertices)
  
  ## Sum all degrees from vertices
  community.vertices.degree.total     <- sum(community.vertices.degree)
  intra.degree.total                  <- sum(intra.degree)
  not.community.vertices.degree.total <- sum(not.community.vertices.degree) 
  
  ## Sum of edges linking vertices where one vertex is in the cluster 
  ## and the second vertex is not in the cluster 
  ## measure the degree for inter-community edges 
  f.1 <- community.vertices.degree.total - intra.degree.total 	
  
                                        # Mminium of sum of edges in cluster or rest of graph 
  ## f.2 <- min(community.vertices.degree.total, not.community.vertices.degree.total) / 2
  f.2 <- intra.degree.total / 2
  
  ## in some cases the community can be isolated
  if (f.2 == 0){
  	return(NaN)
  }
  
  return(f.1/f.2)
}

community.quality.wilcox <- function (graph, community.vertices){
  
  ## Get graph composed solely of vertices belonging to the community
  subgraph <- induced.subgraph(graph, community.vertices)
  
  ## Measure the degree for intra-community edges
  intra.degree <- degree(subgraph)
  
  ## Degree of vertices in community
  community.vertices.degree <- degree(graph, community.vertices) 
  
  ## Measure the degree for inter-community edges 
  inter.degree <- community.vertices.degree - intra.degree 
  
  ## significance <- wilcox.test(jitter(intra.degree), jitter(inter.degree))
  
                                        #return(significance$p.value)
  sig <- (sum(intra.degree) / (sum(inter.degree) + sum(intra.degree)))
  return(sig)
}




community.quality.modularity <- function(graph, community.vertices){
######################################################################
  ## Measure the based on modularity calculation 
  ## quality of the a community indicated by a set of vertices
  ## that belong to the community in comparison to the rest of the graph
  ## Reference: 
  ## "On Modularity Clustering," Knowledge and Data Engineering, 
  ## IEEE Transactions on , vol.20, no.2, pp.172-188, Feb. 2008
######################################################################
  
  ## Get graph composed solely of vertices belonging to the community
  subgraph <- induced.subgraph(graph, community.vertices)
  
  ## Number of edges interal to community
  community.num.edges = ecount(subgraph)
  ## Total number of edge is graph 
  m = ecount(graph)
  
  ## Measure the degree for intra-community edges
  intra.degree <- degree(subgraph)
  
  ## Degree of vertices in community
  community.vertices.degree <- degree(graph, community.vertices) 
  
  ## Measure the degree for inter-community edges 
  inter.degree <- community.vertices.degree - intra.degree 
  
                                        # Calculate final result
  f.1 <- community.num.edges / m
  f.2 <- (sum(community.vertices.degree) / (2*m) )^2
  quality  <- f.1 - f.2
  
  ##f.1 <- sum(intra.degree)
  ##f.2 <- (sum(community.vertices.degree) / (2*m))^2
  ##quality <- (f.1 - f.1*f.2) / ((f.1 - f.1*f.2) + sum(inter.degree))
  
  return( quality )
  
}

communityStatSignificance <- function(graph, cluster.algo){
  ############################################################################
  ## Computer the statistical significance of community structure for a given 
  ## graph. The graph structure is compared to the community structure
  ## probability density of the randomized version of the graph. The
  ## randomization procedure maintains the degree distributions of the original
  ## graph. 
  ## - Input -
  ## graph: original graph for which we would like to find the statistical
  ##        significance of community structure
  ## cluster.algo: clustering algorithm used to find communities
  ## - Output -
  ## p-value: the result of the statistical significance test
  ############################################################################
  ## extract largest connected component
  graph.connected   <- largestConnectedSubgraph(graph)
  ## extract clusters
  graph.clusters <- cluster.algo(graph.connected)
  ## save communities that have more than 10 vertices
  #graph.clusters.more <- select.communities.more(graph.clusters, 10)
  
  ## compute cluster conductance values 
  cluster.conductance <- compute.all.community.quality(graph.connected, 
  		                 graph.clusters, "conductance")
  ## compute randomized conductance samples
  niter <- 1000
  rand.samps <- randomizedConductanceSamples(graph, niter, cluster.algo)
  
  ## test for normality
  normality.test <- shapiro.test(rand.samps)
 
  ## compute normal distribution
  mean.conductance <- mean(rand.samps)
  sd.conductance   <- sd  (rand.samps)
    
  ## perform t-test on test statistic
  t.test.result <- t.test(rand.samps, cluster.conductance)
}

randomizedConductanceSamples <- function(graph, niter, cluster.algo) {
	############################################################################
	## Randomize a given graph while maintaining the degree distribution using
	## a rewiring concept. For each randomized graph a decomposition is performed
	## and the conductance is measured for each trial and is saved.
	## - Input -
	## graph: an igraph object that is to be randomized
	## niter: the number of iterations in randomizing and measuring conductance
	## cluster.algo: clustering algorithm used to find communities
	## - Ouput - 
	## conduct.vec: the conductance for each trial
	############################################################################

	# check if loops exist in the original graph, this information is necessary 
	# tochoose the appropriate rewiring strategy
	loops.exist <- any(is.loop(graph))  
	if (loops.exist) {
		rewire.mode = "loops"
	}
	else {
		rewire.mode = "simple"
	}
	
	## perform iterations
	conduct.vec <- vector()
	pb <- txtProgressBar(min = 0, max = niter, style = 3)
	for (i in 1:niter) {
	  ## update progress bar
	  setTxtProgressBar(pb, i)
	  
	  ## rewire graph, randomize the graph while maintaining the degree distribution
	  rw.graph <- rewire(graph, mode = rewire.mode, niter = 100)
	  rw.graph.connected <- largestConnectedSubgraph(rw.graph)
	  
	  ## find clusters
	  rw.graph.clusters <- cluster.algo(rw.graph.connected)
	  
	  ## only analyze clusters that are large than 10 vertices
	  #rw.graph.clusters.more <- select.communities.more(rw.graph.clusters, 10)
	  
	  ## compute conductance
	  rw.cluster.conductance <- compute.all.community.quality(
			rw.graph.connected, rw.graph.clusters, "conductance")
	  conduct.vec <- append(conduct.vec, mean(rw.cluster.conductance))
	}
	## close progress bar
	close(pb)
	
	return(conduct.vec)
}


##========================================================================
##     						Page Rank 
##========================================================================
compute.pagerank <- function(.tags, .damping=0.85, transpose=FALSE, weights=NULL) {
  if (transpose) {
    g <- graph.adjacency(t(.tags), mode="directed", weighted=weights)
  } else {
    g <- graph.adjacency(.tags, mode="directed", weighted=weights)
  }
  ranks <- page.rank(g, directed=TRUE, damping=.damping)
  
  return(ranks)
}




## Determine the N most important developers (as per the
## PageRank measure). This returns a list ordered by pagerank.
## (Note that the raw pagerank data are given by compute.pagerank()
## give the ranks ordered by ID)
influential.developers <- function(N, .ranks, .tags, .iddb) {
  if (is.na(N)) {
    N <- length(.ranks$vector)
  }
  
  idx = order(.ranks$vector, decreasing=TRUE)
  idlst = seq(1,length(.ranks$vector))[idx[1:N]]
  
  res = data.frame(name=as.character(IDs.to.names(.iddb, idlst)), ID=idlst,
    TagsGiven=sapply(idlst, function(.id) { tags.given(.id, .tags)}),
    TagsReceived=sapply(idlst, function(.id) { tags.received(.id, .tags)}),
    TagsGivenNoRep=sapply(idlst, function(.id) { tags.given.norep(.id, .tags)}),
    TagsReceiveNoRep=sapply(idlst, function(.id) { tags.received.norep(.id, .tags)}),
    rank = .ranks$vector[idx[1:N]])
  
  return(res)
}

writePageRankData <- function(outdir, devs.by.pr, devs.by.pr.tr){
  
  ##print("Top 20 page, rank (focus on giving tags)")
  write.table(devs.by.pr[1:20,], file=paste(outdir, "/top20.pr.txt", sep=""), sep="\t",
              quote=FALSE)
  print(xtable(devs.by.pr[1:20,]), type="latex", floating=FALSE,
        file=paste(outdir, "/top20.pr.tex", sep=""), sanitize.colnames.function=rotate.label.30)
  ##print("Top 20 page rank (focus on being tagged)")
  write.table(devs.by.pr.tr[1:20,], file=paste(outdir, "/top20.pr.tr.txt", sep=""), sep="\t",
              quote=FALSE)
  print(xtable(devs.by.pr.tr[1:20,]), type="latex", floating=FALSE,
        file=paste(outdir, "/top20.pr.tr.tex", sep=""), sanitize.colnames.function=rotate.label.30)
  
}

#########################################################################
##     					 Main Functions
#########################################################################

performAnalysis <- function(outdir, conf) {
  ################## Process the data #################
  status("Reading files")
  adjMatrix <- read.table(file=paste(outdir, "/adjacencyMatrix.txt", sep=""),
                     sep="\t", header=FALSE)
  colnames(adjMatrix) <- rownames(adjMatrix)
  
  ## The adjacency matrix file format uses a different convention for edge
  ## direction than GNU R, so we need to transpose the matrix
  adjMatrix <- t(adjMatrix)
  
  ids <- read.csv(file=paste(outdir, "/ids.txt", sep=""),
                  sep="\t", header=TRUE)
  
  ## IDs are zero-based, but everything in R is 1-based, so simplify
  ## things by adapting the IDs...
  ids$ID <- ids$ID + 1
  
  id.subsys <- read.csv(file=paste(outdir, "/id_subsys.txt", sep=""),
			sep="\t", header=TRUE)
  id.subsys$ID <- id.subsys$ID + 1

  ## If there are only two column names (ID and general), then
  ## the project is not equipped with an explicit subsystem
  ## description.
  if (length(colnames(id.subsys)) == 2) {
    id.subsys <- NULL
  }
  
  performGraphAnalysis(adjMatrix, ids, outdir, id.subsys)
}

writeClassicalStatistics <- function(outdir, ids.connected) {
  rank.by.total <- get.rank.by.field(ids.connected, "total", 20)
  rank.by.numcommits  <- get.rank.by.field(ids.connected, "numcommits", 20)

  write.table(rank.by.total, file=paste(outdir, "/top20.total.txt", sep=""),
              sep="\t", quote=FALSE)
  print(xtable(rank.by.total), type="latex", floating=FALSE,
        file=paste(outdir, "/top20.total.tex", sep=""),
        sanitize.colnames.function=rotate.label)

  write.table(rank.by.numcommits, file=paste(outdir, "/top20.numcommits.txt",
                                    sep=""), sep="\t", quote=FALSE)
  print(xtable(rank.by.numcommits), type="latex", floating=FALSE,
        file=paste(outdir, "/top20.numcommits.tex", sep=""),
        sanitize.colnames.function=rotate.label)
}

performGraphAnalysis <- function(adjMatrix, ids, outdir,  id.subsys=NULL){
  
  ##====================================
  ##     Find Connected Subgraphs
  ##====================================
  ## Scale edge weights to integer values 
  ## adjMatrix <- ceiling(scale.data(adjMatrix, 0, 1000))  
  
  ## Isolated graph members are outliers for the Linux kernel. Eliminate
  ## them to create a connected graph (NOTE: This must not be done for
  ## projects where proper direct clustering can happen)
  status("Computing adjacency matrices")
  
  g <- graph.adjacency(adjMatrix, mode="directed", weighted=TRUE)
  g.clust <- clusters(g)
  
  ## Find the index of the largest connected cluster 
  largestClustMembership = which(g.clust$csize == max(g.clust$csize))
  ## Get all indecies of connected developers for the largest cluster
  idx <- which(g.clust$membership==largestClustMembership) 
  adjMatrix.connected <- as.matrix(adjMatrix[idx,idx])
  
  
  ## Build adjacency matrix of connected developers
  ids.connected <- ids[idx,]
  ids.connected$ID=seq(1:length(idx))
  ids.connected$Name <- as.character(ids.connected$Name)

  if (!is.null(id.subsys)) {
    id.subsys.connected <- id.subsys[idx,]
    id.subsys.connected$ID=seq(1:length(idx))
  }
  
  g.connected <- graph.adjacency(adjMatrix.connected, mode="directed",
                                 weighted=TRUE)
  V(g.connected)$label <- as.character(ids.connected$Name)
  
  ## TODO: Include computing classical statistics from performTagAnalysis
  
  ##========================
  ##  Page rank analysis 
  ##========================
  
  ## Compute the page ranking for all developers in the database
  status("Computing page rank")
  ## This puts the focus on tagging other persons
  pr.for.all <- compute.pagerank(adjMatrix.connected, transpose=TRUE,
                                 weights=TRUE)
  ## ... and this on being tagged. 
  pr.for.all.tr <- compute.pagerank(adjMatrix.connected, .damping=0.3,
                                    weights=TRUE)
  
  ## NOTE: pr.for.all$value should be one, but is 0.83 for some
  ## reason. This seems to be a documentation bug, though:
  ## https://bugs.launchpad.net/igraph/+bug/526106
  devs.by.pr <- influential.developers(NA, pr.for.all, adjMatrix.connected,
                                       ids.connected)

  devs.by.pr.tr <- influential.developers(NA, pr.for.all.tr,
                                          adjMatrix.connected, ids.connected)
  
  ##-----------
  ##save data 
  ##-----------
  writePageRankData(outdir, devs.by.pr, devs.by.pr.tr)

  status("Computing classical statistics")
  writeClassicalStatistics(outdir, ids.connected)
  
  ##=======================
  ## Find Communities 
  ##=======================
  ## Scale the weight in the adjacency matrix for propers visualization
  ## graphviz requires integer edge weights adjMatrix.connected.scaled =
  ## round( scale.data(adjMatrix.connected, 0, 1000) )
  adjMatrix.connected.scaled <- adjMatrix.connected
  
  ##--------------------
  ##infomap
  ##--------------------
  ##g.infomap.community <- infomap.community(g.connected)
  ##g.walktrap.community <- infomap.community(g.connected)
  
  ##--------------------
  ## spin-glass 
  ##--------------------
  status("Inferring communities with spin glasses")
  set.seed(42)
  g.spin.community <- spinglass.community(g.connected)
  
  status("Writing community graph sources for spin glasses")
  elems.sg.more <- select.communities.more(g.spin.community, 10)

  ## TODO: For the non-tagged analysis, use save.groups instead of
  ## save.groups.NonTag (functions are of the same signature)
  save.groups(adjMatrix.connected.scaled, ids.connected,
              g.spin.community, pr.for.all, outdir,
              "sg_reg_", elems.sg.more, save.group,
              label="Spin Glass Community")
  save.groups(adjMatrix.connected.scaled, ids.connected,
              g.spin.community, pr.for.all.tr, outdir,
              "sg_tr_", elems.sg.more, save.group,
              label="Spin Glass Community")
  
  ##--------------------
  ## Random walk
  ##--------------------
  status("Inferring communities with random walks")
  set.seed(42)
  g.walktrap.community <- walktrap.community(g.connected)
  
  status("Writing community graph sources for random walks")
  elems.wt.more <- select.communities.more(g.walktrap.community, 10)
  ## When selecting elements lower than some value we must take care to
  ## no select communities with size 1 as the graph.adjacency function
  ## fail with the weights attribute true
  elems.wt.less <- select.communitiy.size.range(g.walktrap.community, 2, 10) #communities of size 2-10)
  save.groups(adjMatrix.connected.scaled, ids.connected,
              g.walktrap.community, pr.for.all, outdir,
              "wt_reg_big_", elems.wt.more, save.group,
              label="(big) Random Walk Community")
  save.groups(adjMatrix.connected.scaled, ids.connected,
              g.walktrap.community, pr.for.all.tr, outdir,
              "wt_tr_big_", elems.wt.more, save.group,
              label="(big) Random Walk Community")
  
  save.groups(adjMatrix.connected.scaled, ids.connected, g.walktrap.community,
              pr.for.all, outdir, "wt_reg_small_", elems.wt.less,
              save.group, label="(small) Random Walk Community")
  save.groups(adjMatrix.connected.scaled, ids.connected, g.walktrap.community,
              pr.for.all.tr, outdir, "wt_tr_small_", elems.wt.less,
              save.group, label="(small) Random Walk Community")

  ##--------------------
  ## Community Quality
  ##--------------------
  ## TODO: Export the quality data somewhere
  if (FALSE) {
    sg.quality.modularity  <- compute.all.community.quality(g.connected, g.spin.community, "modularity") 
    sg.quality.conductance <- compute.all.community.quality(g.connected, g.spin.community, "conductance")
    sg.quality.modularization <- compute.all.community.quality(g.connected, g.spin.community, "modularization")
    wt.quality.modularity  <- compute.all.community.quality(g.connected, g.walktrap.community, "modularity")  
    wt.quality.conductance <- compute.all.community.quality(g.connected, g.walktrap.community, "conductance")
    wt.quality.modularization <- compute.all.community.quality(g.connected, g.walktrap.community, "modularization")
  }

  ##------------------
  ## Write other data 
  ##-----------------
  status("Writing the all-developers graph sources")
  
  ## NOTE: The all-in-one graphs get a different suffix (ldot for "large
  ## dot") so that we can easily skip them when batch-processing graphviz
  ## images -- they take a long while to compute
  g.all <- save.all(adjMatrix.connected.scaled, ids.connected, pr.for.all,
                    g.spin.community, save.group,
                    paste(outdir, "/sg_reg_all.ldot", sep=""),
                    label="Spin glass, regular page rank")
  g.all <- save.all(adjMatrix.connected.scaled, ids.connected,
                    pr.for.all.tr, g.spin.community, save.group,
                    paste(outdir, "/sg_tr_all.ldot", sep=""),
                    label="Spin glass, transposed page rank")
  g.all <- save.all(adjMatrix.connected.scaled, ids.connected, pr.for.all,
                    g.walktrap.community, save.group,
                    paste(outdir, "/wt_reg_all.ldot", sep=""),
                    label="Random walk, regular page rank")
  g.all <- save.all(adjMatrix.connected.scaled, ids.connected, pr.for.all.tr,
                    g.walktrap.community, save.group,
                    paste(outdir, "/wt_tr_all.ldot", sep=""),
                    label="Random walk, transposed page rank")

  if (!is.null(id.subsys)) {
    status("Plotting per-cluster subsystem distribution")
    plot.comm.subsys(g.spin.community, id.subsys.connected,
                     paste(outdir, "/sg_comm_subsys.pdf", sep=""),
                     "spin glass")
    ## Since walktrap produces smaller, but more communities, we divide the
    ## plot into two parts
    plot.comm.subsys(g.walktrap.community, id.subsys.connected,
                     paste(outdir, "/wt_comm_subsys_big.pdf", sep=""),
                     "random walk", elems=elems.wt.more)
    plot.comm.subsys(g.walktrap.community, id.subsys.connected,
                     paste(outdir, "/wt_comm_subsys_small.pdf", sep=""),
                     "random walk", elems=elems.wt.less)
    
    status("Saving raw per-cluster, per-subsystem statistical summaries")
    save.cluster.stats.subsys(g.spin.community, id.subsys.connected,
                              elems.sg.more, outdir, "sg_cluster_subsys_")
    save.cluster.stats.subsys(g.walktrap.community, id.subsys.connected,
                              elems.wt.more, outdir, "wt_cluster_subsys_")
    save.cluster.stats.subsys(g.walktrap.community, id.subsys.connected,
                              elems.wt.less, outdir, "wt_cluster_subsys_")
  }

  status("Saving raw per-cluster statistical summaries")
  save.cluster.stats(g.spin.community, ids.connected, elems.sg.more, pr.for.all,
                     outdir, "sg_cluster_more_")
  save.cluster.stats(g.walktrap.community, ids.connected, elems.wt.more, pr.for.all,
                     outdir, "wt_cluster_more_")
  save.cluster.stats(g.walktrap.community, ids.connected, elems.wt.less, pr.for.all,
                     outdir, "wt_cluster_less_")

  ## Also save the complete decomposition without removing any small communities
  save.cluster.stats(g.spin.community, ids.connected,
                     unique(g.spin.community$membership), pr.for.all,
                     outdir, "sg_cluster_")
  save.cluster.stats(g.walktrap.community, ids.connected,
                     unique(g.walktrap.community$membership), pr.for.all,
                     outdir, "wt_cluster_")
}


compute.all.community.quality <- function(graph, community, test) {
########################################################################
  ##Input:
  ##   - graph, igraph object

  ##   - community, community object that indicates how vertices in graph are
  ##		belonging to a community, must contain csize and
  ##		membership attributes these are standard attributes
  ##		for community dectection algorithms in
  ##
  ##       igraph
  ## Output:
  ##   - vector indexed by community number indicating the quality of that
  ##		community
########################################################################
  
  ## Get number of communities 
  community.id <- unique(community$membership)
  members <- sapply(community.id,
                    function(x) { return(list(which(community$membership==x))) })
  
  if(test == "modularity") {
    quality.vec <- sapply(community.id,
                          function(x) {return(community.quality.modularity(graph, members[[x]]))})
  }
  else if (test == "wilcox") {
    quality.vec <- sapply(community.id,
                          function(x) {return(community.quality.wilcox(graph, members[[x]]))})
  }
  else if (test == "conductance") {
    quality.vec <- sapply(community.id,
                          function(x) {return(community.quality.conductance(graph, members[[x]]))})
    
  }
  else if (test == "modularization") {
    quality.vec <- sapply(community.id,
                          function(x) {return(community.quality.modularization(graph, members[[x]], community$membership))})
  }
  
  ## remove nan values that might be introduced by isolated communities or 
  ## single communities 
  quality.vec.rm.nan <- quality.vec[!is.nan(quality.vec)]
  
  return(quality.vec.rm.nan)
}



## Compare the results of the tag and non tag based graphs 
graphComparison <- function(adjMatrix1, ids1, adjMatrix2, ids2,
                            outputFileName) {
  ## Normalize graphs to have edge weight between 0-1
  nonTagAdjMatrix.weighted <- scale.data(adjMatrix1, 0, 1000)
  tagAdjMatrix.weighted	 <- scale.data(adjMatrix2, 0, 1000)

  ## Normalize graphs to have binary edge weight
  nonTagAdjMatrix <- ceiling( scale.data(adjMatrix1, 0, 1) )
  tagAdjMatrix    <- ceiling( scale.data(adjMatrix2, 0, 1) )

  ## Create igraph objects
  g.nonTag <- graph.adjacency(nonTagAdjMatrix, mode="directed")
  g.Tag    <- graph.adjacency(tagAdjMatrix   , mode="directed")
  
  ## Get largest connected cluster
  idx.nonTag.connected <- largestConnectedSubgraphIndices(g.nonTag)
  idx.Tag.connected    <- largestConnectedSubgraphIndices(g.Tag   )
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
  
  performGraphAnalysis(similarity.adjMatrix.weighted, ids.intersect,
                       "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/experiments")
  
  ##write.graph.2.file("/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/experiments/similarityGraph.dot", g.similarity, ids.intersect, ids.intersect$ID)
}

get.community.graph <- function(graph, community, prank, ids) {
  community.idx <- sort(unique(community$membership))
  influential.people <- sapply(community.idx,
                               function(comm.idx) {
                                 which(prank$vector == max(prank$vector[which(community$membership == comm.idx)]))[1]
                               })

  names <- ids$Name[influential.people]
  
  g.contracted <- contract.vertices(graph, membership(community))
  E(g.contracted)$weight <- 1 
  g.simplified  <- simplify(g.contracted)
  V(g.simplified)$label <- names
  
  ## We also use the page rank to specify the font size of the vertex
  V(g.simplified)$fontsize <- scale.data(prank$vector, 15, 50)[influential.people]
  
  ## The amount of changed lines is visualised by the nodes background colour:
  ## The darker, the more changes.
  ##fc <- as.character(as.integer(100-scale.data(log(.iddb$total+1),0,50)[idx]))
  V(g.simplified)$fillcolor <- paste("grey", 50, sep="")
  V(g.simplified)$style="filled"
  write.graph(g.simplified, "/Users/Mitchell/Desktop/community.dot",
              format="dot")
}

runRandCompare <- function(){
  
  ## Setup Directories
  nonTagDir <- "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_NonTag/30"
  tagDir    <- "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_Tag/30"
  
  ## Read files for ids and adjacency matrix
  nonTagAdjMatrix <- read.table(file=paste(nonTagDir, "/adjacencyMatrix.txt", sep=""),
                                sep="\t", header=FALSE)
  ids.nonTag <- read.csv(file=paste(nonTagDir, "/ids.txt", sep=""),
                         sep="\t", header=TRUE, stringsAsFactors = FALSE)
  ids.nonTag$ID <- ids.nonTag$ID + 1
  
  
  tagAdjMatrix <- read.table(file=paste(tagDir, "/tags.txt", sep=""),
                             sep="\t", header=FALSE)
  ids.Tag <- read.csv(file=paste(tagDir, "/ids.txt", sep=""),
                      sep="\t", header=TRUE, stringsAsFactors = FALSE)
  ids.Tag$ID <- ids.Tag$ID + 1
  
  
  colnames(nonTagAdjMatrix) <- rownames(nonTagAdjMatrix)
  colnames(tagAdjMatrix)    <- rownames(tagAdjMatrix)
  
  
  ## The adjacency file format uses a different convention for edge direction
  ## than GNU R, so we need to transpose the matrix
  nonTagAdjMatrix <- t(nonTagAdjMatrix)
  tagAdjMatrix    <- t(tagAdjMatrix)
  
  ## Randomize Graph
  ## Get dimension of the adjacency matrix
  dimension.nonTag <- ncol(nonTagAdjMatrix)
  idx <- 1:dimension.nonTag
  ## Randomize the indecies
  idx.rand <- sample(idx, replace=FALSE)
  ## Randomize the adjacency matrix 
  nonTagAdjMatrixRand <- nonTagAdjMatrix[idx.rand, idx.rand]

  ## Run comparison on randomized adjacency matrix 
  fileName = '/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/experiments/RandomSimilarityComparison.pdf'
  graphComparison(nonTagAdjMatrixRand, ids.nonTag, tagAdjMatrix, ids.Tag, fileName)
  
  
}

runGraphCompare.Tag.nonTag <- function() {
  ## Setup Directories
  nonTagDir <- "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_NonTag/30"
  tagDir    <- "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_Tag/30"
  
  ## Read files for ids and adjacency matrix
  nonTagAdjMatrix <- read.table(file=paste(nonTagDir, "/adjacencyMatrix.txt", sep=""),
                                sep="\t", header=FALSE)
  ids.nonTag <- read.csv(file=paste(nonTagDir, "/ids.txt", sep=""),
                         sep="\t", header=TRUE, stringsAsFactors = FALSE)
  ids.nonTag$ID <- ids.nonTag$ID + 1
  
  
  tagAdjMatrix <- read.table(file=paste(tagDir, "/adjacencyMatrix.txt", sep=""),
                             sep="\t", header=FALSE)
  ids.Tag <- read.csv(file=paste(tagDir, "/ids.txt", sep=""),
                      sep="\t", header=TRUE, stringsAsFactors = FALSE)
  ids.Tag$ID <- ids.Tag$ID + 1
  
  
  colnames(nonTagAdjMatrix) <- rownames(nonTagAdjMatrix)
  colnames(tagAdjMatrix)    <- rownames(tagAdjMatrix)
  
  
  ## The adjacency file format uses a different convention for edge direction
  ## than GNU R, so we need to transpose the matrix
  nonTagAdjMatrix <- t(nonTagAdjMatrix)
  tagAdjMatrix    <- t(tagAdjMatrix)
  
  fileName = '/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/experiments/SimilarityComparison.pdf'
  graphComparison(nonTagAdjMatrix, ids.nonTag, tagAdjMatrix, ids.Tag, fileName)
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
	if(percent.difference != 0){
		browser()
	}
	return(percent.difference)
}

write.graph.2.file <- function(.filename, g, .iddb, idx) {
  V(g)$label <- as.character(IDs.to.names(.iddb, idx))	
  
  write.graph(g, .filename, format="dot")
}

#########################################################################
                                        #     					 Experiments 
#########################################################################
experiment <- function(g, g.connected){
                                        # Somce other generic graph measures. TODO: See if/how we can use them
  max(closeness(g))
  max(betweenness(g))
  
                                        # NOTE: Computing the adjacency graphs with weighted instead
                                        # of multiple edges could be done with
                                        # g <- graph.adjaceny(tags, mode="directed", weighted=TRUE)
                                        # but takes quite a long time (and is also not suitable for most
  o# community detection algorithms)
  
  ranks <- page.rank(g)
                                        # Map the page rank values to [0,100]
  ranks.norm <-  ranks
  ranks.norm$vector <- scale.data(ranks.norm$vector, 0, 100)
  
  test <-  clique.community(g.connected, 5)
  
  test <- largeScaleCommunity(g.connected)
}

#########################################################################
##     					 Testing Section  
#########################################################################
nonTagTest <- function(){
  
  dataDir <- "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_NonTag/30"
  performAnalysis(dataDir)
  
}
TagTest <- function(){
  
  dataDir <- "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_Tag/30"
  performAnalysis(dataDir)
  
}

test.community.quality <- function() {
  
  r.1 <- c(0,1,1,1,1,0,0,0)
  r.2 <- c(1,0,1,1,0,0,0,0)
  r.3 <- c(1,1,0,1,0,0,0,1)
  r.4 <- c(1,1,1,0,0,0,0,0)
  r.5 <- c(1,0,0,0,0,1,0,0)
  r.6 <- c(0,0,0,0,1,0,1,0)
  r.7 <- c(0,0,0,0,0,1,0,1)
  r.8 <- c(0,0,1,0,0,0,1,0)
  
  adj.matrix <- matrix(data = c(r.1,r.2,r.3,r.4,r.5,r.6,r.7,r.8), ncol = 8, nrow = 8)
  
  g <- graph.adjacency(adj.matrix)
  
  
  ## Test that modularity is correct
  g.spincommunity <- spinglass.community(g)
  igraph.modularity.result <- modularity(g, g.spincommunity$membership)
  modularity.result        <- sum(compute.all.community.quality(g, g.spincommunity, "modularity"))
  if( !(igraph.modularity.result == modularity.result)){
    print("Error: modularity test failed")
  }
  else{
    print("Success: modularity test passed")
  }
}

test.community.quality.modularity <- function() {
  
  ##        1,2,3,4,5,6,7,8
  r.1 <- c(0,0,1,0,0,0,0,0)
  r.2 <- c(0,0,1,0,0,0,0,0)
  r.3 <- c(0,0,0,0,1,0,0,0)
  r.4 <- c(1,0,0,0,1,0,0,0)
  r.5 <- c(0,0,0,0,0,0,0,0)
  r.6 <- c(0,0,0,0,0,0,1,1)
  r.7 <- c(0,0,0,0,0,0,0,0)
  r.8 <- c(0,0,0,0,1,0,1,0)
  
  adj.matrix <- t(matrix(data = c(r.1,r.2,r.3,r.4,r.5,r.6,r.7,r.8), ncol = 8, nrow = 8))
  
  g <- graph.adjacency(adj.matrix)
  g.clust <- list() 
  g.clust$membership <- c(1,1,1,2,2,3,3,3)
  
  quality <- compute.all.community.quality(g, g.clust, "modularization")
  
}

#########################################################################
##     					 Executed Statements  
#########################################################################
##TagTest()
##nonTagTest()
##test.community.quality()
##test.community.quality.modularity()

##----------------------------
## Parse commandline arguments
##----------------------------
parser <- OptionParser(usage = "%prog resdir config")
arguments <- parse_args(parser, positional_arguments = TRUE)

if(length(arguments$args) != 2) {
  cat("Error: Please specify result directory and configuration file!\n\n")
  print_help(parser)
  stop()
  
} else {
  resdir <- arguments$args[1]
  config.file <- arguments$args[2]
}

##------------------------------
## Perform appropriate analysis
##------------------------------
options(error = quote(dump.frames("error.dump", TRUE)))

conf <- load.config(config.file)
global.conf <- load.global.config("prosoda.conf")
conf <- init.db(conf, global.conf)

performAnalysis(resdir, conf)
