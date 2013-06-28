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
##  various measures and test for the significance and quality of a community
##  or cluster in a graph structure


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
	inter_edge_sum <- community.vertices.degree.total - intra.degree.total

	intra_edge_sum <- intra.degree.total / 2

	return(inter_edge_sum / (inter_edge_sum + intra_edge_sum))
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
	graph.connected   <- largest.subgraph(graph)
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
	t.test.result <- t.test(rand.samps,  mu=mean(cluster.conductance))
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
		weights <- E(graph)$weight
		rw.graph <- rewire(graph, mode = rewire.mode, niter = 10000)
		E(rw.graph)$weight <- weights
		rw.graph.connected <- largest.subgraph(rw.graph)

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
	if (class(community) == "communities") {
		community.id <- unique(community$membership)
		members <- sapply(community.id,
				function(x) { return(list(which(community$membership==x))) })
	}
	else if (class(community) == "overlapComm") {
		community.id <- 1:length(community$csize)
		members <- community
	}
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
	else if (test == "betweenness") {
		quality.vec <- sapply(community.id,
				function(x) {return (mean(betweenness(graph, members[[x]])))})
	}
	return(quality.vec)
}