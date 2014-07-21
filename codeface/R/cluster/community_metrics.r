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
##  Various measures and test for the significance and quality of a community
##  or cluster in a graph structure
suppressMessages(library(BiRewire))
suppressMessages(library(parallel))

edge.weight.to.multi <- function(g) {
  ## Converters an edge weight to multiple edges between the connected nodes
  ## Args:
  ##	g: igraph graph object
  ## Returns:
  ##	g.multi: igraph graph object with multiple edges and non-weighted edges
  mult.edges <- c()
  mult.edges <-unlist(mapply(function(x.1,x.2,w) rep(c(x.1,x.2), times=w-1),
                      get.edgelist(g)[,1], get.edgelist(g)[,2],
                      E(g)$weight))
  g.multi <- add.edges(g, mult.edges)
  return(g.multi)
}
##############################################
## Based on the idea of modularization
## Reference: Social network clustering and visualization using hierarchical
##			edge bundles (Jia, Garland, Hart)
###############################################
community.quality.modularization <- function(graph, community.vertices,
                                             membership.vec) {
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
			function(x) {
                          res <- sum(graph[community.vertices,
                                           which(x == membership.vec)]) +
                                 sum(graph[which(x == membership.vec),
                                           community.vertices])
                          return(res)
                        })

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


#######################################
## Finds the conductance of a cluster
#######################################
community.quality.conductance <- function(graph, community.vertices) {
  ## Get graph composed solely of vertices belonging to the community
  cluster.subgraph <- induced.subgraph(graph, community.vertices)

  ## get weighted degree for subgraphs
  intra.degree <- graph.strength(cluster.subgraph, mode="all")

  ## degree of vertices in community
  community.vertices.degree <- graph.strength(graph, community.vertices,
											  mode="all")

  ## sum all degrees from vertices
  community.vertices.degree.total  <- sum(community.vertices.degree)
  intra.degree.total               <- sum(intra.degree)

  ## edge sum of edges that cross
  inter.edge.sum <- community.vertices.degree.total - intra.degree.total

  return(inter.edge.sum / community.vertices.degree.total)
}

community.quality.wilcox <- function (graph, community.vertices) {
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


######################################################################
## Measure the based on modularity calculation
## quality of the a community indicated by a set of vertices
## that belong to the community in comparison to the rest of the graph
## Reference:
## "On Modularity Clustering," Knowledge and Data Engineering,
## IEEE Transactions on , vol.20, no.2, pp.172-188, Feb. 2008
######################################################################
community.quality.modularity <- function(graph, community.vertices) {
  ## Get graph composed solely of vertices belonging to the community
  subgraph <- induced.subgraph(graph, community.vertices)

  ## Number of edges interal to community
  community.num.edges = sum(graph.strength(subgraph, mode="all")) / 2
  ## Total number of edge is graph
  m = sum(graph.strength(graph, mode="all")) / 2

  ## Measure the degree for intra-community edges
  intra.degree <- igraph::degree(subgraph)

  ## Degree of vertices in community
  community.vertices.degree <- graph.strength(graph, community.vertices, mode="all")

  ## Calculate final result
  f.1 <- community.num.edges / m
  f.2 <- (sum(community.vertices.degree) / (2*m) )^2
  quality  <- f.1 - f.2
  return(quality)
}

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
community.stat.significance <- function(graph, cluster.algo, metric, niter) {
  ## Extract clusters
  graph.clusters <- community.detection.disconnected(graph, cluster.algo)

  ## Compute cluster quality
  cluster.quality <- community.metric(graph, graph.clusters,
                                      metric)
  cluster.quality <- cluster.quality[!is.na(cluster.quality)]
 
  ## Compute randomized conductance samples
  rand.samps <- rewired.graph.samples(graph, cluster.algo, metric, niter)

  ## Test for normality
  ## check if values are same because the test while fail if they are 
  if(length(unique(rand.samps)) == 1) {
    shapiro.test.result <- c()
    shapiro.test.result$p.value <- -1
  } else {
    shapiro.test.result <- shapiro.test(rand.samps)
  }

  ## Perform T-test on test statistic
  t.test.result <- t.test(rand.samps,  mu=mean(cluster.quality))

  ## Output result
  res <- list(random.simulation.quality=rand.samps,
              original.quality=cluster.quality,
              t.test.result=t.test.result,
              shapiro.test.result=shapiro.test.result)

  return(res)
}


## write the significance test results to pdf
plot.t.test <- function(random.samples, test.values, t.test, shapiro.test, title, metric.name) {
  test.value.mean <- mean(test.values)
  conf.intervals <- c(t.test$conf.int[1], t.test$conf.int[2]) 
  x.lower.lim <- 0
  x.upper.lim  <- 1
  x.lim = c(x.lower.lim, x.upper.lim)
  t.test.annotation <- paste("T-test p-value =", as.character(signif(t.test$p.value,3))) 
  shapiro.test.annotation <- paste("Shapiro-test p-value =", as.character(signif(shapiro.test$p.value,3)))
  total.annotation <- paste(t.test.annotation, shapiro.test.annotation, sep="\n")
  y.upper.lim <- max(density(random.samples)$y)

  t.test.plot <- ggplot(data.frame(random.samples), aes(x=random.samples)) +
                 geom_histogram(aes(y=..density..), color="black", fill="white") +
                 geom_density(alpha=.2, fill="grey") +
                 geom_vline(xintercept=conf.intervals, linetype="dotted") +
                 geom_point(data=data.frame(x=test.value.mean, y=0), aes(x=x, y=y), size=5, color='black') +
                 scale_x_continuous(limits = x.lim) +
                 ggtitle(title) +
                 xlab(metric.name) +
                 ylab("Density") +
                 theme(plot.title = element_text(lineheight=.8, face="bold"),
                       axis.title.x = element_text(size=rel(1.3)),
                       axis.title.y = element_text(size=rel(1.3))) +
                 annotate("text", label=total.annotation, x=x.upper.lim, y=y.upper.lim*1.15) +
                 theme_bw(base_size=16)

  return(t.test.plot)
}


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
rewired.graph.samples <- function(graph, cluster.algo, metric, niter) {
  ## Check if loops exist in the original graph, this information is necessary
  ## to choose the appropriate rewiring strategy
  loops.exist <- any(is.loop(graph))
  if (loops.exist) {
    rewire.mode = "loops"
  }
  else {
    rewire.mode = "simple"
  }

  ## Perform iterations
  graph.multi <- edge.weight.to.multi(graph)
  graph.multi <- remove.vertex.attribute(graph.multi, 'name')

  compute.rewire <- function(i) {
    ## Rewire graph, randomize the graph while maintaining the degree distribution
    rw.graph <- birewire.rewire(graph.multi, MAXITER_MUL=1000, exact=T, verbose=F)

    E(rw.graph)$weight <- 1
    rw.graph <- simplify(rw.graph, remove.loops=FALSE)

    ## Find clusters
    rw.graph.clusters <- community.detection.disconnected(rw.graph, cluster.algo)

    ## Only analyze clusters that are large than 4 vertices
    min.community <- minCommGraph(rw.graph, rw.graph.clusters, 5)
    rw.graph.min <- min.community$graph
    rw.graph.clusters.min <- min.community$community

    ## Compute conductance
    rw.cluster.conductance <- community.metric(rw.graph.min, rw.graph.clusters.min, metric)
    
    return(mean(rw.cluster.conductance, na.rm=TRUE))
  }

  conduct.vec <- unlist(mclapply(1:niter, compute.rewire, mc.cores=2))

  return(conduct.vec)
}


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
##   - vector of metric values indexed by community number
########################################################################
community.metric <- function(graph, community, test) {
  ## Get number of communities
  if (class(community) == "communities") {
    community.id <- sort(unique(community$membership))
    members <- sapply(community.id,
                      function(x) {
                        return(list(which(community$membership==x)))
                      })
  }
  else if (class(community) == "overlapComm") {
    community.id <- 1:length(community$csize)
    members <- community
  }

  if(test == "modularity") {
    modularity.vec <- sapply(community.id,
                             function(x) {
                               return(community.quality.modularity(graph, members[[x]]))
                            })
    metric.vec <- sum(modularity.vec)
  }
  else if (test == "wilcox") {
    metric.vec <- sapply(community.id,
                          function(x) {
                            return(community.quality.wilcox(graph, members[[x]]))})
  }
  else if (test == "conductance") {
    metric.vec <- sapply(community.id,
                          function(x) {
                            return(community.quality.conductance(graph, members[[x]]))
                          })

  }
  else if (test == "modularization") {
    metric.vec <- sapply(community.id,
                          function(x) {
                            return(community.quality.modularization(graph,
                                            members[[x]], community$membership))
                          })
  }
  else if (test == "betweenness") {
    metric.vec <- sapply(community.id,
                          function(x) {
                            g.sub <- induced.subgraph(graph, members[[x]])
                            return(betweenness(g.sub))
                          })
  }
  else if(test == "transitivity") {
    metric.vec <- sapply(community.id,
                           function(x) {
                             g.sub <- induced.subgraph(graph, members[[x]])
                             return (transitivity(g.sub,type="local"))
                           })
  }
  else if(test == "out.weight") {
    metric.vec <- sapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (graph.strength(g.sub, mode="out"))
        })
  }
  else if(test == "in.weight") {
    metric.vec <- sapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (graph.strength(g.sub, mode="in"))
        })
  }
  else if(test == "out.deg") {
    metric.vec <- sapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (igraph::degree(g.sub, mode="out"))
        })
  }
  else if(test == "in.deg") {
    metric.vec <- sapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (igraph::degree(g.sub, mode="in"))
        })
  }
  else if(test == "diameter") {
    metric.vec <- sapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (diameter(g.sub))
        })
  }

  return(metric.vec)
}


## Compute SNA metrics using community centric perspective
## ARGS:
##  g: igraph graph object
##  comm: igraph communities object
## RETURNS:
##  res: list containing all statistics
compute.community.metrics <- function(g, comm, link.type=NULL) {
  res <- list()

  ## Intra-community features
  res$intra.betweenness  <- community.metric(g, comm,
                                             "betweenness")
  res$intra.transitivity <- community.metric(g, comm,
                                             "transitivity")
  res$intra.in.deg     <- community.metric(g, comm, "in.deg")
  res$intra.out.deg    <- community.metric(g, comm, "out.deg")
  res$intra.in.weight  <- community.metric(g, comm, "in.weight")
  res$intra.out.weight <- community.metric(g, comm, "out.weight")
  res$intra.diameter   <- community.metric(g, comm, "diameter")
  
  ## Inter-community features
  g.con     <- contract.vertices(g, membership(comm), vertex.attr.comb=toString)
  g.con.sim <- simplify(g.con)
  res$inter.betweenness   <- betweenness(g.con.sim)
  res$inter.transitivity <- transitivity(g.con.sim, type="local", isolates='zero')
  res$inter.in.deg       <- igraph::degree(g.con.sim, mode="in")
  res$inter.out.deg      <- igraph::degree(g.con.sim, mode="out")
  res$inter.in.weight    <- graph.strength(g.con.sim, mode="in")
  res$inter.out.weight   <- graph.strength(g.con.sim, mode="out")
  res$inter.diameter     <- diameter(g.con.sim, weights=NULL)
  res$num.communities    <- vcount(g.con.sim)

  ## Community Quality
  res$conductance <- community.metric(g, comm, "conductance")
  res$mean.conductance <- mean(res$conductance, na.rm=TRUE)
  res$sd.conductance <- sd(res$conductance, na.rm=TRUE)
  res$modularity     <- modularity(g, comm$membership)

  ## Transpose matrix for tag network
  #if(link.type == "tag") {
    #adj.mat <- get.adjacency(g)
    #g       <- graph.adjacency(t(adj.mat))
  #}

  ## Global graph features
  global.df <- data.frame()
  res$cluster.coefficient <- transitivity(g, type="local", isolates='zero')
  res$betweenness.centrality <- betweenness(g, directed=FALSE, normalize=TRUE)
  res$page.rank <- seq(1,100)#page.rank(g, directed=FALSE)$vector  
  res$average.path.len <- average.path.length(g, directed=FALSE)
  res$v.degree <- igraph::degree(g, mode="all")
  res$num.vertices <- vcount(g)
  res$diameter <- diameter(g, weights=NULL)
  

  ## Community Aggregates
  res$mean.size <- mean(comm$csize)
  res$sd.size   <- sd(comm$csize)
  res$max.size  <- max(comm$csize)
  intra.edges   <- unlist(lapply(res$intra.in.deg,sum))
  res$mean.num.edges <- mean(intra.edges)
  res$sd.num.edges   <- sd(intra.edges)
  res$max.edges <- max(intra.edges)
  return(res)
}


generate.graph.trends <- function(con, cluster.method="Spin Glass Community", 					  
                                  construct.method="prox") {
  project.data   <- data.frame(id=c(18,23,37,38,39)) #query.projects(con) 
  project.data$name <- apply(project.data, 1, 
                             function(p.id) query.project.name(con, p.id))
  project.data$analysis.method <- apply(project.data, 1, 
                                        function(p.id) query.project.analysis.method(con, p.id))
  range.data <- lapply(project.data$id, function(p.id) get.cycles.con(con, p.id))
  metrics.df <- data.frame()
  project.list <- list()
  projects.df.list <- list()

  for(i in 1:nrow(project.data)) {
    project <- project.data[i,]
    p.id <- project$id
    project.name <- project$name
    analysis.method <- project$analysis.method
    p.ranges <- data.frame(range.ids=range.data[[i]]$range.id,
                           range.start=round_date(range.data[[i]]$date.start,'day'),
                           range.end=round_date(range.data[[i]]$date.end,'day'))

    ## Check if revision has an associate graph
    range.has.graph <- sapply(p.ranges$range.ids,
                              function(range.id) {
                                g.id <- query.global.collab.con(con, p.id, range.id)
                                return(!is.null(g.id))
                              })

    ## Keep only the revisions that have graphs
    p.ranges <- p.ranges[range.has.graph,]

    graph.data <- apply(p.ranges, 1,function(r) {
          r.id <- as.character(r["range.ids"])
          cycle <- paste(r['range.start'], r['range.end'], sep='-')
          graph <- get.graph.data.local(con, p.id, r.id, cluster.method)
          graph$p.id <- p.id
          graph$range.id <- r.id
          graph$cycle <- cycle
          graph$project.name <- project.name
          graph$analysis.method <- analysis.method
          return(graph)
          })

    ## check edgelist, if no edges then the graph is not needed
    graph.data <- lapply(graph.data, function(x) { 
                         if(length(x$edgelist) == 0) {
                           x <- NA
                         } 
                         return (x)
                        })
    ## remove NA from graph list
    graph.data <- graph.data[!is.na(graph.data)]
    
    graph.data <- lapply(graph.data,
        function(x) {
          x$graph <- graph.data.frame(x$edgelist, directed=TRUE,
              vertices=data.frame(x$v.local.ids))
          return(x)
          })

    ## Compute community metrics
    ## select communities which are of a minimum size 4
    graph.data <- lapply(graph.data, function(g) {
                         graph.comm <- minCommGraph(g$graph, g$comm, min=4)
                         g$graph <- graph.comm$graph
                         g$comm <- graph.comm$community
                         return(g)})

    ## Remove graphs that have no communities
    graph.data <- graph.data[sapply(graph.data, function(g) return(!is.null(g$comm)))]
    
    graph.data <- lapply(graph.data, function(g) {
                         g$stats <- compute.community.metrics(g$graph,  g$comm,
                                                              construct.method)
                         g$stats$page.rank <- g$rank
                         return(g)})
    
    ## create data frame for scalar graph measures
    df.list <- lapply(graph.data, function(g) {
                                    stats <- g$stats
                                    row <- list()
				                            ## Meta Data
                                    row$project <- g$p.id
                                    row$cycle <- g$cycle
                                    ## Global graph
                                    row$diameter <- stats$diameter 
                                    row$vcount <- stats$vcount
                                    row$p.rank.mean <- mean(stats$page.rank)
                                    row$p.rank.sd <- sd(stats$page.rank)
                                    row$deg.mean <- mean(stats$v.degree)
                                    row$c.coef <- mean(stats$clust.coeff)
                                    row$conductance.mean <- stats$mean.conductance
                                    row$modularity <- stats$modularity
                                    
                                    ## Inter Community
                                    row$num.comms <- stats$num.comms
                                    row$inter.comm.diameter <- stats$inter.diameter
                                    row$inter.bet.mean <- mean(stats$inter.betweenness)
                                    row$inter.tran.mean <- mean(stats$inter.transitivity)
					
			                              ## Intra Community
                                    row$diameter.mean <- mean(stats$intra.diameter)
                                    row$intra.bet.mean <- mean(unlist(stats$intra.betweenness))
                                    row$intra.tran.mean <- mean(unlist(stats$intra.transitivity))
                                    df <- data.frame(row)
                                    return(df)})

    projects.df.list[[i]] <- do.call("rbind", df.list)
    ## Handle higher dimensional data differently
    project.list[[i]] <- graph.data    
  }
  projects.data <- list()
  projects.data$df <- do.call("rbind", projects.df.list)
  
  return(project.list)
}

plot.influence.ts <- function(project.stats) {
  project.ranks <- lapply(project.stats, function(x) {
                                   y <- list()
                                   y$rank <- as.vector(x$comm.stats$p.rank)
                                   y$length <- length(y$rank)
                                   y$cycle <- x$cycle
                                   return (y)})
         
  ranks <- unlist(lapply(project.ranks, function(x) x$rank))
  
  cycles <- unlist(lapply(project.ranks, function(x) x$cycle))
  lengths <- unlist(lapply(project.ranks, function(x) x$length))
  cycle <- factor(rep(cycles, lengths))                                 
  
  data <- data.frame(cycle, page.rank=ranks)
  rank.mean <- aggregate(data$page.rank, by=list(cycle=data$cycle), mean)
  rank.sd <- aggregate(data$page.rank, by=list(cycle=data$cycle), sd)
  browser()
  
  plot1 <- ggplot(data, aes(x=page.rank, colour=cycle)) + geom_density()
  return(plot1)
}

## Change plot to create box plots for page rank
## not just use mean and sd 
plot.project.trends <- function(g.trends) {
   plots <- list()
   # Developer Influence
   columns <- 3:ncol(g.trends)

   plots <- lapply(columns, function(col) {
                              y <- g.trends[,col]
                              ylab <- colnames(g.trends)[col]
                              qplot(data=g.trends, y=y, ylab=ylab, xlab="revision")
                              })
   
  
  do.call(grid.arrange,c(plots))
      

}

plot.box <- function(project.data, feature, outdir) {
  cycles <- do.call(rbind, lapply(project.data, 
                                  function(p) data.frame(cycle=p$cycle)))
  analysis.method <- project.data[[1]]$analysis.method
  project.name <- project.data[[1]]$project.name
  graph.feature <- lapply(project.data, 
                          function(g) return(g$stats[feature]))

  df <- melt(graph.feature)
  names(df) <- c("value", 'feature',"row.id")
  df <- merge(df, cycles, by.x='row.id', by.y='row.names')
  df$cycle <- as.factor(df$cycle)

  p0 <- ggplot(df, aes(x=cycle, y=value)) + geom_boxplot(outlier.shape = NA) + ylab(feature) + 
    xlab("Revision") + labs(title=project.name) + expand_limits(y=0) +
    theme(axis.text.x = element_text(family="Arial Narrow", 
                                     colour="black",size=12,angle=60,
                                     hjust=.6,vjust=.7,face="plain"))
  up.lim <- max(unlist(lapply(split(df$value, df$cycle), function(x) boxplot.stats(x)$stats[c(2,4)])))
  print(up.lim)
  ylim1 <- boxplot.stats(df$value)$stats[c(2,4)]
  ylim1[2] <- up.lim
  ylim1[1] <- 0
  p1 = p0 + coord_cartesian(ylim = ylim1*1.05)
  file.dir <- paste(outdir, "/", project.name, "_", analysis.method, sep="")
  dir.create(file.dir)
  file.name <- paste(file.dir, "/", feature, ".png",sep="")
  ggsave(file.name, p1, height=8, width=11)

}

plot.series <- function(project.data, feature, outdir) {
  cycles <- do.call(rbind, lapply(project.data, 
                                  function(p) data.frame(cycle=p$cycle)))
  analysis.method <- project.data[[1]]$analysis.method
  project.name <- project.data[[1]]$project.name
  graph.feature <- lapply(project.data, 
                          function(g) return(g$stats[feature]))
  df <- melt(graph.feature)
  names(df) <- c('value', 'feature', 'row.id')
  df <- merge(df, cycles, by.x='row.id', by.y='row.names')
  df$cycle <- as.factor(df$cycle)
  p <- ggplot(df, aes(x=cycle, y=value)) + geom_point(color= I('black')) + ylab(feature) + 
              xlab("Revision") + labs(title=project.name) + expand_limits(y=0) +
              theme(axis.text.x = element_text(family="Arial Narrow", 
                                     colour="black",size=12,angle=60,
                                     hjust=.6,vjust=.7,face="plain"))
  
  file.dir <- paste(outdir, "/", project.name, "_", analysis.method, sep="")
  dir.create(file.dir)
  file.name <- paste(file.dir, "/", feature, ".png",sep="")
  ggsave(file.name, p, height=8, width=11)
}

plot.page.rank <- function(project.data) {
  cycles <- sapply(1:length(project.data), function(x) project.data[[x]]$cycle)
  cycles <- sapply(cycles, function(x) strsplit(x, split="-")[[1]][1])

  p.rank.list <- lapply(project.data, function(g) {
                                      stats <- g$stats
                                      return (stats$p.rank)    
                                    })

  p.rank.median <- sapply(p.rank.list, function(x) mhedian(x))
  p.rank.med.df <- data.frame(cycles, p.rank.median)

  median.plot <- ggplot(p.rank.med.df, aes(x=cycles, y=p.rank.median)) + 
                        geom_point() + ylab("Median Page Rank") + xlab("Revision") + 
                        ggtitle("Linux Developer Page Rank Evolution") + 
                        theme(axis.text.x = element_text(family="Arial Narrow", 
                                            colour="black",size=12,angle=60,
                                            hjust=.6,vjust=.7,face="plain"))

  df <- melt(p.rank.list)
  names(df) <- c("rank", "rev")
  df$rev <- cycles[df$rev]
  df$rev <- as.factor(df$rev)
  p0 <- ggplot(df, aes(x=rev, y=rank)) + geom_boxplot(outlier.shape = NA) + ylab("Page Rank") + 
               xlab("Revision") + labs(title="Linux Developer Page Rank Evolution") + 
               theme(axis.text.x = element_text(family="Arial Narrow", 
                                   colour="black",size=12,angle=60,
                                   hjust=.6,vjust=.7,face="plain"))
  ylim1 <- boxplot.stats(df$rank)$stats[c(1,5)]
  ylim1[1] <- 0
  p1 = p0 + coord_cartesian(ylim = ylim1*1.6)
  
  ggsave("linux.png", p1, height=8, width=11)
  ggsave("linux_median.png", median.plot, height=8, width=11)

}

run.trends.analysis <- function (con) {
  outdir <- "/home/mitchell/workspace/trends"
  trends <- generate.graph.trends(con)
  metrics.box <- c('cluster.coefficient',
                   'betweenness.centrality',
                   'conductance',
                   'page.rank')
  metrics.series <- c('diameter',
                      'average.path.len',
                      'num.communities',
                      'num.vertices',
                      'inter.diameter',
                      'modularity')

  ## Generate and save box plots
  lapply(trends, function(t) sapply(metrics.box, function(m) plot.box(t, m, outdir)))
   
  ## Generate and save series plots
  lapply(trends, function(t) sapply(metrics.series, function(m) plot.series(t, m, outdir)))
  
  return(0)
}
