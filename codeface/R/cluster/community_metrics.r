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
suppressMessages(library(robustbase))
suppressMessages(library(ineq))
suppressMessages(library(markovchain))
suppressMessages(library(scales))
suppressMessages(library(xts))

source("../dependency_analysis.r", chdir=TRUE)
source("../network_stream.r", chdir=TRUE)
source("../developer_classification.r", chdir=TRUE)
source("community_helpers.r", chdir=TRUE)

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


###############################################################################
## Compute graph evolution at time t to t+1
## Given two graphs computers for time t and time t+1 we compute
## a data frame encompasing the turn-over data
## - Input
## igraph_t - graph at time t
## igraph_t_1 - graph at time t+1
## g_ids_t - list of global ids for graph_t
## g_ids_t_1 - list of global ids for graph_t_1
##
## - Output
## a dataframe containing the degree of each node in each of the graphs
## with -1 indicating the node was not present which is distinct from 0
## which means the node was present but just with a zero degree
###############################################################################
graph.turnover <- function(graph.t, graph.t.1, g.ids.t, g.ids.t.1, index) {
  g.id.intersect <- intersect(g.ids.t, g.ids.t.1)
  degree.t <- igraph::degree(graph.t)
  graph.t.degree <- data.frame(g.id=names(degree.t), degree.t=degree.t)
  degree.t.1 <- igraph::degree(graph.t.1)
  graph.t.1.degree <- data.frame(g.id=names(degree.t.1), degree.t.1=degree.t.1)

  ## Initialize data frame so that all nodes that were or were not present is known
  res <- data.frame(g.id=union(g.ids.t, g.ids.t.1))
  res <- merge(res, data.frame(g.id=g.ids.t, time.t=0), all.x=TRUE)
  res <- merge(res, data.frame(g.id=g.ids.t.1, time.t.1=0), all.x=TRUE)
  ## All columns with NA will be nodes that did not appear in both graphs and
  ## we reassign that to the -1 state to indicate not present
  res[is.na(res)] <- -1

  ## Add degree for each node in each graph
  res <- merge(res, graph.t.degree, by="g.id", all.x=TRUE)
  res <- merge(res, graph.t.1.degree, by="g.id", all.x=TRUE)
  res[is.na(res)] <- 0

  ## Sum columns
  res$time.t <- res$time.t + res$degree.t
  res$time.t.1 <- res$time.t.1 + res$degree.t.1

  ## Categorize node into states
  state.t <- paste("state.t.", as.character(index), sep="")
  state.t.1 <- paste("state.t.", as.character(index+1), sep="")
  res[, state.t] <- categorize.nodes(res$time.t)
  res[, state.t.1] <- categorize.nodes(res$time.t.1)

  return(res[, c("g.id", state.t, state.t.1)])
}

categorize.nodes <- function(node.degree) {
  ## Group nodes into categores based on there degree
  ## Absent = node not present
  ## Core = upper 20% in terms of degree
  ## Peripheral = lower 80% in terms of degree
  ## Isolated = present but with zero degree
  quant.80 <- quantile(node.degree, prob=.8)
  core <- node.degree > quant.80
  peripheral <- node.degree <= quant.80 & node.degree > 0
  isolated <- node.degree == 0
  absent <- node.degree < 0

  res <- vector(length=length(node.degree))
  res[core] <- "core"
  res[peripheral] <- "peripheral"
  res[isolated] <- "isolated"
  res[absent] <- "absent"

  return(res)
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
    modularity.vec <- lapply(community.id,
                             function(x) {
                               return(community.quality.modularity(graph, members[[x]]))
                            })
    metric.vec <- sum(modularity.vec)
  }
  else if (test == "wilcox") {
    metric.vec <- lapply(community.id,
                          function(x) {
                            return(community.quality.wilcox(graph, members[[x]]))})
  }
  else if (test == "conductance") {
    metric.vec <- lapply(community.id,
                          function(x) {
                            return(community.quality.conductance(graph, members[[x]]))
                          })

  }
  else if (test == "modularization") {
    metric.vec <- lapply(community.id,
                          function(x) {
                            return(community.quality.modularization(graph,
                                            members[[x]], community$membership))
                          })
  }
  else if (test == "betweenness") {
    metric.vec <- lapply(community.id,
                          function(x) {
                            g.sub <- induced.subgraph(graph, members[[x]])
                            return(betweenness(g.sub))
                          })
  }
  else if(test == "transitivity") {
    metric.vec <- lapply(community.id,
                           function(x) {
                             g.sub <- induced.subgraph(graph, members[[x]])
                             return (transitivity(g.sub,type="local"))
                           })
  }
  else if(test == "out.weight") {
    metric.vec <- lapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (graph.strength(g.sub, mode="out"))
        })
  }
  else if(test == "in.weight") {
    metric.vec <- lapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (graph.strength(g.sub, mode="in"))
        })
  }
  else if(test == "out.deg") {
    metric.vec <- lapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (igraph::degree(g.sub, mode="out"))
        })
  }
  else if(test == "in.deg") {
    metric.vec <- lapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (igraph::degree(g.sub, mode="in"))
        })
  }
  else if(test == "diameter") {
    metric.vec <- lapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return (diameter(g.sub))
        })
  }
  else if(test == "v.size") {
    metric.vec <- lapply(community.id,
        function(x) {
          comm.size <- length(members[[x]])
          return(comm.size)
        })
  }
  else if(test == "density") {
    metric.vec <- lapply(community.id,
        function(x) {
          g.sub <- induced.subgraph(graph, members[[x]])
          return(graph.density(g.sub, loops=FALSE))
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
compute.community.metrics <- function(g, comm) {
  res <- list()

  ## Intra-community features
  res$community.v.size <- community.metric(g, comm, "v.size")
  res$intra.betweenness  <- community.metric(g, comm,
                                             "betweenness")
  res$intra.transitivity <- community.metric(g, comm,
                                             "transitivity")
  res$intra.in.deg     <- community.metric(g, comm, "in.deg")
  res$intra.out.deg    <- community.metric(g, comm, "out.deg")
  res$intra.in.weight  <- community.metric(g, comm, "in.weight")
  res$intra.out.weight <- community.metric(g, comm, "out.weight")
  res$intra.diameter   <- community.metric(g, comm, "diameter")
  res$com.density <- community.metric(g, comm, "density")

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
  res$modularity     <- modularity(g, comm$membership)

  ## Global graph features
  global.df <- data.frame()
  res$cluster.coefficient <- transitivity(g, type="local", isolates='zero')
  names(res$cluster.coefficient) <- V(g)$name
  res$betweenness.centrality <- betweenness(g, directed=FALSE, normalize=TRUE)
  res$page.rank <- page.rank(g, directed=FALSE)$vector
  res$average.path.len <- average.path.length(g, directed=FALSE)
  res$v.degree <- sort(igraph::degree(g, mode="all"), decreasing=T)
  res$degree.gini <- ineq(res$v.degree, type="Gini")
  res$core.count <- length(res$v.degree[cumsum(res$v.degree) / sum(res$v.degree) <= 0.8]) / length(res$v.degree)
  res$num.vertices <- vcount(g)
  res$diameter <- diameter(g, weights=NULL)
  res$density <- graph.density(g, loops=FALSE)
  res$ev.cent <- evcent(g, scale=TRUE)$vector
  res$ev.cent.gini <- ineq(res$ev.cent, type="Gini")
  res$adhesion <- graph.adhesion(g, checks=T)
  res$diversity <- graph.diversity(g)
  res$diversity[!is.finite(res$diversity)] <- NA
  res$min.cut <- graph.mincut(g)
  res$edge.vert.ratio <- ecount(g) / vcount(g)

  ## Power-law fiting
  p.fit <- power.law.fit(res$v.degree, implementation="plfit")
  param.names <- c("alpha", "xmin", "KS.p")
  res[param.names] <- p.fit[param.names]
  ## Check percent of vertices under power-law
  res$num.power.law <- length(which(res$v.degree >= res$xmin))
  res$percent.power.law <- 100 * (res$num.power.law / length(res$v.degree))

  ## If less than N developers are in the power law, set x_min manually
  ## to include a minimum of number of developers and recompute the powerlaw fit
  min.devs <- 30
  non.zero.degree.v.count <- length(res$v.degree[res$v.degree > 0])
  if(res$num.power.law < min.devs & non.zero.degree.v.count >= min.devs) {
    ## vertex degree is sorted above
    x.min <- res$v.degree[[min.devs]]
    p.fit <- power.law.fit(res$v.degree, implementation="plfit", xmin=x.min)
    res[param.names] <- p.fit[param.names]

    ## Check percent of vertices under power-law
    res$num.power.law <- length(which(res$v.degree >= res$xmin))
    res$percent.power.law <- 100 * (res$num.power.law / length(res$v.degree))
  }

  ## Remove non conclusive sample sizes
  if(res$num.power.law < min.devs) res$KS.p <- NA

  ## Prepare data to be melted, maintain vertex and cluster ids
  ## by converting named vectors to named lists
  res <- lapply(res, function(x) as.list(x))

  return(res)
}


compute.all.project.trends <- function(con, type, outdir) {
  project.ids <- query.projects(con)$id

  for (p.id in project.ids) {
    trends <- compute.project.graph.trends(con, p.id, type)
    if (length(trends) > 0) {
      write.plots.trends(trends$metrics, trends$markov.chains,
                         trends$developer.classifications,
                         trends$class.edge.probs,
                         outdir)
    } else {
      print("project data frame empty")}
    rm(trends)
  }

  return(0)
}


compute.project.graph.trends <-
  function(conf, type, window.size=90, step.size=14) {
  project.data <- list()
  project.data$p.id <- conf$pid
  project.data$name <- query.project.name(conf$con, conf$pid)
  project.data$analysis.method <- query.project.analysis.method(conf$con, conf$pid)
  range.data <- get.cycles.con(conf$con, conf$pid)
  start.date <- min(range.data$date.start)
  end.date <- max(range.data$date.end)

  loginfo("Processing network evolution for %s", project.data$name)

  if (any(is.na(range.data))) {
    logerror("Database contains insufficient data: %s", range.data)
    stop()
  }

  p.ranges <- compute.sliding.window(start.date, end.date,
                                     step.size, window.size)

  metrics.df <- data.frame()
  e <- new.env()
  e$developer.classes <- list()
  e$developer.edge.probs <- list()
  project.name <- project.data$name
  analysis.method <- project.data$analysis.method

  ## Split up ranges into blocks to be processed in parallel
  n.cores <- get.num.cores()
  chunk.size <- n.cores
  row.ids <- row.names(p.ranges)
  chunks <- split(row.ids, ceiling(seq_along(row.ids) / chunk.size))

  metrics.df <- ldply(chunks, .progress='text',
    function(chunk) {
      ## Select on the ranges for this particular chunk
      p.ranges.chunk <- p.ranges[chunk,]
      ## Get graph and additional data for each revision
      if(type == "developer") {
          conf$type <- "function"
          edgelist.stream <- build.dev.net.stream(conf, p.ranges.chunk)
      }

        revision.data <-
        apply(p.ranges.chunk, 1,
              function(r) {
                res <- list()
                start.date <- r['start.date']
                end.date <- r['end.date']
                cycle <- paste(start.date, end.date, sep='-')
                res$cycle <- cycle

                if (type == "developer") {
                    dev.net <- edgelist.stream[[cycle]]
                    edgelist <- dev.net$edgelist

                    if (!empty(edgelist)) {
                      ## Remove loops
                      edgelist <- edgelist[!(edgelist$to == edgelist$from),]
                    }

                    res$edgelist <- edgelist
                    res$v.global.ids <- dev.net$vertex.data$id
                }
                else if (type == "co-change") {
                  window.start <- ymd(start.date) - ddays(180)
                  edgelist <- get.co.change.edgelist(conf, window.start, end.date)
                  if (!empty(edgelist)) {
                    v.id <- unique(unlist(as.list(edgelist[, c("X1", "X2")])))
                    res$v.global.ids <- v.id
                  }

                  res$edgelist <- edgelist
                }
                else if (type == "semantic") {
                  semantic.coupling <- computeSemanticCouplingCon(con, p.id, start.date, end.date)

                  ## Map vertex number to names
                  X1 <- semantic.coupling$vertex.data$id[semantic.coupling$edgelist$X1]
                  X2 <- semantic.coupling$vertex.data$id[semantic.coupling$edgelist$X2]

                  res$edgelist <- semantic.coupling$edgelist
                  if (!empty(res$edgelist)) res$edgelist$weight <- 1
                  res$v.global.ids <- semantic.coupling$vertex.data$name
                }

                if(empty(res$edgelist)) {
                  sprintf("removing empty cycle %s", cycle)
                  res <- NULL
                }
                else if (FALSE) {
                  ## Compute core developers based on commit counts
                  e$developer.classes[["1"]][[end.date]] <-
                      get.developer.class.con(con, p.id, start.date, end.date,
                                              "VCS", count.type="commit")

                  ## Compute core developers based on loc counts
                  e$developer.classes[["2"]][[end.date]] <-
                      get.developer.class.con(con, p.id, start.date, end.date,
                                              "VCS", count.type="loc")

                  ## Compute core developers based on mail counts
                  e$developer.classes[["3"]][[end.date]] <-
                      get.developer.class.con(con, p.id, start.date, end.date,
                                              "mail", count.type="mail")

                  e$developer.edge.probs[[end.date]] <-
                      compute.edge.probs(e$developer.classes[["4"]][[end.date]], 
                                         res$edgelist, res$v.global.ids)

                  ## Compute core developer based on eigen vector centrality
                  e$developer.classes[["5"]][[end.date]] <-
                      get.developer.class.centrality(res$edgelist, res$v.global.ids,
                                                     source="VCS", metric="page.rank")

                  ## Compute core developer based on hierarchy
                  e$developer.classes[["6"]][[end.date]] <-
                      get.developer.class.centrality(res$edgelist, res$v.global.ids,
                                                     source="VCS", metric="hierarchy")

                  ## Compute core developer based on email network degree
                  email.edgelist <- query.mail.edgelist(con, p.id, start.date,
                                                        end.date)
                  v.global.ids <- unique(c(email.edgelist$from, email.edgelist$to))

                  e$developer.classes[["7"]][[end.date]] <-
                      get.developer.class.centrality(email.edgelist, v.global.ids,
                                                     source="mail", metric="degree")

                  e$developer.classes[["8"]][[end.date]] <-
                      get.developer.class.centrality(email.edgelist, v.global.ids,
                                                     source="mail", metric="page.rank")
                } else {
                  ## Compute core developers based on degree centrality
                  e$developer.classes[["4"]][[end.date]] <-
                      get.developer.class.centrality(res$edgelist, res$v.global.ids,
                          source="VCS", metric="degree")
                }

                return(res)})

        revision.data[sapply(revision.data, is.null)] <- NULL

      ## Create igraph object and select communities which are of a minimum size 4
      revision.data <-
        mclapply(revision.data, mc.cores=n.cores,
                 function(rev) {
                   graph <- graph.data.frame(rev$edgelist,
                                             directed=TRUE,
                                             vertices=data.frame(rev$v.global.ids))
                   V(graph)$name <- rev$v.global.ids
                   E(graph)$weight <- log(1 + E(graph)$weight)
                   comm <- community.detection.disconnected(graph,
                                                            spinglass.community.connected)
                   graph.comm <- minCommGraph(graph, comm, min=1)
                   rev$graph <- graph
                   rev$comm <- comm

                   if(is.null(rev$comm)) {
                     rev <- NULL
                   }

                   return(rev)})

      ## Remove revisions that don't have graphs
      revision.data[sapply(revision.data, is.null)] <- NULL

      ## Compute network metrics
      revision.df.list <-
        mclapply(revision.data, mc.cores=n.cores,
                 function(rev) {
                   df <- melt(compute.community.metrics(rev$graph, rev$comm))
                   df[,names(project.data)] <- project.data
                   df$cycle <- rev$cycle
                   df <- rename(df, c("L1"="metric", "L2"="g.id"))
                   return(df)})

      res <- do.call("rbind", revision.df.list)

      return(res)})

  ## Merge developer classification
  developer.classifications <- list()
  for (type in names(e$developer.classes)) {
    developer.classifications[[type]] <- melt(e$developer.classes[[type]],
                                              c("author", "class", "metric"))
  }

  ## Compute Markov chains
  if(length(e$developer.classes[["4"]]) > 1) {
    markov.chain.centrality <- compute.class.markov.chain(e$developer.classes[["4"]])
    markov.chains <- list(markov.chain.centrality=markov.chain.centrality)
  }
  else {
    loginfo("Less than 2 revisions, unable to perform turn-over analysis")
    markov.chains <- NULL
  }

  res <- list(metrics=metrics.df, markov.chains=markov.chains,
              developer.classifications=developer.classifications,
              class.edge.probs=e$developer.edge.probs,
              developer.class.list=e$developer.classes[["4"]])

  return(res)
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

  data <- data.frame(cycle, page.rank=ranks)
  rank.mean <- aggregate(data$page.rank, by=list(cycle=data$cycle), mean)
  rank.sd <- aggregate(data$page.rank, by=list(cycle=data$cycle), sd)
  browser()

  plot1 <- ggplot(data, aes(x=page.rank, colour=cycle)) + geom_density()
  return(plot1)
}


plot.box <- function(project.df, feature, outdir) {
  ## Select all rows for the feature
  keep.row <- project.df$metric == feature
  project.df <- project.df[keep.row,]

  if(nrow(project.df) != 0) {
    project.name <- unique(project.df$name)
    analysis.method <- unique(project.df$analysis.method)

    p0 <- ggplot(project.df, aes(x=cycle, y=value)) +
          geom_boxplot(outlier.shape = NA) + ylab(feature) +
          xlab("Revision") + labs(title=project.name) + expand_limits(y=0) +
          theme(axis.text.x = element_text(family="Arial Narrow",
                                           colour="black",size=12,angle=60,
                                           hjust=.6,vjust=.7,face="plain"))
    up.lim <- max(unlist(lapply(split(project.df$value, project.df$cycle),
                                function(x) boxplot.stats(x)$stats[c(2,4)])), na.rm=TRUE)
    ylim1 <- boxplot.stats(project.df$value)$stats[c(2,4)]
    ylim1[2] <- up.lim
    ylim1[1] <- 0
    p1 = p0 + coord_cartesian(ylim = ylim1*1.05)

    file.dir <- paste(outdir, "/", project.name, "_", analysis.method, sep="")
    dir.create(file.dir, recursive=T)
    file.name <- paste(file.dir, "/", feature, ".png",sep="")
    ggsave(file.name, p1, height=8, width=20)

    ## ## Adjusted box plots for skewed data
    ## file.name <- paste(file.dir, "/", feature, "_adjusted.pdf", sep="")

    ## pdf(file.name)

    ## adjbox(value ~ cycle, data=project.df, outline=FALSE)

    ## ## x axis with ticks but without labels
    ## axis(1, labels = FALSE)

    ## dev.off()

    if(feature %in% c('page.rank','v.degree')) {
      file.name <- paste(file.dir, '/', feature, "_distribution.pdf", sep="")
      p2 <- ggplot(project.df, aes(x=value)) +
            geom_histogram(aes(y=..density..),colour="black", fill="white") +
            geom_density(alpha=.2, fill="#FF6666")
      ggsave(file.name, p2, height=8, width=20)
    }
  }
}

plot.series <- function(project.df, feature, outdir) {
  ## Select all rows for the feature
  keep.row <- project.df$metric %in% feature
  project.df <- project.df[keep.row,]
  x.labels <- unique(project.df$cycle)
  n.cycles <- length(x.labels)
  project.df$cycle <- as.factor(project.df$cycle)

  if(nrow(project.df) != 0) {
    project.name <- unique(project.df$name)
    analysis.method <- unique(project.df$analysis.method)

    p <- ggplot(project.df, aes(x=cycle, y=value)) +
                geom_point(color= I('black')) +
                geom_line(aes(group=name)) +
                facet_wrap(~ metric, ncol=1, scales="free_y") +
                ylab("Value") +
                xlab("Revision") +
                scale_x_discrete(breaks=x.labels[seq(from=1, to=n.cycles, by=25)]) +
                labs(title=project.name) + expand_limits(y=0) +
                theme(axis.text.x = element_text(family="Arial Narrow",
                                                 colour="black",size=12,
                                                 face="plain", angle=45,
                                                 hjust=0.5),
                      strip.text.x = element_text(size=15))
  }

  file.dir <- paste(outdir, "/", project.name, "_", analysis.method, sep="")
  dir.create(file.dir, recursive=T)
  file.name <- paste(file.dir, "/time_series_metrics.png",sep="")
  ggsave(file.name, p, height=41, width=20)
}


plot.scatter <- function(project.df, feature1, feature2, outdir) {
  ## Select all rows for the feature
  keep.row.f1 <- project.df$metric == feature1
  keep.row.f2 <- project.df$metric == feature2
  cols <- names(project.df)
  join.cols <- cols[!(cols %in% c("value", "metric"))]
  project.df <- merge(project.df[keep.row.f1,],
                      project.df[keep.row.f2,],
                      by=join.cols)

  rmv.row <- (project.df$value.x == 0) | (project.df$value.y == 0)
  project.df <- project.df[!rmv.row,]

  if(nrow(project.df) != 0) {
   project.name <- unique(project.df$name)
   analysis.method <- unique(project.df$analysis.method)

   p <- ggplot(data=project.df, aes(x=value.x,y=value.y)) +
        geom_point() +
        xlab(feature1) + ylab(feature2) +
        scale_y_log10() + scale_x_log10() +
        facet_wrap( ~ cycle) +
        geom_smooth(method="lm")

    file.dir <- paste(outdir, "/", project.name, "_", analysis.method, sep="")
    dir.create(file.dir, recursive=T)
    file.name <- paste(file.dir, "/", feature1, "_vs_", feature2, ".png",sep="")
    ggsave(file.name, p, height=40, width=40)
  }
}

plot.class.match <- function(class.match.df, class.rank.cor, filename) {
  rank.cor.text <- paste("Correlation:", signif(class.rank.cor,3), sep=" ")
  class.match.df$date <- as.Date(class.match.df$date)
  match.plot <- ggplot(data=class.match.df, aes(y=value, x=date)) +
                      stat_smooth(aes(group=metric, color=metric), fill="grey65", level=0.95,
                                  size=0.5) +
                       geom_point(aes(color=metric), size=1) +
                       scale_x_date(labels = date_format("%Y"),
                       breaks = "2 year", expand=c(0,0)) +
                       scale_y_continuous(limits=c(0, 1)) +
                       ylab("Percent Agreement") +
                       ggtitle(rank.cor.text)

  ggsave(plot=match.plot, filename=filename, width=7, height=5)
}

write.plots.trends <- function(trends, markov.chains, developer.classifications,
                               class.edge.probs, outdir) {
  metrics.box <- c('cluster.coefficient',
                   'betweenness.centrality',
                   'conductance',
                   'page.rank',
                   'community.v.size',
                   'v.degree',
                   'ev.cent',
                   'diversity')

  metrics.series <- c('num.communities',
                      'num.vertices',
                      'percent.power.law',
                      'modularity',
                      'density',
                      'alpha',
                      'xmin',
                      'KS.p',
                      'degree.gini',
                      'num.power.law',
                      'edge.vert.ratio')


  ## Generate and save box plots for each project
  dlply(trends, .(p.id), function(df) sapply(metrics.box, function(m)
        plot.box(df, m, outdir)))

  ## Generate and save series plots
  dlply(trends, .(p.id), function(df) plot.series(df, metrics.series, outdir))

  ## Gernerate scatter plots
  dlply(trends, .(p.id), function(df) plot.scatter(df, "v.degree",
        "cluster.coefficient", outdir))

  project.name <- unique(trends$name)
  analysis.method <- unique(trends$analysis.method)

  file.dir <- paste(outdir, "/", project.name, "_", analysis.method, sep="")

  ## Save markov chain plot
  if(!is.null(markov.chains)) {
    chain.types <- names(markov.chains)
    for (type in chain.types) {
      filename <- paste(file.dir, "/", type, ".pdf", sep="")
      pdf(file=filename)
      plot(markov.chains[[type]], margin=0.25)
      dev.off()
    }
  }

  ## Compute all classification match stats
  all.agreement <- list()
  if (length(developer.classifications) > 1) {
    all.agreement <- compare.classification.all(developer.classifications)
  }

  ## Save data to file
  data <- list(trends=trends,markov.chains=markov.chains,
               developer.classifications= developer.classifications,
               class.edge.probs=class.edge.probs,
               all.agreement=all.agreement)

  save(data, file=paste(file.dir, "/project_data.dat",sep=""))

}


run.trends.analysis <- function (con) {
  base.dir <- "/home/mitchell/trends"
  types <- c("developer", "co-change")

  sapply(types,
    function(type) {
      outdir <- paste(base.dir, type, sep="/")
      compute.all.project.trends(con, type, outdir)
    })

  return(0)
}

remove.outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.90), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x > (qnt + H)] <- NA
  return(y)
}

save.as.graphml <- function(project.data, outdir) {
  project.name <- project.data[[1]]$project.name
  analysis.method <- project.data[[1]]$analysis.method
  file.dir <- paste(outdir, "/", project.name, "_", analysis.method, "/", "GraphML",sep="")
  dir.create(file.dir)
  graph.list <- lapply(project.data,
                          function(g) return(g$graph))

  sapply(1:length(graph.list), function(i) {
                                filename <- paste(file.dir, "/", "graph_", as.character(i), ".graphml", sep="")
                                 write.graph(graph.list[[i]], file=filename, format="graphml")})
}
