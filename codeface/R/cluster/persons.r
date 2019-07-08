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
suppressPackageStartupMessages(library(graph))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(xtable))
suppressPackageStartupMessages(library(logging))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(plyr))
source("../utils.r", chdir=TRUE)
source("../config.r", chdir=TRUE)
source("../db.r", chdir=TRUE)
source("../query.r", chdir=TRUE)
source("community_metrics.r")
source("community_helpers.r")
source("network_visualization.r")

#######################################################################
##		Lower Level Functions
#######################################################################

##========================================================================
##		Utility
##========================================================================
read.oslom <- function(input.file){
  ## reads a file created by the oslom clustering program
  con   	 <- file(input.file, open='r')
  comms 	 <- list() 
  membership <- c()
  csize      <- c()
  res		 <- list()
  while(length(line <- readLines(con, n=1, warn=FALSE)) > 0) {
    if ( "#" == substring(line,1,1)) {
	  next
	}
	else{
      comm.str <- (strsplit(line, " "))
      comm.int <- list(as.numeric(comm.str[[1]]))
      comms    <- c(comms,comm.int)
	}
  }
  close(con)  

  ##TODO: change this to support overlapping communities, ATM nodes split
  ##			between two clusters end up arbitrarily in one
  ## create a igraph communities style object
  for (i in 1:length(comms)){
    verts <- comms[[i]]
    membership[verts] <- i
  }
  membership.conseq <- remapConsecSeq(membership)
  for (i in 1:length(unique(membership.conseq))) {
    csize[i] <- length(which(membership.conseq == i))
  }
  class(res)     <- "communities"
  res$membership <- membership.conseq 
  res$csize	     <- csize
  res$algorithm  <- "OSLOM"
  return(res)
}


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
  res <- res[order(res[c(.field)], decreasing=TRUE),]
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

four.digit <- function(n) {
  loc <- as.character(n)

  if (nchar(loc) != 4) {
    loc <- paste(paste(rep("0", 4-nchar(loc)), collapse=''), loc, sep="")
  }

  return(loc)
}

col.to.hex <- function(prefix="0x", r,g,b) {
  return (paste(prefix, int.to.hex(r), int.to.hex(g), int.to.hex(b), sep=""))
}


## Return indices of vertices that are in the largest connected subgraph
largest.subgraph.idx <- function(graph) {
  ## Find all connected subgraphs
  g.clust <- clusters(graph)

  ## Select the id of the largest cluster, and find all mathing indices
  clusters <- data.frame(id=1:length(g.clust$csize), size=g.clust$csize)
  clusters <- clusters[sort(clusters$size, index.return=TRUE, decreasing=TRUE)$ix,]
  id.largest <- clusters$id[1]

  ## Get all indices of connected developers for the largest cluster
  idx <- which(g.clust$membership==id.largest)

  return(idx)
}


largest.subgraph <- function(graph) {
  ##############################################################################
  ## Returns a graph composed only of the largest connected component
  ## - Input -
  ## graph: igraph object
  ## - Ouput -
  ## graph.connected: igraph object, composed of the largest connected component
  ##                  provided by the input graph
  ##############################################################################
  ## Get all vertices that exist in the largest connected component
  idx <- largest.subgraph.idx(graph)
  ## Get vertices to remove
  idx.rmv <- setdiff(V(graph),idx)

  ## Remove all vertices that are not in the largest connected component
  graph.connected <- delete.vertices(graph, idx.rmv)

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
oslom.community <- function(g) {
  ## uses the OSLOM progam to generate an igraph-like communities object
  ## Args:
  ##  g: igraph graph object
  ## Returns:
  ##  community: igraph-like communities object
  ##TODO: we need to figure out how to set this up better w.r.t. where the
  ##      OSLOM program files should be stored
  prog.loc <- getwd()
  file.name <- paste(prog.loc, "/oslom.dat", sep="")
  ## write graph to file
  g.frame <- get.data.frame(g, what="edges")
  g.frame["weight"] <- E(g)$weight
  write.table(g.frame, file.name, sep="\t", row.names=FALSE, col.names=FALSE)

  ## make system call to oslom
  oslom.prog <- paste(prog.loc, "/oslom_undir -w -t 1.0 -cp 1.0 -copra 10 -infomap 10 -f", sep="")
  cmd <- paste(oslom.prog, file.name, sep=" ")
  system(cmd, ignore.stdout=TRUE)

  ## read output file
  community <- read.oslom(paste(file.name, "_oslo_files/tp_without_singletons", sep=""))
  return (community)
}


link.community <- function(g){
  #########################################
  ## Description:
  ##   Utilize linkcomm package to perform network decomposition
  ##   provided with an igraph graph object
  #########################################
  ## construct a directed and weighted edge list
  edge.list.dir.we <- data.frame( cbind(get.edgelist(g), E(g)$weight))
  ## perform decomposition
  link.communities <- getLinkCommunities(edge.list.dir.we, hcmethod="single",
										 directed=TRUE, plot=FALSE, verbose=FALSE)
  ## create an igraph community object to return results in accordance with
  ## existing infrastructure in handeling communities
  node.clust <- link.communities$nodeclusters
  num.clust <- link.communities$numbers[3]
  membership <- list()
  csize      <- c()
  for (i in 1:num.clust) {
    membership[[i]] <- as.numeric(as.character(node.clust[node.clust$cluster == i,1]))
	csize[i] <- length(membership[[i]])
  }
  membership$csize <- csize
  class(membership) <- "overlapComm"
  return(membership)
}


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
         theme(axis.text.x=element_text(angle=-90, hjust=0)) +
         labs(title=paste("Subsystem distribution for community clusters (algorithm: ", .alg,
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


## Given a single cluster of persons, construct an igraph object,
## compute some attributes for proper visualisation, and export the
## result as a graphviz dot format if a filename is provided.
save.group <- function(conf, .tags, .iddb, idx, .prank, .filename=NULL, label) {
  ## Select the subset of the global graph that forms the community,
  ## and ensure that the per-cluster indices range from 1..|V(g.cluster)|
  subset <- .tags[idx, idx]

  ## A 1x1 matrix is not of class matrix, but of class numeric,
  ## so ncol() won't work any more in this case. Ensure that we are actually
  ## working with a matrix before explicitely setting the row and column
  ## names to consecutive numbers.
  if (class(subset) == "matrix") {
    rownames(subset) <- 1:ncol(subset)
    colnames(subset) <- 1:ncol(subset)
  }

  g <- graph.adjacency(subset, mode="directed", weighted=TRUE)

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
    ## Scale edge weights, extremely large weights will cause graphviz to
    ## fail during rendering
    g.scaled <- g
    E(g.scaled)$weights <- scale.data(log(E(g.scaled)$weights + 1), 0, 100)
    
    write.graph(g.scaled, .filename, format="dot")
  }

  return(g)
}


## Prepare graph data for database and insert
store.graph.db <- function(conf, baselabel, idx, .iddb, g.reg, g.tr, j) {
  ## Construct a systematic representation of the graph for the data base
  edges <- get.data.frame(g.reg, what="edges")
  colnames(edges) <- c("fromId", "toId")
  edges$fromId <- as.integer(edges$fromId)
  edges$toId <- as.integer(edges$toId)

  ## NOTE: Index handling is somewhat complicated: idx contains a set
  ## of indices generated for the global graph. .iddx[index,]$ID.orig
  ## maps these back to the in-DB indices.
  ## V(g) for the current graph uses another different indexing system:
  ## indices are 1..|V(g)|. To convert from the graph-local indices
  ## to the graph-global ones, use idx[V(g)]. To convert these to in-DB
  ## indices, use .iddb[idx[V(g)]]$ID.org.

  edges$toId <- .iddb[idx[edges$toId],]$ID.orig
  edges$fromId <- .iddb[idx[edges$fromId],]$ID.orig

  ## Create a weighted edgelist, and associate it with the in-cluster
  ## database id
  if (dim(edges)[1] > 0) {
    ## Only write an edge list if the cluster has any edges, actually
    edges <- gen.weighted.edgelist(edges)
    write.graph.db(conf, conf$range.id, baselabel, edges, j)
  }
}


## Iterate over all clusters in a community decomposition, create
## a graphviz input file (via save.groups), and also store the
## cluster into the database.
## NOTE: The decomposition into clusters is identical for the
## regular and transposed pagerank, only the vertex _attributes_ (but not
## the vertices as such) differ. Edges are identical in all respects.
## Consequently, we only need to write into tables cluster_user_mapping
## and edgelist once.
save.groups <- function(conf, .tags, .iddb, .comm, .prank.list, .basedir,
                        .prefix, comm.quality, label) {
  baselabel <- label
  label.tr <- NA
  j <- 0

  for (i in unique(.comm$membership)) {
    filename.reg <- paste(.basedir, "/", .prefix, "reg_", "group_", four.digit(i),
                          ".dot", sep="")
    filename.tr <- paste(.basedir, "/", .prefix, "tr_", "group_", four.digit(i),
                          ".dot", sep="")

    if (class(.comm) == "communities") {
      idx <- as.vector(which(.comm$membership==i))

      ## Do not store clusters of size one
      if(length(idx) < 2) {
        next
      }
    }
    else if(class(.comm) == "overlapComm") {
      idx <- as.vector(.comm[[i]])
    }

    if (!is.na(baselabel)) {
      label <- paste(baselabel, i, "Community Quality = ", comm.quality[i],
			         sep=" ")
      label.tr <- paste(baselabel, "(tr)", i,  "Community Quality = ",
			            comm.quality[i], sep=" ")
    }
    g.reg <- save.group(conf, .tags, .iddb, idx, .prank.list$reg,
                        filename.reg, label)
    g.tr <- save.group(conf, .tags, .iddb, idx, .prank.list$tr,
                       filename.tr, label.tr)

    ## Store the cluster content into the database
    if (!is.na(baselabel)) {
      store.graph.db(conf, baselabel, idx, .iddb, g.reg, g.tr, j)
      j <- j + 1
    }
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
          floating=FALSE, file=paste(.outdir, "/", .basename, four.digit(i), ".tex", sep=""))
  }
}

save.all <- function(conf, .tags, .iddb, .prank.list, .comm, .filename.base=NULL,
                     label) {
  g.all.reg <- save.group(conf, .tags, .iddb, .iddb$ID, .prank.list$reg,
                          .filename=NULL, label=NA)
  g.all.tr <- save.group(conf, .tags, .iddb, .iddb$ID, .prank.list$tr,
                         .filename=NULL, label=NA)

  ## NOTE: The all-in-one graphs get a different suffix (ldot for "large
  ## dot") so that we can easily skip them when batch-processing graphviz
  ## images -- they take a long while to compute

  V(g.all.reg)$label <- .iddb$ID
  V(g.all.reg)$pencolor <- V(g.all.reg)$fillcolor

  V(g.all.tr)$label <- .iddb$ID
  V(g.all.tr)$pencolor <- V(g.all.reg)$fillcolor

  elems <- unique(.comm$membership)
  red <- as.integer(scale.data(0:(length(elems)+1), 0, 255))
  ##  grey <- as.integer(scale.data(0:(length(elems)+1), 0, 99))
  for (i in elems) {
    idx <- as.vector(which(.comm$membership==i))

    V(g.all.reg)[idx]$fillcolor <- col.to.hex("#", red[i+1], 0, 0)
    V(g.all.tr)[idx]$fillcolor <- col.to.hex("#", red[i+1], 0, 0)
  }

  if (!is.na(label)) {
    g.all.reg$label = label
    g.all.tr$label = label

    ## The global graph gets community index -1
    idx <- 1:length(.iddb$ID) ## Select all elements
    store.graph.db(conf, label, idx, .iddb, g.all.reg, g.all.tr, -1)
  }

  if (!is.null(.filename.base)) {
    filename.reg <- paste(.filename.base, "reg_all.ldot", sep="")
    filename.tr <- paste(.filename.base, "tr_all.ldot", sep="")

    write.graph(g.all.reg, filename.reg, format="dot")
    write.graph(g.all.tr, filename.tr, format="dot")
  }

  ## Community visualization
  filename.comm <- paste(.filename.base, "community.ldot", sep="")
  save.graph.graphviz(conf$con, conf$pid, conf$range.id, label, filename.comm)
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

store.pageranks <- function(conf, .iddb, devs.by.pr, range.id, technique) {
  ## First, create an entry in table pagerank to get the DB internal
  ## id for the releaseRangeId/technique tuple
  prank.id <- get.clear.pagerank.id(conf, range.id, technique)

  dat <- devs.by.pr[,c("ID", "rank")]
  colnames(dat) <- c("personId", "rankValue")
  ## Convert personId to in-DB values from the local indices
  dat$personId <- .iddb[dat$personId,]$ID.orig
  dat$pageRankId <- prank.id

  res <- dbWriteTable(conf$con, "pagerank_matrix", dat, append=TRUE, row.names=FALSE)
  if (!res) {
    stop("Internal error: Could not write pagerank matrix into database!")
  }
}

writePageRankData <- function(conf, outdir, .iddb, devs.by.pr, devs.by.pr.tr) {
  ## Top 20 page rank (focus on giving tags)
  print(xtable(devs.by.pr[1:20,]), type="latex", floating=FALSE,
        file=paste(outdir, "/top20.pr.tex", sep=""),
        sanitize.colnames.function=rotate.label.30)

  ## Top 20 page rank (focus on being tagged)
  print(xtable(devs.by.pr.tr[1:20,]), type="latex", floating=FALSE,
        file=paste(outdir, "/top20.pr.tr.tex", sep=""),
        sanitize.colnames.function=rotate.label.30)

  ## Emit the results into the database
  store.pageranks(conf, .iddb, devs.by.pr, conf$range.id, 0)
  store.pageranks(conf, .iddb, devs.by.pr.tr, conf$range.id, 1)
}

#########################################################################
##     					 Main Functions
#########################################################################

performAnalysis <- function(outdir, conf) {
  ################## Process the data #################
  logdevinfo("Reading files", logger="cluster.persons")
  mat.file <- paste(outdir, "/adjacencyMatrix.txt", sep="")
  adjMatrix <- read.table(mat.file, sep="\t", header=TRUE)
  adjMatrix.ids <- unlist(strsplit(readLines(mat.file, n=1), "\t"))

  colnames(adjMatrix) <- rownames(adjMatrix)

  ## The adjacency matrix file format uses a different convention for edge
  ## direction than GNU R, so we need to transpose the matrix
  adjMatrix <- t(adjMatrix)

  ids.db <- get.range.stats(conf$con, conf$range.id)

  ## Check that ids are in correct order, the ids queried from the
  ## db are not necessarily in the same order as the adjacency matrix
  ## columns. Here we remap ids to the correct order.
  remapping <- unlist(lapply(adjMatrix.ids, function(id) {which(id == ids.db$ID)}))
  ids <- ids.db[remapping,]
  if(!all(ids$ID==adjMatrix.ids)) {
      logerror("Id mismatch", logger="cluster.persons")
  }

  id.subsys <- read.csv(file=paste(outdir, "/id_subsys.txt", sep=""),
			sep="\t", header=TRUE)
  id.subsys$ID <- id.subsys$ID + 1

  ## If there are only two column names (ID and general), then
  ## the project is not equipped with an explicit subsystem
  ## description.
  if (length(colnames(id.subsys)) == 2) {
    id.subsys <- NULL
  }
  
  ## Get off diagonal sum  
  mat.diag <- diag(adjMatrix) 
  diag(adjMatrix) <- 0
  non.diag.sum <- sum(adjMatrix)
  diag(adjMatrix) <- mat.diag

  if(sum(non.diag.sum) == 0) {
    loginfo("Adjacency matrix empty, exiting cluster analysis", logger="cluster.persons")
    return(1)
  } else {
    performGraphAnalysis(conf, adjMatrix, ids, outdir, id.subsys)
  }
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

## Detect, visualise and save communities with clustering algorithm FUN
## (for instance spinglass.communities or walktrap.communities)
detect.communities <- function(g, ids, adjMatrix, prank.list, outdir,
                               prefix, label, min.fract, upper.bound, FUN) {
  set.seed(42)
  if (vcount(g) == 1) {
    ## If there is only one vertex in the graph (which can happen for
    ## very short bug-fix cycles when a single contributor interacts
    ## with the repo), it does not make sense to try detecting communities.
    ## (besids, spinglass community detection would run into an infinite
    ## loop in this case).
    ## Note: We don't construct a community with one member, but choose
    ## the interpretation that there are no communities in this case.
    ## This possibility needs to be supported anyway because the OSLOM
    ## clustering method, for instance, can fail to detect significant communities
    ## even for larger graphs if there aren't any.
    g.community <- NULL
    elems.selected <- logical(0)
  } else {
    g.community <- community.detection.disconnected(g, FUN)
    ## compute community quality
    comm.quality <- community.metric(g, g.community, "conductance")
  }

  logdevinfo(str_c("Writing community graph sources for algorithm ", label), logger="cluster.persons")
  ## NOTE: The cluster decomposition is independent of the page
  ## rank calculation technique -- only the edge strengths, but not the
  ## page rank values influence the decomposition.
  clear.all.clusters(conf, conf$range.id, label)
  save.groups(conf, adjMatrix, ids,
              g.community, prank.list, outdir, prefix,
			  comm.quality, label=label)
  return(g.community)
}


performGraphAnalysis <- function(conf, adjMatrix, ids, outdir, id.subsys=NULL){
  ##====================================
  ##     Find Connected Subgraphs
  ##====================================
  ## Scale edge weights to integer values
  ## adjMatrix <- ceiling(scale.data(adjMatrix, 0, 1000))

  ## Remove loops
  ## Loops indicate a nodes collaboration with iteself, this
  ## information could be used to identify isolated hero developers by looking
  ## at the relative collaboration with others vs. their loop edge weight.
  ## At this point we have no use for loops and the large edge weight of loops
  ## leads to additional challenges in the analysis and visualization. For
  ## now we will just remove the loops.
  n <- ncol(adjMatrix)
  adjMatrix <- adjMatrix * abs(diag(1, n, n) - 1)

  logdevinfo("Computing adjacency matrices", logger="cluster.persons")
  g <- graph.adjacency(adjMatrix, mode="directed", weighted=TRUE)
  idx <- V(g)

  ## Working with the adjacency matrices is easier if the IDs are numbered
  ## consecutively. We need to be able to map the consecutive (local) ids
  ## back to the (global) ids used in the data base later on, so keep
  ## a mapping by storing a copy in ID.orig.
  ids$ID.orig <- ids$ID
  ids$ID=seq(1:length(idx))
  ids$Name <- as.character(ids$Name)

  if (!is.null(id.subsys)) {
    id.subsys <- id.subsys[idx,]
    id.subsys$ID=seq(1:length(idx))
  }

  V(g)$label <- as.character(ids$Name)

  ## TODO: Include computing classical statistics from performTagAnalysis

  ##========================
  ##  Page rank analysis
  ##========================

  ## Compute the page ranking for all developers in the database
  logdevinfo("Computing page rank", logger="cluster.persons")
  ## This puts the focus on tagging other persons
  pr.for.all <- compute.pagerank(adjMatrix, transpose=TRUE,
                                 weights=TRUE)
  ## ... and this on being tagged.
  pr.for.all.tr <- compute.pagerank(adjMatrix, .damping=0.3,
                                    weights=TRUE)

  ## NOTE: pr.for.all$value should be one, but is 0.83 for some
  ## reason. This seems to be a documentation bug, though:
  ## https://bugs.launchpad.net/igraph/+bug/526106
  devs.by.pr <- influential.developers(NA, pr.for.all, adjMatrix,
                                       ids)

  devs.by.pr.tr <- influential.developers(NA, pr.for.all.tr,
                                          adjMatrix, ids)

  ##-----------
  ##save data
  ##-----------
  writePageRankData(conf, outdir, ids, devs.by.pr, devs.by.pr.tr)

  logdevinfo("Computing classical statistics", logger="cluster.persons")
  writeClassicalStatistics(outdir, ids)

  ## Parameters for removing too small communities; see select.communities
  ## for details (TODO: Should we make this configurable?)
  MIN.CUT.FRACTION <- 0.95
  MAX.CUT.SIZE <- 10
  ##=======================
  ## Find Communities
  ##=======================
  ## NOTE: We don't use weighted graphs because not all clustering algorithms
  ## support these; they weights are implicitly given by repeated edges in
  ## any case.

  ## Scale the weight in the adjacency matrix for propers visualization
  ## graphviz requires integer edge weights adjMatrix.connected.scaled =
  ## round( scale.data(adjMatrix.connected, 0, 1000) )

  ##--------------------
  ##infomap
  ##--------------------
  ##g.infomap.community <- infomap.community(g.connected)
  ##g.walktrap.community <- infomap.community(g.connected)

  logdevinfo("Inferring communities with spin glasses", logger="cluster.persons")
  g.spin.community <- detect.communities(g, ids,
                                         adjMatrix,
                                         list(reg=pr.for.all, tr=pr.for.all.tr),
                                         outdir, "sg_", "Spin Glass Community",
                                         MIN.CUT.FRACTION, MAX.CUT.SIZE,
                                         spinglass.community.connected)

  logdevinfo("Inferring communities with random walks", logger="cluster.persons")
  g.walktrap.community <- detect.communities(g, ids,
                                             adjMatrix,
                                             list(reg=pr.for.all, tr=pr.for.all.tr),
                                             outdir, "wt_", "Random Walk Community",
                                             MIN.CUT.FRACTION, MAX.CUT.SIZE,
                                             walktrap.community)

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
  logdevinfo("Writing the all-developers graph sources", logger="cluster.persons")

  save.all(conf, adjMatrix, ids,
           list(reg=pr.for.all, tr=pr.for.all.tr),
           g.spin.community,
           paste(outdir, "/sg_", sep=""),
           label="Spin Glass Community")
  save.all(conf, adjMatrix, ids,
           list(reg=pr.for.all, tr=pr.for.all.tr),
           g.walktrap.community,
           paste(outdir, "/wt_", sep=""),
           label="Random Walk Community")
}


get.community.graph <- function(graph, community, prank, ids, outdir) {
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
  write.graph(g.simplified, outdir, format="dot")
}

runRandCompare <- function(nonTagDir, tagDir, outfile) {
  ## Read files for ids and adjacency matrix
  nonTagAdjMatrix <- read.table(file=paste(nonTagDir, "/adjacencyMatrix.txt",
                                  sep=""), sep="\t", header=FALSE)
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
  graphComparison(nonTagAdjMatrixRand, ids.nonTag, tagAdjMatrix,
                  ids.Tag, outfile)
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

}

#########################################################################
##     					 Testing Section
#########################################################################
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
  modularity.result        <- sum(community.metric(g, g.spincommunity, "modularity"))
  if( !(igraph.modularity.result == modularity.result)){
    logerror("modularity test failed", logger="cluster.persons")
  }
  else{
    logdevinfo("Success: modularity test passed", logger="cluster.persons")
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

  quality <- community.metric(g, g.clust, "modularization")

}

#########################################################################
##     					 Executed Statements
#########################################################################
##----------------------------
## Parse commandline arguments
##----------------------------

config.script.run({
  conf <- config.from.args(positional.args=list("resdir", "range.id"),
                           require.project=TRUE)
  performAnalysis(conf$resdir, conf)
})
