#! /usr/bin/env Rscript
# Analyse the developer connections

# This file is part of prosoda.  prosoda is free software: you can
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
# Copyright 2010, 2011, 2012 by Wolfgang Mauerer <wm@linux-kernel.net>
# All Rights Reserved.

# TODO: Can we deploy the page rank idea to patches as well?
# For instance, we could interpret that patches are "touched" by
# files, so patches involving more heaviliy patched files would
# receive higher scores. Or we could also determine how many
# people touched a particular piece of code (to simplify things,
# this computation could be done on the file level in a first stage),
# and use this as basis for the patch evaluation.
# NOTE: To compare the results to traditional metric, we could
# also use anonymised names to not insult anyone.
# TODO: Could we use graph cohesion as a more mature (and
# mathematically sound) replacement to OOP cohesion?
# TODO: Another idea is also categorisation by subsystems.
#       (i.e., use a known/suspected distribution into subsystems
#        for learning, and then apply the inferred clustering on
#        the complete data set to infer subsystems in a quasi-Bayesian
#        way)
# Perform a full analysis:
# 1.) Call ./cluster.py with the appropriate revision settings
# 2.) ./persons.r /Users/wolfgang/papers/csd/cluster/res/<rev>/
# 3.) for file in `ls prefix*.dot`; do
#        basefile=`basename $file .dot`;
#        echo "Processing $file";
#        cat $file | ../../conv.py | neato (or sfp, or sfdp) -Tpdf > ${basefile}.pdf;
#     done
# 4.) Use gen_images.sh to create all PDFs with dot
library(igraph)
library(stringr)
library(xtable)
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(optparse))



#######################################################################
#						Lower Level	Functions
#######################################################################

#========================================================================
#   						Utility 
#========================================================================

status <- function(str) {
	cat(paste("\r", paste(rep(" ", 75), collapse=''), sep=""))
	cat(paste("\r", str, sep=""))
}

# Given an eMail address like "Name N. Surname <name.surname@domain.com>",
# extract the person name without the electronic address
name.of.email <- function(str) {
	return(str_sub(str, 1, str_locate(str, "<")[1]-2))
}

# NOTE: We rely that the ids are already sorted numerically. To ensure
# this is really the case, a check could do no harm
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

# Scale a given data set to the range [min,max]
scale.data <- function(dat, .min=0, .max=1) {
	dat <- dat - min(dat)
	dat <- dat/max(dat)*(.max-.min)
	dat <- dat + .min
	return(dat)
}

rotate.label <- function(x) {
	return(paste("\\rotatebox{60}{", x, "}", sep=""))
}

rotate.label.30 <- function(x) {
	return(paste("\\rotatebox{30}{", x, "}", sep=""))
}

txt.comm.subsys <- function(.comm, .id.subsys, i) {
	idx <- which(.comm$membership==i)
	
#  return(summary(.id.subsys[idx,2:dim(.id.subsys)[2]]))
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


#========================================================================
#    						Edge Functons
#========================================================================


# These two function return the absolute number of tags received and given.
# If two persons exchange tags multiple times, this is counted repeatedly
tags.received <- function(.id, .tags) {
	return(sum(.tags[,.id]))
}

tags.given <- function(.id, .tags) {
	return(sum(.tags[.id,]))
}

# These functions, in turn, compute from/to how many _different_ developers
# a tag was given to/received from
tags.received.norep <- function(.id, .tags) {
	return(length(which(.tags[,.id]>0)))
}

tags.given.norep <- function(.id, .tags) {
	return(length(which(.tags[.id,]>0)))
}



#========================================================================
# 						Community Detection
#========================================================================
# Clique percolation. Stolen from http://igraph.wikidot.com/community-detection-in-r
# Does not work on our graph. Maybe it works in an undirected version
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

# Was not particularly useful on some test graphs (essentially, the graph
# is split into two halves, with some very small pieces in addition))
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


# Select communities with more than .min members
select.communities.more <- function(.comm, .min) {
	N <- length(unique(.comm$membership))
	num.members <- sapply(1:(N),
			function(x) { return(length(which(.comm$membership==x))) })
	
	elems <- which(num.members > .min)
	
	return(elems)
}

# Select communities with less or equal than .max members
select.communities.less.equal <- function(.comm, .max) {
	N <- length(unique(.comm$membership))
	num.members <- sapply(1:(N),
			function(x) { return(length(which(.comm$membership==x))) })
	
	elems <- which(num.members <= .max)

	return(elems)
}


# Summarise in which subsystems the authors of a given community are active
comm.subsys <- function(.comm, .id.subsys, N) {
	idx <- which(.comm$membership==N)
	suppressMessages(molten <- melt(.id.subsys[idx,2:dim(.id.subsys)[2]]))
	colnames(molten) <- c("Subsystem", "Fraction")
	
	return(data.frame(Community=N, molten))
}



# This function allows to do a sanity check for the plot
#N <- 3
#summary(id.subsys.connected[which(g.spin.community$membership==N), 2:dim(id.subsys.connected)[2]])
plot.comm.subsys <- function(.comm, .id.subsys, filename, .alg,
		elems=1:(length(unique(.comm$membership))), .height=8, .width=14) {
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



# TODO: Dunno if that tells us something. Most likely not.
#densityplot(~pranks|group, data=construct.pr.info(g.spin.community, pr.for.all),
#            plot.points="rug")

# NOTE: When raw IDs are used as labels, they are zero-based, not
# 1-based as in the ID mapping table
# NOTE: This works for both, communities derived from walktrap and
# spinglass.
plot.group <- function(N, .tags, .iddb, .comm) {
	s <- which(.comm$membership==N)
	g <- graph.adjacency(.tags[s,s], mode="directed")
	V(g)$name <- IDs.to.names(.iddb, V(g)$name)
	plot(g, vertex.label=IDs.to.names(.iddb, s))
}

save.group.NonTag <- function(.tags, .iddb, idx, .prank, .filename=NULL, label=NA) {
	g <- graph.adjacency(.tags[idx,idx], mode="directed")
	# as.character is important. The igraph C export routines bark
	# otherwise (not sure what the actual issue is)
	# NOTE: V(g)$name as label index does NOT work because the name attribute
	# is _not_ stable.
	V(g)$label <- as.character(IDs.to.names(.iddb, idx))
	V(g)$prank <- .prank$vector[idx]
	
	# We also use the page rank to specify the font size of the vertex
	V(g)$fontsize <- scale.data(.prank$vector, 15, 50)[idx]
	
	# The amount of changed lines is visualised by the nodes background colour:
	# The darker, the more changes.
	#fc <- as.character(as.integer(100-scale.data(log(.iddb$total+1),0,50)[idx]))
	V(g)$fillcolor <- paste("grey", 50, sep="")
	V(g)$style="filled"
	
	# And one more bit: The width of the bounding box changes from thin to thick
	# with the number of commits
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

save.group <- function(.tags, .iddb, idx, .prank, .filename=NULL, label=NA) {
	g <- graph.adjacency(.tags[idx,idx], mode="directed")
	# as.character is important. The igraph C export routines bark
	# otherwise (not sure what the actual issue is)
	# NOTE: V(g)$name as label index does NOT work because the name attribute
	# is _not_ stable.
	V(g)$label <- as.character(IDs.to.names(.iddb, idx))
	V(g)$prank <- .prank$vector[idx]
	
	# We also use the page rank to specify the font size of the vertex
	V(g)$fontsize <- scale.data(.prank$vector, 15, 50)[idx]
	
	# The amount of changed lines is visualised by the nodes background colour:
	# The darker, the more changes.
	fc <- as.character(as.integer(100-scale.data(log(.iddb$total+1),0,50)[idx]))
	V(g)$fillcolor <- paste("grey", fc, sep="")
	V(g)$style="filled"
	
	# And one more bit: The width of the bounding box changes from thin to thick
	# with the number of commits
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

save.groups.NonTag <- function(.tags, .iddb, .comm, .prank, .basedir, .prefix, .which, label=NA) {
	baselabel <- label
	for (i in .which) {
		filename <- paste(.basedir, "/", .prefix, "group_", three.digit(i), ".dot", sep="")
		status(paste("Saving", filename))
		idx <- as.vector(which(.comm$membership==i))
		if (!is.na(baselabel)) {
			label <- paste(baselabel, i, sep=" ")
		}
		save.group.NonTag(.tags, .iddb, idx, .prank, filename, label)
	}
}


save.groups <- function(.tags, .iddb, .comm, .prank, .basedir, .prefix, .which, label=NA) {
	baselabel <- label
	for (i in .which) {
		filename <- paste(.basedir, "/", .prefix, "group_", three.digit(i), ".dot", sep="")
		status(paste("Saving", filename))
		idx <- as.vector(which(.comm$membership==i))
		if (!is.na(baselabel)) {
			label <- paste(baselabel, i, sep=" ")
		}
		save.group(.tags, .iddb, idx, .prank, filename, label)
	}
}


# Determine which persons are in a community detection algorithm
# determined sub-community N
persons.in.group <- function(N, .comm, .iddb) {
	idlist <- which(.comm$membership==N)
	return(data.frame(ID=idlist, name=IDs.to.names(.iddb, idlist)))
}

#print("Persons in community a as determined by the spinglass algorithm")
#print(persons.in.group(7, g.spin.community, ids.connected))

# Determine the page rank factors of persons in community N
pr.in.group <- function(N, .comm, .pr) {
	return(.pr[.pr$ID %in% which(.comm$membership==N),][c("name", "ID", "rank")])
}

# Check how the page ranks are distributed within the groups
construct.pr.info <- function(.comm, .pr, N=length(unique(.comm$membership))) {
	for (i in 0:(N-1)) {
		grp.pranks <- log(pr.in.group(i, .comm, .pr)$rank)
		
		if (i == 0) {
			res <- data.frame(group=i, max = max(grp.pranks), min=min(grp.pranks),
					avg=mean(grp.pranks), pranks=grp.pranks)
		} else {
			res <- rbind(res, data.frame(group=i, max = max(grp.pranks),
							min=min(grp.pranks), avg=mean(grp.pranks),
							pranks=grp.pranks))
		}
	}
	
	res$group=as.factor(res$group)
	return (res)
}

save.cluster.stats <- function(.comm, .id.subsys, .elems, .outdir, .basename) {
	for (i in .elems) {
		print(xtable(txt.comm.subsys(.comm, .id.subsys, i)), type="latex",
				floating=FALSE, file=paste(.outdir, "/", .basename, three.digit(i), ".tex", sep=""))
	}
}

save.all <- function(.tags, .iddb, .prank, .comm, .filename=NULL, label=NA) {
	g.all <- save.group(.tags, .iddb, .iddb$ID, .prank, .filename=NULL)
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


# TODO: Investigate the results of inner.links and outer.links (maybe compute
# averages and min/max for all elements of a given community to see how
# well "closed" the communities are)
# TODO: Together with a subsystem distribution of the authors, this should be
# a good basis for a nice ggplot graph.
compute.community.links <- function(g, .comm, N) {
	# TODO: Continue writing here. Compute averages for inner.link and outer.link
	# over all vertices of community N
	idx <- which(.comm$membership==N)
	function(i) {
		subspin <- spinglass.community(g.connected, vertex=V(g)[idx[i]])
		return(c(subspin$inner.links, subspin$outer.links))
	}
}



#========================================================================
#     						Page Rank 
#========================================================================
compute.pagerank <- function(.tags, .damping=0.85, transpose=FALSE) {
	if (transpose) {
		g <- graph.adjacency(t(.tags), mode="directed")
	} else {
		g <- graph.adjacency(.tags, mode="directed")
	}
	ranks <- page.rank(g, directed=TRUE, damping=.damping)
	
	return(ranks)
}




# Determine the N most important developers (as per the
# PageRank measure). This returns a list ordered by pagerank.
# (Note that the raw pagerank data aa given by compute.pagerank()
# give the ranks ordered by ID)
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
	#print("Top 20 page, rank (focus on giving tags)")
	write.table(devs.by.pr[1:20,], file=paste(outdir, "/top20.pr.txt", sep=""), sep="\t",
			quote=FALSE)
	print("here 1")
	print(xtable(devs.by.pr[1:20,]), type="latex", floating=FALSE,
			file=paste(outdir, "/top20.pr.tex", sep=""), sanitize.colnames.function=rotate.label.30)
	print("here 2")
	#print("Top 20 page rank (focus on being tagged)")
	write.table(devs.by.pr.tr[1:20,], file=paste(outdir, "/top20.pr.tr.txt", sep=""), sep="\t",
			quote=FALSE)
	print(xtable(devs.by.pr.tr[1:20,]), type="latex", floating=FALSE,
			file=paste(outdir, "/top20.pr.tr.tex", sep=""), sanitize.colnames.function=rotate.label.30)
	
}

#########################################################################
#     					 Main Functions
#########################################################################

performTagAnalysis <- function(outdir){
	################## Process the data #################
	status("Reading files")
	tags <- read.table(file=paste(outdir, "/tags.txt", sep=""),
			sep="\t", header=FALSE)
	colnames(tags) <- rownames(tags)
	
# The tags file format uses a different convention for edge direction
# than GNU R, so we need to transpose the matrix
	tags <- t(tags)
	
	ids <- read.csv(file=paste(outdir, "/ids.txt", sep=""),
			sep="\t", header=TRUE)
	print(length(ids))
# IDs are zero-based, but everything in R is 1-based, so simplify
# things by adapting the IDs...
	ids$ID <- ids$ID + 1
	
	id.subsys <- read.csv(file=paste(outdir, "/id_subsys.txt", sep=""),
			sep="\t", header=TRUE)
	id.subsys$ID <- id.subsys$ID + 1
	
# Isolated graph members are outliers for the Linux kernel. Eliminate
# them to create a connected graph (NOTE: This must not be done for
# projects where proper direct clustering can happen)
	status("Computing adjacency matrices")
	g <- graph.adjacency(tags, mode="directed")
	g.clust <- clusters(g)
	idx <- which(g.clust$membership==1) # All connected developers are in group 0
	tags.connected <- tags[idx,idx]
#ids.connected <- data.frame(Name=ids[idx,]$Name, ID=seq(1:length(idx)))
	ids.connected <- ids[idx,]
	ids.connected$ID=seq(1:length(idx))
	ids.connected$Name <- as.character(ids.connected$Name)
	print(length(ids.connected))
	id.subsys.connected <- id.subsys[idx,]
	id.subsys.connected$ID=seq(1:length(idx))
	
	g.connected <- graph.adjacency(tags.connected, mode="directed")
	V(g.connected)$label <- as.character(ids.connected$Name)
	
# Compute the traditional list of developer influence by counting how
# large their commit contributions were, which is fairly easy to compute
# NOTE: The amount of code changes differs from the lwn.net analysis -- we
# generally attribute more core to the developers. A few values were
# cross-checked,  but I could not find anythin bogous in our calculations.
	status("Computing classical statistics")
	rank.by.total <- get.rank.by.field(ids.connected, "total", 20)
	rank.by.numcommits  <- get.rank.by.field(ids.connected, "numcommits", 20)
	
	write.table(rank.by.total, file=paste(outdir, "/top20.total.txt", sep=""), sep="\t",
			quote=FALSE)
	print(xtable(rank.by.total), type="latex", floating=FALSE,
			file=paste(outdir, "/top20.total.tex", sep=""), sanitize.colnames.function=rotate.label)
	
	write.table(rank.by.numcommits, file=paste(outdir, "/top20.numcommits.txt", sep=""), sep="\t",
			quote=FALSE)
	print(xtable(rank.by.numcommits), type="latex", floating=FALSE,
			file=paste(outdir, "/top20.numcommits.tex", sep=""), sanitize.colnames.function=rotate.label)
	
	
# Some id conversion magic tests
#ids[which(substr(ids$Name, 0, 14) == "Linus Torvalds"),]$ID
	
# Compute the page ranking for all developers in the database
	status("Computing page rank")
# This puts the focus on tagging other persons
	pr.for.all <- compute.pagerank(tags.connected, transpose=TRUE)
# ... and this on being tagged. 
	pr.for.all.tr <- compute.pagerank(tags.connected, .damping=0.3)
	
# NOTE: pr.for.all$value should be one, but is 0.83 for some
# reason. This seems to be a documentation bug, though:
# https://bugs.launchpad.net/igraph/+bug/526106
	devs.by.pr <- influential.developers(NA, pr.for.all, tags.connected,
			ids.connected)
	
	devs.by.pr.tr <- influential.developers(NA, pr.for.all.tr, tags.connected,
			ids.connected)
	
#print("Top 20 page, rank (focus on giving tags)")
	write.table(devs.by.pr[1:20,], file=paste(outdir, "/top20.pr.txt", sep=""), sep="\t",
			quote=FALSE)
	print(xtable(devs.by.pr[1:20,]), type="latex", floating=FALSE,
			file=paste(outdir, "/top20.pr.tex", sep=""), sanitize.colnames.function=rotate.label.30)
	
#print("Top 20 page rank (focus on being tagged)")
	write.table(devs.by.pr.tr[1:20,], file=paste(outdir, "/top20.pr.tr.txt", sep=""), sep="\t",
			quote=FALSE)
	print(xtable(devs.by.pr.tr[1:20,]), type="latex", floating=FALSE,
			file=paste(outdir, "/top20.pr.tr.tex", sep=""), sanitize.colnames.function=rotate.label.30)
	
	
# Consistency check (names and tags should be identical, IDs can differ)
#influential.developers(20, tags.connected, ids.connected) == influential.developers(20, tags, ids)
	
# NOTE: This one is very cpu time intensive. Did not finish after several
# hours. TODO: Is there any progress indicator, or until up to what size
# does it work?
#gBlocks <- cohesive.blocks(g)
	
# NOTE: When an undirected matrix is required, then
# likely, the best thing is to compute the adjacency matrix with "add"
# Some of the algorithms only work with undirected graphs.
# See http://igraph.wikidot.com/community-detection-in-r
	
	######### Communities derived with the spinglass algorithm #################
# NOTE: The group labels may change between different invocations
# of the community detection algorithms. Setting a fixed random seed
# before the detection prevents this.
# TODO: Also investigate how stable the communities are across
# different kernel releases.
	status("Inferring communities with spin glasses")
	set.seed(42)
	g.spin.community <- spinglass.community(g.connected)
	
	status("Inferring communities with random walks")
	set.seed(42)
	g.walktrap.community <- walktrap.community(g.connected)
	
	
	
	
# TODO: Also make a pass for communities with _less_ than 10 members to see possibly
	status("Writing community graph sources for spin glasses")
	elems.sg.more <- select.communities.more(g.spin.community, 10)
	save.groups(tags.connected, ids.connected, g.spin.community, pr.for.all, outdir,
			"sg_reg_", elems.sg.more, label="Spin Glass Community")
	save.groups(tags.connected, ids.connected, g.spin.community, pr.for.all.tr, outdir,
			"sg_tr_", elems.sg.more, label="Spin Glass Community")
	
	status("Writing community graph sources for random walks")
	elems.wt.more <- select.communities.more(g.walktrap.community, 10)
	elems.wt.less <- select.communities.less.equal(g.walktrap.community, 10)
	save.groups(tags.connected, ids.connected, g.walktrap.community, pr.for.all, outdir,
			"wt_reg_big_", elems.wt.more, label="(big) Random Walk Community")
	save.groups(tags.connected, ids.connected, g.walktrap.community, pr.for.all.tr, outdir,
			"wt_tr_big_", elems.wt.more, label="(big) Random Walk Community")
	save.groups(tags.connected, ids.connected, g.walktrap.community, pr.for.all, outdir,
			"wt_reg_small_", elems.wt.less, label="(small) Random Walk Community")
	save.groups(tags.connected, ids.connected, g.walktrap.community, pr.for.all.tr, outdir,
			"wt_tr_small_", elems.wt.less, label="(small) Random Walk Community")
	
	status("Writing the all-developers graph sources")
# NOTE: The all-in-one graphs get a different suffix (ldot for "large dot") so that we can easily
# skip them when batch-processing graphviz images -- they take a long while to compute
	#g.all <- save.all(tags.connected, ids.connected, pr.for.all, g.spin.community,
			#save.all(tags.connected, ids.connected, pr.for.all, g.spin.community,
			#paste(outdir, "/sg_reg_all.ldot", sep=""),
			#label="Spin glass, regular page rank")
	g.all <- save.all(tags.connected, ids.connected, pr.for.all.tr, g.spin.community,
			paste(outdir, "/sg_tr_all.ldot", sep=""),
			label="Spin glass, transposed page rank")
	g.all <- save.all(tags.connected, ids.connected, pr.for.all, g.walktrap.community,
			paste(outdir, "/wt_reg_all.ldot", sep=""),
			label="Random walk, regular page rank")
	g.all <- save.all(tags.connected, ids.connected, pr.for.all.tr, g.walktrap.community,
			paste(outdir, "/wt_tr_all.ldot", sep=""),
			label="Random walk, transposed page rank")
	
	status("Plotting per-cluster subsystem distribution")
	plot.comm.subsys(g.spin.community, id.subsys.connected, paste(outdir, "/sg_comm_subsys.pdf", sep=""),
			"spin glass")
# Since walktrap produces smaller, but more communities, we devide the plot into two parts
	plot.comm.subsys(g.walktrap.community, id.subsys.connected,
			paste(outdir, "/wt_comm_subsys_big.pdf", sep=""), "random walk", elems=elems.wt.more)
	plot.comm.subsys(g.walktrap.community, id.subsys.connected,
			paste(outdir, "/wt_comm_subsys_small.pdf", sep=""), "random walk",
			elems=elems.wt.less)
	
	status("Saving raw per-cluster statistical summaries")
	save.cluster.stats(g.spin.community, id.subsys.connected, elems.sg.more, outdir, "sg_cluster_")
	save.cluster.stats(g.walktrap.community, id.subsys.connected, elems.wt.more, outdir, "wt_cluster_")
	save.cluster.stats(g.walktrap.community, id.subsys.connected, elems.wt.less, outdir, "wt_cluster_")
	
	print("finished person.r")
	
}



performGraphAnalysis <- function(adjMatrix, ids, outDir){
	
	#====================================
	#     Find Connected Subgraphs
	#====================================
	
	# Isolated graph members are outliers for the Linux kernel. Eliminate
	# them to create a connected graph (NOTE: This must not be done for
	# projects where proper direct clustering can happen)
	status("Computing adjacency matrices")
	
	#round matrix, for some reason when calling graph.adjacency on 
	#the adjMatrix with weighted=TRUE the call fails
	adjMatrix <- ceiling(adjMatrix)
	
	g <- graph.adjacency(adjMatrix, mode="directed", weighted=TRUE)
	g.clust <- clusters(g)
	
	#TODO: check on this membership == 1 thing I think its trying to get 
	# the largest connected component 
	idx <- which(g.clust$membership==2) # All connected developers are in group 0
	adjMatrix.connected <- adjMatrix[idx,idx]
	
	
	#build adjacency matrix of connected developers
	ids.connected <- ids[idx,]
	ids.connected$ID=seq(1:length(idx))
	ids.connected$Name <- as.character(ids.connected$Name)
	
	g.connected <- graph.adjacency(adjMatrix.connected, mode="directed", weighted=TRUE)
	V(g.connected)$label <- as.character(ids.connected$Name)
	
	
	
	#========================
	#  Page rank analysis 
	#========================
	
	# Compute the page ranking for all developers in the database
	status("Computing page rank")
	# This puts the focus on tagging other persons
	pr.for.all <- compute.pagerank(adjMatrix.connected, transpose=TRUE)
	# ... and this on being tagged. 
	pr.for.all.tr <- compute.pagerank(adjMatrix.connected, .damping=0.3)
	
	# NOTE: pr.for.all$value should be one, but is 0.83 for some
	# reason. This seems to be a documentation bug, though:
	# https://bugs.launchpad.net/igraph/+bug/526106
	devs.by.pr <- influential.developers(NA, pr.for.all, adjMatrix.connected,
		ids.connected)

	devs.by.pr.tr <- influential.developers(NA, pr.for.all.tr, adjMatrix.connected,
		ids.connected)
	#-----------
	#save data 
	#-----------
	writePageRankData(outDir, devs.by.pr, devs.by.pr.tr)
	
	
	
	#=======================
	# Find Communities 
	#=======================

	#--------------------
	#spin-glass 
	#--------------------
	status("Inferring communities with spin glasses")
	set.seed(42)
	g.spin.community <- spinglass.community(g.connected, weights=NA)
	
	status("Writing community graph sources for spin glasses")
	elems.sg.more <- select.communities.more(g.spin.community, 10)
	save.groups.NonTag(adjMatrix.connected, ids.connected, g.spin.community, pr.for.all, outdir,
			"sg_reg_", elems.sg.more, label="Spin Glass Community")
	save.groups.NonTag(adjMatrix.connected, ids.connected, g.spin.community, pr.for.all.tr, outdir,
			"sg_tr_", elems.sg.more, label="Spin Glass Community")
	
	#--------------------
	#random walk
	#--------------------
	status("Inferring communities with random walks")
	set.seed(42)
	g.walktrap.community <- walktrap.community(g.connected)
	
	status("Writing community graph sources for random walks")
	elems.wt.more <- select.communities.more(g.walktrap.community, 10)
	elems.wt.less <- select.communities.less.equal(g.walktrap.community, 10)
	save.groups.NonTag(adjMatrix.connected, ids.connected, g.walktrap.community, pr.for.all, outdir,
			"wt_reg_big_", elems.wt.more, label="(big) Random Walk Community")
	save.groups.NonTag(adjMatrix.connected, ids.connected, g.walktrap.community, pr.for.all.tr, outdir,
			"wt_tr_big_", elems.wt.more, label="(big) Random Walk Community")
	save.groups.NonTag(adjMatrix.connected, ids.connected, g.walktrap.community, pr.for.all, outdir,
			"wt_reg_small_", elems.wt.less, label="(small) Random Walk Community")
	save.groups.NonTag(adjMatrix.connected, ids.connected, g.walktrap.community, pr.for.all.tr, outdir,
			"wt_tr_small_", elems.wt.less, label="(small) Random Walk Community")
	
	
	
	#------------------
	# Write other data 
	#-----------------
	status("Writing the all-developers graph sources")
	# NOTE: The all-in-one graphs get a different suffix (ldot for "large dot") so that we can easily
	# skip them when batch-processing graphviz images -- they take a long while to compute
	#g.all <- save.all(adjMatrix.connected, ids.connected, pr.for.all, g.spin.community,
	#		paste(outdir, "/sg_reg_all.ldot", sep=""),
	#		label="Spin glass, regular page rank")
	g.all <- save.all(adjMatrix.connected, ids.connected, pr.for.all.tr, g.spin.community,
			paste(outdir, "/sg_tr_all.ldot", sep=""),
			label="Spin glass, transposed page rank")
	g.all <- save.all(adjMatrix.connected, ids.connected, pr.for.all, g.walktrap.community,
			paste(outdir, "/wt_reg_all.ldot", sep=""),
			label="Random walk, regular page rank")
	g.all <- save.all(adjMatrix.connected, ids.connected, pr.for.all.tr, g.walktrap.community,
			paste(outdir, "/wt_tr_all.ldot", sep=""),
			label="Random walk, transposed page rank")
	
	status("Plotting per-cluster subsystem distribution")
	plot.comm.subsys(g.spin.community, id.subsys.connected, paste(outdir, "/sg_comm_subsys.pdf", sep=""),
			"spin glass")
	# Since walktrap produces smaller, but more communities, we devide the plot into two parts
	plot.comm.subsys(g.walktrap.community, id.subsys.connected,
			paste(outdir, "/wt_comm_subsys_big.pdf", sep=""), "random walk", elems=elems.wt.more)
	plot.comm.subsys(g.walktrap.community, id.subsys.connected,
			paste(outdir, "/wt_comm_subsys_small.pdf", sep=""), "random walk",
			elems=elems.wt.less)
	
	status("Saving raw per-cluster statistical summaries")
	save.cluster.stats(g.spin.community, id.subsys.connected, elems.sg.more, outdir, "sg_cluster_")
	save.cluster.stats(g.walktrap.community, id.subsys.connected, elems.wt.more, outdir, "wt_cluster_")
	save.cluster.stats(g.walktrap.community, id.subsys.connected, elems.wt.less, outdir, "wt_cluster_")
	
	print("")
	
}


performNonTagAnalysis <- function(outDir){
	
	#-----------------
	# Read Data
	#-----------------
	status("Reading files")
	adjMatrix <- read.table(file=paste(outDir, "/adjacencyMatrix.txt", sep=""),
			sep="\t", header=FALSE)
	
	colnames(adjMatrix) <- rownames(adjMatrix)
	
	# The adjacency file format uses a different convention for edge direction
	# than GNU R, so we need to transpose the matrix
	adjMatrix <- t(adjMatrix)
	
	ids <- read.csv(file=paste(outDir, "/ids.txt", sep=""),
			sep="\t", header=TRUE)
	
	# IDs are zero-based, but everything in R is 1-based, so simplify
	# things by adapting the IDs...
	ids$ID <- ids$ID + 1
	
	
	#--------------------------
	# Graph Analysis
	#--------------------------
	
	performGraphAnalysis(adjMatrix, ids, outdir)
	
	
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
#     					 Executed Statements  
#########################################################################

#----------------------------
#parse commandline arguments
#----------------------------
parser <- OptionParser(usage = "%prog datadir")
arguments <- parse_args(parser, positional_arguments = TRUE)

if(length(arguments$args) != 2) {
	
	cat("Please specify data directory\n\n")
	print_help(parser)
	stop()
	
} else {
	
	dataDir <- arguments$args[1]
	type    <- arguments$args[2]
	
}

#------------------------------
# Perform Appropriate Analysis 
#------------------------------
if (type == "tag") {
	
	print("Performing Tag Based Graph Analysis")
	performTagAnalysis(dataDir)
	
} else if (type == "nonTag") {
	
	print("Performing nonTag Based Graph Analysis")
	performNonTagAnalysis(dataDir)
	
} else{
	
	print("incorrect command line arguments for persons.r")
	
}
