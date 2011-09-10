#! /usr/bin/env Rscript
# Analyse the developer connections
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
# 3.) for i in `seq 0 16`; do
#   cat group_$i.dot | ./conv.py > g$i.dot;
#   neato -Tpdf g$i.dot> g$i.pdf;
# done
library(igraph)
library(stringr)
library(lattice)
library(optparse)

parser <- OptionParser(usage = "%prog datadir")
arguments <- parse_args(parser, positional_arguments = TRUE)

if(length(arguments$args) != 1) {
    cat("Please specify data directory\n\n")
    print_help(parser)
    stop()
} else {
    datadir <- arguments$args
}

#datadir <- "/Users/wolfgang/papers/csd/cluster/res/33/"
outdir <- datadir


# Given an eMail address like "Name N. Surname <name.surname@domain.com>",
# extract the person name without the electronic address
name.of.email <- function(str) {
  return(str_sub(str, 1, str_locate(str, "<")[1]-2))
}

# NOTE: We rely that the ids are already sorted numerically. To make
# this sure, a check could do no harm
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

tags.received <- function(.id, .tags) {
  return(sum(.tags[.id,]))
}

tags.given <- function(.id, .tags) {
  return(sum(.tags[,.id]))
}

# Determine the N most important developers (as per the
# PageRank measure)
# NOTE: This works very well and should be applied to multiple
# kernel releases, and systematically compared with old results.
# NOTE: This takes a few minutes to be computed for all developers
influential.developers <- function(N, .tags, .iddb) {
  g <- graph.adjacency(.tags, mode="directed")
  ranks <- page.rank(g)
  if (is.na(N)) {
    N <- length(ranks$vector)
  }
  
#  idlst = which(ranks$vector >= sort(ranks$vector, decreasing=T)[N])
  idlst = seq(1,length(ranks$vector))[order(ranks$vector, decreasing=TRUE)[1:N]]

  res = data.frame(name=IDs.to.names(.iddb, idlst), ID=idlst,
    TagsGiven=sapply(idlst, function(.id) { tags.given(.id, .tags)}),
    TagsReceived=sapply(idlst, function(.id) { tags.received(.id, .tags)}),
    rank = ranks$vector[order(ranks$vector, decreasing=TRUE)[1:N]])

  return(res)
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

#print("Page rank of persons in a group")
#print(pr.in.group(19, g.spin.community, pr.for.all))

# Check how the page ranks are distributed within the groups
construct.pr.info <- function(.comm, N=length(unique(.comm$membership))) {
  for (i in 0:(N-1)) {
    grp.pranks <- log(pr.in.group(i, .comm, pr.for.all)$rank)
    
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

# TODO: Dunno if that tells us something. Most likely not.
#densityplot(~pranks|group, data=construct.pr.info(g.spin.community),
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

save.group <- function(N, .tags, .iddb, .comm, .filename=NULL) {
  s <- which(.comm$membership==N)
  g <- graph.adjacency(.tags[s,s], mode="directed")
  # as.character is important. The igraph C export routines bark
  # otherwise (not sure what the actual issue is)
  # NOTE: V(g)$name as label index does NOT work because the name attribute
  # is _not_ stable.
  V(g)$label <- as.character(IDs.to.names(.iddb, s)) 
  # TODO: Determine fontsize and label shape by page rank and/or number
  # of in- and outlinks
  
  if (!is.null(.filename)) {
    write.graph(g, .filename, format="dot")
  }
  
  return(g)
}

save.groups <- function(.tags, .iddb, .comm, .basedir) {
  N <- length(unique(.comm$membership))
  for (i in 0:(N-1)) {
    filename <- paste(.basedir, "/group_", i, ".dot", sep="")
    print(filename)
    save.group(i, .tags, .iddb, .comm, filename)
  }
}

################## Process the data #################
tags <- read.table(file=paste(outdir, "/tags.txt", sep=""),
                   sep="\t", header=FALSE)
colnames(tags) <- rownames(tags)

ids <- read.csv(file=paste(outdir, "/ids.txt", sep=""),
                sep="\t", header=FALSE)
colnames(ids) <- c("ID", "Name", "eMail", "added", "deleted", "total", "numcommits")
#ids$Name <- sapply(ids$Name, name.of.email)
# IDs are zero-based, but everything in R is 1-based, so simplify
# things by adapting the IDs...
ids$ID <- ids$ID + 1

# Compute the traditional list of developer influence by counting how
# large their commit contributions were (easily computed for number
# of files touched, code added, etc.)
added <- ids[c("Name", "total")]
added <- added[order(added$total, decreasing=T),]
added[1:20,]
added <- cbind(added, data.frame(total.norm = added$total/max(added$total)))

numcommits <- ids[c("Name", "numcommits")]
numcommits <- numcommits[order(numcommits$numcommits, decreasing=T),]
numcommits[1:20,]
numcommits <- cbind(numcommits,
                    data.frame(numcommits.norm =
                               numcommits$numcommits/max(numcommits$numcommits)))

# Isolated graph members are outliers for the Linux kernel. Eliminate
# them to create a connected graph (NOTE: This must not be done for
# projects where direct clustering can happen)
g <- graph.adjacency(tags, mode="directed")
g.clust <- clusters(g)
idx <- which(g.clust$membership==0) # All connected developers are in group 0
tags.connected <- tags[idx,idx]
ids.connected <- data.frame(Name=ids[idx,]$Name, ID=seq(1:length(idx)))
ids.connected$Name <- as.character(ids.connected$Name)
g.connected <- graph.adjacency(tags.connected, mode="directed")
V(g.connected)$label <- as.character(ids.connected$Name)

# TODO: ranks$value should be one, but is 0.83 for some
# reason. This seems to be a documentation bug, though:
# https://bugs.launchpad.net/igraph/+bug/526106
#ranks$value 

# Some id conversion magic tests
#ids[which(substr(ids$Name, 0, 14) == "Linus Torvalds"),]$ID

# Compute the page ranking for all developers in the database
pr.for.all <- influential.developers(NA, tags.connected, ids.connected)
pr.for.all$name <- as.character(pr.for.all$name)

print("The 20 most influential developers for the range under consideration")
# (same results are in pr.for.all[1:20,])
print(influential.developers(20, tags.connected, ids.connected))[c("name", "TagsGiven", "TagsReceived")]
ids.connected$Name <- as.character(ids.connected$Name)
print(influential.developers(20, tags, ids))[c("name", "TagsGiven", "TagsReceived")]

# Consistency check (names and tags should be identical, IDs can differ)
influential.developers(20, tags.connected, ids.connected) == influential.developers(20, tags, ids)

# TODO: Compute the subgraph with the most influential developers,
# and plot it.

# NOTE: This one is very cpu time intensive. Did not finish after several
# hours. TODO: Is there any progress indicator, or until up to what size
# does it work?
#gBlocks <- cohesive.blocks(g)

# TODO: Try the various community detection algorithms from igraph
# TODO: Can we convert the directed graph into an undirected graph
# with weights to model the interaction strength between developers?
# Some of the algorithms only work with undirected graphs.
# See http://igraph.wikidot.com/community-detection-in-r
# TODO: Especially try the clique percolation algorithm, since it
# allows for overlapping clusters. Then find out which developers
# are in multiple communities.
#fastgreedy.community(g) # Only for undirected graphs

######### Communities derived with the spinglass algorithm #################
# TODO: Evaluate the results
# TODO: Also investigate how stable the communities are across
# different kernel releases.
# TODO: The direction of the edges is neglected, which is not exactly
# what we want...
set.seed(42)
g.spin.community <- spinglass.community(g.connected)

# NOTE: In general, the groups seem to cluster around one or
# two developers who have received a substantial amount of 
# attention.
# NOTE: The group labels may change between different invocations
# of the community detection algorithms. Set a fixed random seed
# before the detection to prevent this.
# For 2.6.36 (with seed 42), 9 is KVM
#plot.group(9, tags.connected, ids.connected, g.spin.community)
#g.sub <- save.group(19, tags.connected, ids.connected, g.spin.community,
#                    "/tmp/test.dot")

# See conv.py how to post-process graphs and generate pdfs 
save.groups(tags.connected, ids.connected, g.spin.community, outdir)

quit()




#################### Some experiments ##############
######### Communities derived with the walktrap method
g.walktrap.community <- walktrap.community(g.connected)
# Virtualisation?
plot.group(11, tags.connected, ids.connected, g.walktrap.community)
# SCSI?
plot.group(13, tags.connected, ids.connected, g.walktrap.community)


##########
# Somce other generic graph measures. TODO: See if/how we can use them
max(closeness(g))
max(betweenness(g))

# NOTE: Computing the adjacency graphs with weighted instead
# of multiple edges could be done with
# g <- graph.adjaceny(tags, mode="directed", weighted=TRUE)
# but takes quite a long time (and is also not suitable for most
# community detection algorithms)

ranks <- page.rank(g)
# Map the page rank values to [0,100]
ranks.norm <-  ranks
ranks.norm$vector <- scale.data(ranks.norm$vector, 0, 100)
