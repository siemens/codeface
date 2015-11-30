library(ggplot2)
library(igraph)
library("BiRewire")

source("query.r")
source("config.r")
source("dependency_analysis.r")
source("process_dsm.r")
source("process_jira.r")

plot.to.file <- function(g, outfile) {
  g <- simplify(g,edge.attr.comb="first")
  g <- delete.vertices(g, names(which(degree(g)<2)))
  E(g)[is.na(E(g)$color)]$color <- "gray80"
  png(file=outfile, width=10, height=10, units="in", res=300)
  plot(g, layout=layout.kamada.kawai, vertex.size=2,
       vertex.label.dist=0.5, edge.arrow.size=0.5,
       vertex.label=NA)
  dev.off()
}

motif.generator <- function(type) {
  motif <- graph.empty(directed=FALSE)
  if (type=="square") {
    motif <- add.vertices(motif, 4)
    motif <- add.edges(motif, c(1,2, 1,3, 2,4, 3,4))
    V(motif)$kind <- c(person.role, person.role, artifact.type, artifact.type)
    V(motif)$color <- vertex.coding[V(motif)$kind]
  }
  else if (type=="triangle") {
  motif <- add.vertices(motif, 3)
  motif <- add.edges(motif, c(1,2, 1,3, 2,3))
  V(motif)$kind <- c(person.role, person.role, artifact.type)
  V(motif)$color <- vertex.coding[V(motif)$kind]
  }
  else {
    motif <- NULL
  }

  return(motif)
}

preprocess.graph <- function(g) {
  ## Remove loops and multiple edges
  g <- simplify(g, remove.multiple=TRUE, remove.loops=TRUE,
                edge.attr.comb="first")

  ## Remove low degree artifacts
  artifact.degree <- degree(g, V(g)[V(g)$kind==artifact.type])
  low.degree.artifact <- artifact.degree[artifact.degree < 2]
  g <- delete.vertices(g, v=names(low.degree.artifact))

  ## Remove isolated developers
  dev.degree <- degree(g, V(g)[V(g)$kind==person.role])
  isolated.dev <- dev.degree[dev.degree==0]
  g <- delete.vertices(g, v=names(isolated.dev))

  return(g)
}

## Configuration
if (!exists("conf")) conf <- connect.db("../../codeface.conf")
dsm.filename <- "/home/mitchell/Downloads/cassandra-2.1.0.dsm.xlsx"
jira.filename <- "/home/mitchell/Downloads/jira-comment-authors.csv"
codeface.filename <- "/home/mitchell/Downloads/jiraId_CodefaceId.csv"
con <- conf$con
project.id <- 2
artifact.type <- list("function", "file")[[2]]
dependency.type <- list("co-change", "dsm")[[2]]
communication.type <- list("mail", "jira")[[2]]
person.role <- "developer"
start.date <- "2015-07-01"
end.date <- "2015-10-01"
file.limit <- 30
historical.limit <- ddays(365)

## Compute dev-artifact relations
vcs.dat <- query.dependency(con, project.id, artifact.type, file.limit,
                            start.date, end.date, impl=FALSE, rmv.dups=FALSE)
vcs.dat$author <- as.character(vcs.dat$author)

## Compute communication relations
if (communication.type=="mail") {
  comm.dat <- query.mail.edgelist(con, project.id, start.date, end.date)
  colnames(comm.dat) <- c("V1", "V2", "weight")
} else if (communication.type=="jira") {
  comm.dat <- load.jira.edgelist(jira.filename, codeface.filename)
}
comm.dat[, c(1,2)] <- sapply(comm.dat[, c(1,2)], as.character)

## Compute entity-entity relations
relavent.entity.list <- unique(vcs.dat$entity)
if (dependency.type == "co-change") {
  start.date.hist <- as.Date(start.date) - historical.limit
  end.date.hist <- start.date

  commit.df.hist <- query.dependency(con, project.id, artifact.type, file.limit,
                                     start.date.hist, end.date.hist)

  commit.df.hist <- commit.df.hist[commit.df.hist$entity %in% relavent.entity.list, ]

  ## Compute co-change relationship
  freq.item.sets <- compute.frequent.items(commit.df.hist)
  ## Compute an edgelist
  dependency.dat <- compute.item.sets.edgelist(freq.item.sets)
  names(dependency.dat) <- c("V1", "V2")

} else if (dependency.type == "dsm") {
  dependency.dat <- load.dsm.edgelist(dsm.filename)
  dependency.dat <-
    dependency.dat[dependency.dat[, 1] %in% relavent.entity.list &
                   dependency.dat[, 2] %in% relavent.entity.list, ]
}

## Compute node sets
node.function <- unique(vcs.dat$entity)
node.dev <- unique(c(mail.dat$to, mail.dat$from, vcs.dat$author))

## Generate bipartite network
g.nodes <- graph.empty(directed=FALSE)
g.nodes <- add.vertices(g.nodes, nv=length(node.dev),
                        attr=list(name=node.dev, kind=person.role,
                                  type=TRUE))
g.nodes  <- add.vertices(g.nodes, nv=length(node.function),
                         attr=list(name=node.function, kind=artifact.type,
                                   type=FALSE))

## Add developer-entity edges
vcs.edgelist <- with(vcs.dat, ggplot2:::interleave(author, entity))
g.bipartite <- add.edges(g.nodes, vcs.edgelist)

## Add developer-developer communication edges
g <- graph.empty(directed=FALSE)
comm.edgelist <- as.character(with(comm.dat, ggplot2:::interleave(V1, V2)))
g <- add.edges(g.bipartite, mail.edgelist, attr=list(color="red"))

## Add entity-entity edges
dependency.edgelist <- as.character(with(dependency.dat,
                                         ggplot2:::interleave(V1, V2)))
g <- add.edges(g, dependency.edgelist)

## Apply filters
g <- preprocess.graph(g)

## Apply vertex coding
vertex.coding <- c()
vertex.coding[person.role] <- 1
vertex.coding[artifact.type] <- 2
V(g)$color <- vertex.coding[V(g)$kind]

## Define motif
motif <- motif.generator("square")

## Count subgraph isomorphisms
motif.count <- count_subgraph_isomorphisms(motif, g, method="vf2")

## Compute null model
niter <- 1000
motif.count.null <- c()

motif.count.null <-
  sapply(seq(niter),
    function(i) {
      ## Rewire dev-artifact bipartite
      g.bipartite.rewired <- g.bipartite #birewire.rewire.bipartite(simplify(g.bipartite), verbose=FALSE)

      ## Add rewired edges
      g.null <- add.edges(g.nodes,
                          as.character(with(get.data.frame(g.bipartite.rewired),
                                            ggplot2:::interleave(from, to))))
      ## Aritfact-artifact edges
      g.null <- add.edges(g.null, dependency.edgelist)

      ## Test degree dist
      #if(!all(sort(as.vector(degree(g.null))) ==
      #        sort(as.vector(degree(g.bipartite))))) stop("Degree distribution not conserved")

      ## Rewire dev-dev communication graph
      g.mail <- graph.data.frame(mail.dat)
      g.mail.null <- birewire.rewire.undirected(simplify(g.mail),
                                                verbose=FALSE)
      ## Test degree dist
      if(!all(sort(as.vector(degree(g.mail.null))) ==
              sort(as.vector(degree(g.mail))))) stop("Degree distribution not conserved")

      g.null <- add.edges(g.null,
                          as.character(with(get.data.frame(g.mail.null),
                                       ggplot2:::interleave(from, to))),
                          attr=list(color="red"))

      ## Code and count motif
      V(g.null)$color <- vertex.coding[V(g.null)$kind]

      g.null <- preprocess.graph(g.null)

      res <- count_subgraph_isomorphisms(motif, g.null, method="vf2")

      return(res)})

motif.count.dat <- data.frame(motif.count.null=motif.count.null,
                              motif.count.empirical=motif.count)


p.null <- ggplot(data=motif.count.dat, aes(x=motif.count.null)) +
       geom_histogram(aes(y=..density..), colour="black", fill="white") +
       geom_point(aes(x=motif.count.empirical), y=0, color="red", size=5) +
       geom_density(alpha=.2, fill="#AAD4FF")
ggsave(file="motif_count.png", p.null)

p.null <- ggplot(data=data.frame(degree=degree(graph.data.frame(mail.dat))), aes(x=degree)) +
    geom_histogram(aes(y=..density..), colour="black", fill="white") +
    geom_density(alpha=.2, fill="#AAD4FF")
ggsave(file="email_degree_dist.png")