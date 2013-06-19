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
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Create overviews about the types of collaboration graphs appearing in
## projects. Intended as interactive worksheet.

source("config.r")
source("db.r")
source("utils.r")
source("query.r")
library(igraph)

get.config <- function(config.file) {
  conf <- load.config(config.file)
  global.conf <- load.global.config("prosoda.conf")

  return(init.db(conf, global.conf))
}

annotate.cluster <- function(g) {
  V(g)$size <- sqrt(V(g)$prank*5000)
  E(g)$width <- sqrt(E(g)$weight)

  ## We store the global properties as attributes of the graph
  ## to eliminate the need for a second data structure

  ## Reciprocity computes the amount of reciprocal connections. The
  ## larger the score, the better is bidirectional connection in a graph
  ## (low score ones tend to be central developer communities with numerous
  ## unconnected contributors)
  g$rec <- round(reciprocity(g, ignore.loops=F), digits=3)

  ## TODO: Document the purpose of graph strength
  g$strength <- mean(graph.strength(g, mode="all"))
  g$strength <- round(g$strength/vcount(g), digits=3)

  ## Select the most important developers (as per page rank)
  prank.sorted <- sort(V(g)$prank, index.return=T, decreasing=T)

  ## We compute the degree for the three most important developers
  ## (for large graphs) or of the most important developer for
  ## small graphs.
  ## TODO: The threshold is arbitrary
  if (vcount(g) > 15) {
    vertex.idx <- prank.sorted$ix[1:3]
  } else {
    vertex.idx <- prank.sorted$ix
  }

  g$deg.graph <- round(mean(degree(g, vertex.idx, normalize=T)), digits=3)
  g$size <- vcount(g)

  ## TODO: IN the analysis/the clustering phase, also consider the
  ## page rank distribution and mean page rank.

  return(g)
}

gen.graphs.list <- function(l, con) {
  return(lapply(1:length(l), function(i) {
    g <- construct.cluster(con, l[[i]])
    return(annotate.cluster(g))
  }))
}

do.plot <- function(g) {
  V(g)$name <- NA
  plot(g, main=str_c("Reciprocity: ", g$rec, "\nStrength: ", g$strength,
          " deg:", g$deg.graph, " vertices: ", g$size))
}


do.cluster.plots <- function(con, pid, filename) {
  range.ids.list <- query.range.ids.con(con, pid)

  graphs.lists <- lapply(range.ids.list, function(range.id) {
    l <- query.cluster.ids.con(con, pid, range.id, "Spin Glass Community")
    return(gen.graphs.list(l, con))
  })

  pdf(filename, width=14, height=10)
  max.length <- 8

  dummy <- sapply(graphs.lists, function(graphs.list) {
    if (length(graphs.list) < 8) {
      max.length <- length(graphs.list)
    }

    par(mfcol=c(2,4))
    dummy <- sapply(graphs.list[1:max.length], function(g) { do.plot(g) })
  })
  dummy <- dev.off()
}

projects <- list("qemu")
conf.list <- lapply(projects, function (project) {
  conf <- get.config(str_c("conf/", project, ".conf"))

  return(conf)
})

dummy <- sapply(1:length(projects), function(i) {
  conf <- conf.list[[i]]
  do.cluster.plots(conf$con, conf$pid, str_c("/tmp/", projects[[i]], ".pdf"))
})
