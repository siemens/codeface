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
## Copyright 2015 by Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## All Rights Reserved.
    
suppressMessages(library(igraph))

source("db.r")
source("query.r")

## Classify a set of developers based on the number of commits made withing a
## time range, core developers are those which are responsible for a given
## percentage of the work load with a default threshold set a 80% according
## to Ref: Terceiro A, Rios LR, Chavez C (2010) An empirical study on
##         the structural complexity introduced by core and peripheral
##         developers in free software projects.
get.developer.class.con <- function(con, project.id, start.date, end.date) {
  commit.count.df <- get.commits.by.date.con(con, project.id, start.date, end.date,
                                             commit.count=TRUE)
  developer.class <- get.developer.class(commit.count.df)

  return(developer.class)
}

## Low-level function to compute classification
get.developer.class <- function(author.commit.count, threshold=0.8) {
  author.commit.count <- author.commit.count[order(-author.commit.count$freq),]
  num.commits <- sum(author.commit.count$freq)
  commit.threshold <- round(threshold * num.commits)
  core.test <- cumsum(author.commit.count$freq) < commit.threshold
  core.developers <- author.commit.count[core.test,]
  peripheral.developers <- author.commit.count[!core.test,]
  res <- list(core=core.developers, peripheral=peripheral.developers)

  return(res)
}

## Determine developer class based on vertex centrality
get.developer.class.centrality <- function(edgelist, vertex.ids, threshold=0.8,
                                           FUN=igraph::degree) {
  graph <- graph.data.frame(edgelist, directed=TRUE,
                            vertices=data.frame(vertex.ids))
  centrality.vec <- sort(FUN(graph), decreasing=T)
  centrality.df <- data.frame(author=names(centrality.vec),
                              centrality=as.vector(centrality.vec))
  centrality.threshold <- threshold * sum(centrality.vec)
  core.test <- cumsum(centrality.vec) < centrality.threshold
  core.developers <- centrality.df[core.test,]
  peripheral.developers <- centrality.df[!core.test,]
  res <- list(core=core.developers, peripheral=peripheral.developers)

  return(res)
}
