## This file is part of prosoda. prosoda is free software: you can
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

## Helper functions for two-mode author-interest graphs stored in the database

## Given an igraph object for a two-mode community, construct
## the layout parameters and generate an input file in dot format
## that can be fed through graphviz
gen.twomode.dot <- function(con, g, outfile) {
  ## Convert the in-DB (0,1) representation of type into human
  ## readable strings
  V(g)$type <- mapvalues(V(g)$type, from=c(0,1), to=c("person", "keyword"))

  ## Determine the vertex indices which represent persons
  persons.idx <- which(V(g)$type == "person")

  ## Set all attributes that are relevant for the graph layout
  V(g)$style <- "filled"
  V(g)$label <- V(g)$name
  V(g)$shape <- "oval"
  V(g)$shape[persons.idx] <- "box"
  V(g)$style <- "filled"
  V(g)$fillcolor <- "red"
  V(g)$fillcolor[persons.idx] <- "blue"
  V(g)$fontsize <- (V(g)$degree^0.25)*10

  pid.list <- as.numeric(V(g)$label[persons.idx])
  names.list <- sapply(pid.list, function(pid) {
    return(query.person.name(con, pid))
  })
  V(g)$label[persons.idx] <- names.list

  E(g)$penwidth <- E(g)$weight*5

  write.graph(g, outfile, format="dot")
  ## NOTE: Process the resulting file with
  ## dot file.dot -Kneato -Gsplines=true -Goverlap=prism -Tpdf -o/path/to/file.pdf
}


## Given the id of a two-mode graph, query vertices and edge list from
## the database, and construct an igraph object
get.twomode.graph <- function(con, type, ml, range.id) {
  vertices.df <- query.twomode.vertices(con, type, ml, range.id)
  edges.df <- query.twomode.edgelist(con, type, ml, range.id)

  g <- graph.data.frame(edges.df, directed=FALSE, vertices=vertices.df)

  return(g)
}
