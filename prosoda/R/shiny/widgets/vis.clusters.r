#! /usr/bin/env Rscript

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

detailpage <- list(name="widget.clusters.clusters,widget.clusters.correlations,widget.clusters.summary",
                   title="Collaboration Clusters")

source("../../clusters.r", chdir=TRUE)

## Create overviews about the types of collaboration graphs appearing in
## projects.

gen.clusters.list <- function(l, con) {
  clusters.list <- lapply(1:length(l), function(i) {
    g <- construct.cluster(con, l[[i]])

    ## Self-loops in the proximity analysis can become very strong;
    ## the resulting edges then typically destroy the visualisation
    ## completely. Get rid of them, thus.
    ## NOTE: simplify must be called before the cluster is annotated
    ## because the function
    if(!is.null(g)) {
      g <- simplify(g, remove.loops=TRUE)
    }

    return(g)
  })

  ## Remove empty clusters
  clusters.list[sapply(clusters.list, is.null)] <- NULL

  clusters.list <- lapply(clusters.list, function(g) {
    return(annotate.cluster(g))
  })

  return(clusters.list)
}

do.plot <- function(g) {
  V(g)$name <- NA
  plot(g)
}

prepare.clusters <- function(con, pid, range.id) {
  l <- query.cluster.ids.con(con, pid, range.id, "Spin Glass Community")
  clusters.list <- gen.clusters.list(l, con)

  ## Sort the clusters by number of vertices
  sizes <- sapply(clusters.list, vcount)
  clusters.list <- clusters.list[sort(sizes, index.return=TRUE, decreasing=TRUE)$ix]

  max.length <- 8
  if (length(clusters.list) < max.length) {
    max.length <- length(clusters.list)
  }

  return(clusters.list[1:max.length])
}

do.cluster.plots <- function(clusters.list) {
  par(mfcol=c(2,4))
  for (i in 1:length(clusters.list)) {
    do.plot(clusters.list[[i]])
  }
}

gen.cluster.info <- function(g) {
  return(data.frame(Reciprocity=g$rec, Strength=g$strength,
                    Degree=g$deg.graph, Size=g$size,
                    Cent.degree=g$cent.deg, Cent.closeness=g$cent.clo,
                    Cent.betweenness=g$cent.bet, Cent.eigenvec=g$cent.evc))
}

gen.cluster.summary <- function(clusters.list) {
  res <- lapply(1:length(clusters.list), function(i) {
    df <- gen.cluster.info(clusters.list[[i]])
    cbind(ID=i, df)
  })

  return(do.call(rbind, res))
}

createWidgetClass(
  c("widget.clusters.clusters", "widget.clusters", "widget.rangeid"),
  "Clusters",
  "Developer collaboration Clusters",
  c("collaboration"),
  2, 1,
  detailpage=detailpage
)

initWidget.widget.clusters <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$cluster.list <- reactive({prepare.clusters(conf$con, w$pid(), w$view())})
  return(w)
}

renderWidget.widget.clusters.clusters <- function(w) {
  renderPlot({
    do.cluster.plots(w$cluster.list())
  }, height=1024, width=2048)
}

createWidgetClass(
  c("widget.clusters.correlations", "widget.clusters", "widget.rangeid"),
  "Cluster Correlations",
  "Cluster Correlations",
  c("collaboration"),
  detailpage=detailpage
)

initWidget.widget.clusters.correlations <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$dat <- reactive({
    dat <- gen.cluster.summary(w$cluster.list())
    dat <- dat[,c("Reciprocity", "Strength", "Degree", "Size",
                  "Cent.degree", "Cent.closeness", "Cent.betweenness",
                  "Cent.eigenvec")]
    dat
  })
  return(w)
}

renderWidget.widget.clusters.correlations <- function(w) {
  renderPlot({
    corrgram(w$dat(), order=FALSE, lower.panel=panel.shade, upper.panel=panel.pie,
              text.panel=panel.txt, main="")
  })
}

createWidgetClass(
  c("widget.clusters.summary", "widget.clusters", "widget.rangeid"),
  "Cluster Summary",
  "Tabular summary of clusters",
  c("collaboration"),
  3, 1,
  html=widget.tableOutput.html,
  detailpage=detailpage
)

renderWidget.widget.clusters.summary <- function(w, range.id=NULL) {
  renderTable({gen.cluster.summary(w$cluster.list())})
}

