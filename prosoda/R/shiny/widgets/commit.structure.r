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

detailpage = list(name="widget.commit.structure.mds,widget.commit.structure.princomp",
                  title="Commit Structure")

## Perform dimensionality reduction on the per-cycle commits using
## multi-dimensional scaling
do.mds <- function(cmt.info.list, k=2, method="euclidean") {
  res <- lapply(cmt.info.list, function(cmt.info) {
    d <- stats::dist(cmt.info, method=method)
    fit <- cmdscale(d, k=k)
    ## Alternative: fit <- isoMDS(d, k=2)

    res <- data.frame(x=fit[,1], y=fit[,2], inRC=cmt.info$inRC,
                      cycle=cmt.info$cycle)
    return(res)
  })

  res <- do.call(rbind, res)
  return(res)
}

## Perform dimensionality reduction on the per-cycle commits using
## principal component analysis
do.prcomp <- function(cmt.info.list, subset, method="euclidean") {
  res <- lapply(cmt.info.list, function(cmt.info) {
    if (all(is.na(cmt.info$NumTags))) {
      cmt.info <- cmt.info[!(colnames(cmt.info) %in% "NumTags")]
      cmt.info <- cmt.info[!(colnames(cmt.info) %in% "NumSignedOffs")]
      subset <- subset[!(subset %in% c("NumTags", "NumSignedOffs"))]
    }
    cmt.info <- na.omit(cmt.info)
    d <- stats::dist(cmt.info, method=method)

    pr <- prcomp(cmt.info[subset], cor=TRUE, scale=TRUE)

    ## Sum up the proportion of variance for the first two
    ## principal components as an indicator how well the dimensionality
    ## reduced data reflects reality
    prop <- sum(summary(pr)$importance["Proportion of Variance", c("PC1", "PC2")])
    res <- data.frame(x=pr$x[,1], y=pr$x[,2], inRC=cmt.info$inRC,
                      cycle=cmt.info$cycle, prop=prop)
    return(res)
  })

  res <- do.call(rbind, res)
  return(res)
}

createWidgetClass(
  c("widget.commit.structure.mds", "widget.commit.structure"),
  "Commit Structure - Multidimensional Scaling",
  "Multidimensional scaling of the commit structure",
  topics = c("construction"),
  size.x = 2,
  size.y = 1,
  detailpage = detailpage
)

createWidgetClass(
  c("widget.commit.structure.princomp", "widget.commit.structure"),
  "Commit Structure - Principal Components",
  "Principal component analysis of the commit structure",
  topics = c("construction"),
  size.x = 2,
  size.y = 1,
  detailpage = detailpage
)

initWidget.widget.commit.structure <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$subset <- c("CmtMsgBytes", "ChangedFiles", "DiffSize", "NumTags", "NumSignedOffs")
  w$cmt.info.list <- reactive({get.cmt.info.list(conf$con, w$pid(), w$subset)})
  return(w)
}

initWidget.widget.commit.structure.mds <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$data <- reactive({do.mds(w$cmt.info.list(), method="euclidean")})
  return(w)
}

initWidget.widget.commit.structure.princomp <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$data <- reactive({do.prcomp(w$cmt.info.list(), w$subset)})
  return(w)
}

renderWidget.widget.commit.structure.mds <- function(w) {
  renderPlot({
    g <- ggplot(w$data(), aes(x=x, y=y, colour=inRC)) + geom_point() +
      facet_wrap(~cycle)
    print(g)
  })
}

renderWidget.widget.commit.structure.princomp <- function(w) {
  renderPlot({
    g <- ggplot(w$data(), aes(x=x, y=y, colour=prop, shape=inRC)) +
      geom_point() + facet_wrap(~cycle)
    print(g)
  })
}
