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

prepare.prank.table <- function(range.id, technique) {
  prank.id <- get.pagerank.id.con(conf$con, range.id, technique)
  dat <- query.pagerank(conf$con, prank.id)
  dat <- cbind(dat, prank.scaled=dat$prank/max(dat$prank))
  dat <- dat[c("name", "prank", "prank.scaled")]
  colnames(dat) <- c("Name", "Page rank", "Page rank (scaled)")

  return(dat)
}

prepare.commits.table <- function(range.id) {
  dat <- query.top.contributors.commits(conf$con, range.id)
  if (!is.null(dat)) {
    colnames(dat) <- c("Name", "Number of commits")
  }

  return(dat)
}

prepare.changes.table <- function(range.id) {
  dat <- query.top.contributors.changes(conf$con, range.id)
  if (!is.null(dat)) {
    colnames(dat) <- c("Name", "Added LOC", "Deleted LOC", "Total")
  }

  return(dat)
}

createWidgetClass(
  c("widget.contributors.pagerank", "widget.rangeid"),
  "PageRank of Contributors",
  "Interpretational aid: This Page rank focuses on giving tags",
  c("basics", "collaboration"),
  2, 1,
  html=widget.tableOutput.html
)

createWidgetClass(
  c("widget.contributors.pagerank.transposed", "widget.rangeid"),
  "Transposed PageRank of Contributors",
  "Interpretational aid: The transposed Page rank focuses on being tagged",
  c("basics", "collaboration"),
  2, 1,
  html=widget.tableOutput.html
)

createWidgetClass(
  c("widget.contributors.commits", "widget.rangeid"),
  "Contributors by Commit",
  "List of contributors and their commits",
  c("basics", "collaboration"),
  1, 1,
  html=widget.tableOutput.html
)

createWidgetClass(
  c("widget.contributors.changes", "widget.rangeid"),
  "Contributors by Changes",
  "List of Contributors by code changes",
  c("basics", "collaboration"),
  2, 1,
  html=widget.tableOutput.html
)

renderWidget.widget.contributors.pagerank <- function(w) {
  renderTable({prepare.prank.table(w$view(), 0)})
}

renderWidget.widget.contributors.pagerank.transposed <- function(w) {
  renderTable({prepare.prank.table(w$view(), 1)})
}

renderWidget.widget.contributors.commits <-  function(w) {
  renderTable({prepare.commits.table(w$view())})
}

renderWidget.widget.contributors.changes <- function(w) {
  renderTable({prepare.changes.table(w$view())})
}

widgetTitle.widget.contributors.pagerank <- function(w) { reactive({"PageRank"}) }
widgetTitle.widget.contributors.pagerank.transposed <- function(w) { reactive({"Transposed PageRank"}) }
widgetTitle.widget.contributors.commits <-  function(w) { reactive({"Commits"}) }
widgetTitle.widget.contributors.changes <- function(w) { reactive({"Changes"}) }

