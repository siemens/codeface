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

widget.contributors.pagerank <- createRangeIdWidgetClass(
  "widget.contributors.pagerank",
  "PageRank of Contributors"
)
widget.contributors.pagerank$html = tableOutput

renderWidget.widget.contributors.pagerank <- function(w, range.id) {
  if (is.null(range.id)) { range.id <- w$range.ids[[1]] }
  renderTable({prepare.prank.table(range.id, 0)})
}

widget.contributors.pagerank.transposed <- createRangeIdWidgetClass(
  "widget.contributors.pagerank.transposed",
  "Transposed PageRank of Contributors"
)
widget.contributors.pagerank.transposed$html = tableOutput

renderWidget.widget.contributors.pagerank.transposed <- function(w, range.id) {
  if (is.null(range.id)) { range.id <- w$range.ids[[1]] }
  renderTable({prepare.prank.table(range.id, 1)})
}

widget.contributors.commits <- createRangeIdWidgetClass(
  "widget.contributors.commits",
  "Contributors by Commit"
)
widget.contributors.commits$html = tableOutput

renderWidget.widget.contributors.commits <-  function(w, range.id) {
 if (is.null(range.id)) { range.id <- w$range.ids[[1]] }
 renderTable({prepare.commits.table(range.id)})
}

widget.contributors.changes <- createRangeIdWidgetClass(
  "widget.contributors.changes",
  "Contributors by Changes"
)
widget.contributors.changes$html = tableOutput

renderWidget.widget.contributors.changes <- function(w, range.id) {
 if (is.null(range.id)) { range.id <- w$range.ids[[1]] }
 renderTable({prepare.changes.table(range.id)})
}

