#! /usr/bin/env Rscript

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
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

detailpage <- list(name="widget.contributors.pagerank,widget.contributors.pagerank.transposed,widget.contributors.commits,widget.contributors.changes",
                   title="Authors / Pagerank")

prepare.prank.table <- function(range.id, technique) {
  prank.id <- reactive({get.pagerank.id.con(conf$con, range.id, technique)})
  if (is.null(prank.id())) {
    ## No pagerank stored, either because the calculation
    ## failed, or (much more ikely) because only a single
    ## person contributed in the release range under consideration
    return(NULL)
  }

  dat <- query.pagerank(conf$con, prank.id())
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
  html=widget.tableOutput.html,
  detailpage=detailpage
)

createWidgetClass(
  c("widget.contributors.pagerank.transposed", "widget.rangeid"),
  "Transposed PageRank of Contributors",
  "Interpretational aid: The transposed Page rank focuses on being tagged",
  c("basics", "collaboration"),
  2, 1,
  html=widget.tableOutput.html,
  detailpage=detailpage
)

createWidgetClass(
  c("widget.contributors.commits", "widget.rangeid"),
  "Contributors by Commit",
  "List of contributors and their commits",
  c("basics", "collaboration"),
  1, 1,
  html=widget.tableOutput.html,
  detailpage=detailpage
)

createWidgetClass(
  c("widget.contributors.changes", "widget.rangeid"),
  "Contributors by Changes",
  "List of Contributors by code changes",
  c("basics", "collaboration"),
  2, 1,
  html=widget.tableOutput.html,
  detailpage=detailpage
)

## Output a table, or use a custom replacement text if the
## table is empty. Code inspired from shiny, but much simpler
## for our purposes.
render.text.or.table <- function (expr, ..., text = "", env = parent.frame(),
                                  quoted = FALSE) {
    installExprFunction(expr, "func", env, quoted)
    markRenderFunction(tableOutput, function() {
        classNames <- "data table table-bordered table-condensed"
        data <- func()
        if (is.null(data) || identical(data, data.frame()))
            return(paste(utils::capture.output(cat(text)), collapse = "\n"))
        return(paste(utils::capture.output(print(xtable::xtable(data,
            ...), type = "html", html.table.attributes = paste("class=\"",
            classNames, "\"", sep = ""), ...)),
            collapse = "\n"))
    })
}


PR.NOT.COMPUTED <- paste("Page rank could not be computed ",
		         "(likely, only a single developer contributed ",
		         "to the cycle).")
renderWidget.widget.contributors.pagerank <- function(w) {
  render.text.or.table({prepare.prank.table(w$view(), 0)},
                       text=PR.NOT.COMPUTED)
}

renderWidget.widget.contributors.pagerank.transposed <- function(w) {
  render.text.or.table({prepare.prank.table(w$view(), 1)},
                       text=PR.NOT.COMPUTED)
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

