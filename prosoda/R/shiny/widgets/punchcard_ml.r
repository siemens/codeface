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

source("../../ts_utils.r", chdir=TRUE)

## Generate commity activity punch card datasets for all cycles
## of a given project
gen.punchcard.ml <- function(con, ml.id, range.id) {
  dat <- query.ml.activity(conf$con, ml.id, range.id)

  if (!is.null(dat)) {
    act.ts <- xts(x=1:length(dat), order.by=dat)
    res <- compute.hourly.statistics(act.ts, FUN=length)
  } else {
    res <- NULL
  }

  return(res)
}

gen.punchcards.ml <- function(con, pid, ml.id) {
  range.ids.list <- query.range.ids.con(con, pid)
  cycles <- get.cycles.con(con, pid)

  res <- lapply(range.ids.list, function(range.id) {
    res <- gen.punchcard.ml(con, ml.id, range.id)
    if (!is.null(res)) {
      res <- cbind(res, cycle=cycles$cycle[cycles$range.id==range.id])
    }

    return(res)
  })

  res <- do.call(rbind, res)
  return(res)
}

widget.punchcard.ml <- list(
  title = "Mailing list punchcard",
  size.x = 2,
  size.y = 1,
  new = function(pid) {
    w <- make.widget(pid)
    class(w) <- c("widget.punchcard.ml", w$class)
    w$plots <- dbGetQuery(conf$con, str_c("SELECT id, name FROM mailing_list WHERE projectId=", pid))
    w$boundaries <- get.cycles.con(conf$con, pid)
    return (w)
  },
  html = function(id) { plotOutput(id, width="100%", height="100%") }
)
widget.list$widget.punchcard.ml <- widget.punchcard.ml

renderWidget.widget.punchcard.ml <- function(w, view=NULL) {
  if (is.null(view)) {
    view <- w$plots$id[[1]]
  }
  res <- gen.punchcards.ml(conf$con, w$pid, view)
  renderPlot({
    g <- ggplot(res, aes(x=hour, y=day, size=size)) + geom_point() +
      facet_wrap(~cycle)
    print(g)
  })
}

listViews.widget.punchcard.ml <- function(w) {
  l <- w$plots$id
  names(l) <- w$plots$name
  l
}

