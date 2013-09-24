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
gen.punchcard <- function(con, pid, range.id) {
  dat <- gen.commits.activity(con, pid, range.id)
  act.ts <- xts(x=dat$DiffSize, order.by=ymd_hms(dat$commitDate, quiet=TRUE))
  res <- compute.hourly.statistics(act.ts, FUN=length)

  return(res)
}

gen.punchcards <- function(con, pid) {
  range.ids.list <- query.range.ids.con(con, pid)
  cycles <- get.cycles.con(con, pid)

  res <- lapply(range.ids.list, function(range.id) {
    res <- gen.punchcard(con, pid, range.id)
    res <- cbind(res, cycle=cycles$cycle[cycles$range.id==range.id])

    return(res)
  })

  res <- do.call(rbind, res)
  return(res)
}

createWidgetClass(
  "widget.punchcard",
  "Commit Punchcard",
  "Commit Punchcard",
  c("collaboration"),
  2, 1,
  detailpage=list(name="widget.weekend.fraction,widget.weekend.fraction.type,widget.punchcard",
                  title="Authors / Time of Activity") ## See weekend.r
)

initWidget.widget.punchcard <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$res <- reactive({gen.punchcards(conf$con, w$pid())})
  return(w)
}

renderWidget.widget.punchcard <- function(w) {
  renderPlot({
    g <- ggplot(w$res(), aes(x=hour, y=day, size=size)) + geom_point() +
      facet_wrap(~cycle)
    print(g)
  })
}
