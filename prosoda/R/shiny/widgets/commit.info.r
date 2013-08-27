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

## Get a commit.info.splom widget
make.widget.commit.info.splom <- createRangeIdWidgetClass(
  "widget.commit.info.splom",
  "Commit Information - Splom"
)

renderWidget.widget.commit.info.splom <- function(w, range.id=NULL) {
  if (is.null(range.id)) {
    range.id <- w$range.ids[[1]]
  }
  dat <- gen.commits.info(conf$con, w$pid, range.id)
  renderPlot({
    gen.commits.splom(dat$cmt.info, dat$plot.types)
  })
}

## Get a commit.info.corrgram widget
make.widget.commit.info.corrgram <- createRangeIdWidgetClass(
  "widget.commit.info.corrgram",
  "Commit Information - Correlations",
  2, 1
)

renderWidget.widget.commit.info.corrgram <- function(w, range.id=NULL) {
  if (is.null(range.id)) {
    range.id <- w$range.ids[[1]]
  }
  dat <- gen.commits.info(conf$con, w$pid, range.id)
  renderPlot({
    gen.commits.corrgram(dat$cmt.info, dat$plot.types)
  })
}

