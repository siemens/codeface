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
## Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
## All Rights Reserved.

library(png)

## This image was obtained from
## http://commons.wikimedia.org/wiki/File:UTC_hue4map_X_world_Robinson.png
## License: Creative Commons CC0 1.0 Universal Public Domain Dedication
image <- readPNG("tz.png")

timezones <- c("America/New_York", "Europe/Berlin")

## This function transforms a POSIXct timestamp + a UTC offset into a list of
## possible timezones that had this offset or were close
## at the time of the timestamp.
## The offset is specified as (hours*100 + minutes)
## Relevant timezone information can be obtained at
## http://en.wikipedia.org/wiki/Time_Zone
timestamp.offset.to.timezone <- function(timestamp, offset, strict=FALSE) {
  if (is.null(offset)) {
    return(timezones)
  }
  ts.utc <- with_tz(timestamp, "UTC")
  tz.offsets <- sapply(timezones, function(tz) {
    local.time <- with_tz(ts.utc, tz)
    time.where.utc.has.same.value <- force_tz(local.time, "UTC")
    as.integer(difftime(time.where.utc.has.same.value, local.time, units="mins"))
  })
  offset.minutes <- 60 * as.integer(offset / 100) + as.integer(offset %% 100)
  zones <- tz.offsets == offset.minutes
  if (!strict && !any(zones)) {
    loginfo(paste("Unusual time zone offset: ", offset))
    diff <- abs(tz.offsets - offset.minutes)
    zones <- diff == min(diff)
  }
  return(timezones[zones])
}

## Widget which presents a processing overview for prosoda operators
createWidgetClass(
  "widget.timezones.commits",
  "Developer commit timezones", "Timezones where commits were made",
  2, 1
)

## Superclass constructor which already fills important variables
initWidget.widget.timezones.commits <- function(w) {
  # Call superclass
  w <- NextMethod(w)
  w$project.name <- reactive({query.project.name(conf$con, w$pid())})
  w$cycles <- reactive({get.cycles.con(conf$con, w$pid())})
  w$data <- reactive({
    pid <- w$pid()
    cycles <- w$cycles()
    list(
      n.commits = dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM commit WHERE projectId=", pid))[[1]],
    )
  })
  return(w)
}

renderWidget.widget.timezones.commits <- function(w) {
  renderPlot({
    str(image)
    grid.raster(image)
  })
}
