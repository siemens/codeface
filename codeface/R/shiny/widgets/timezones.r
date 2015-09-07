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
## Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
## All Rights Reserved.

library(png)
library(grid)

source("../../timezones.r", chdir=TRUE)

## This image was obtained from
## http://commons.wikimedia.org/wiki/File:UTC_hue4map_X_world_Robinson.png
## License: Creative Commons CC0 1.0 Universal Public Domain Dedication
## It is modified to ease computer identification of the time zones as
## described in the following.
image <- readPNG("tz.png")

## Map red/green values in the map onto time zones:
## "Red" values can be:
##  0x80: Time zone with no summer time
##  0x40: Time zone with summer time
##  0x41: Time zone with summer time, southern hemisphere (if ambiguous)
## "Green" values are 0x80 + (offset to GMT in quarter-hours)

## Construct a raster RGBA image of the world
## given an activity list as a list of timezone names : intensity
## where intensity must be between 0.0 and 1.0. Zones where intensity
## is 0 are treated as "not active".
get.tz.image <- function(timezone.intensity) {
  image.int <- image*255
  new.image <- image
  where.0x80 <- image.int[,,1] == 0x80
  where.0x40 <- image.int[,,1] == 0x40
  where.0x41 <- image.int[,,1] == 0x41
  where.any <- where.0x80 | where.0x40 | where.0x41
  ## Set all land areas to be black at 20% opacity
  new.image[,,1:3][where.any] <- 0.0
  new.image[,,4][where.any] <- 0.2

  for (name in names(timezone.intensity)) {
    raw.intensity <-  timezone.intensity[[name]]
    if (raw.intensity > 0) {
      if (name %in% tz.no.dst) {
        index.green <- which(tz.no.dst == name)
        index.red <- where.0x80
      } else if (name %in% tz.dst) {
        index.green <- which(tz.dst == name)
        index.red <- where.0x40
      } else if (name %in% tz.dst.south) {
        index.green <- which(tz.dst.south == name)
        index.red <- where.0x41
      } else {
        stop(paste("Unknown time zone: ", name))
      }
      ## Set selected areas to be red with the opacity
      ## proportional to the intensity (but at least 20%)
      intensity <- raw.intensity*(1 - 0.2) + 0.2
      where <- index.red & (image.int[,,2] == index.green)
      new.image[,,1][where] <- 1.0
      new.image[,,4][where] <- intensity
    }
  }
  new.image
}

## Test widget: Each timezone is a view
createWidgetClass(
  "widget.timezones.test1",
  "Show timezones on a map", "Shows timezones on a map",
  c("testing"),
  2, 1
)

renderWidget.widget.timezones.test1 <- function(w) {
  renderPlot({
    active.tz <- list()
    index <- as.integer(w$view())
    if (index > 0) {
      active.tz[[timezones[[index]]]] <- 1.0
    } else {
      for (tz in timezones) {
        active.tz[tz] <- 1.0
      }
    }
    this.image <- get.tz.image(active.tz)
    grid.raster(this.image)
  })
}

listViews.widget.timezones.test1 <- function(w) {
  reactive({
    tz <- 0:length(timezones)
    names(tz)[2:(length(timezones)+1)] <- timezones
    names(tz)[1] <- "All"
    tz
  })
}

## Test widget: Each half-hour UTC offset from -12h to 12h is a view
createWidgetClass(
  "widget.timezones.test2",
  "Test mapping offset->timezone", "Test mapping offset-timezone",
  c("testing"),
  2, 1
)

renderWidget.widget.timezones.test2 <- function(w) {
  renderPlot({
    # this is: num [1:745, 1:1425, 1:4]
    active.tz <- list()
    minutes <- as.integer(w$view())
    active.zones <- timestamp.offset.to.timezone(as.POSIXct(1400000000, origin="1970-01-01"), minutes)
    for (tz in active.zones) {
      active.tz[tz] <- 1.0
    }
    this.image <- get.tz.image(active.tz)
    grid.raster(this.image)
  })
}

listViews.widget.timezones.test2 <- function(w) {
  reactive({
    minutes <- (-12*2):(12*2)*30
    names(minutes) <- as.character(minutes)
    minutes
  })
}

## Show timezones of commits
createWidgetClass(
  "widget.timezones.commits",
  "Developer commit timezones", "Timezones where commits were made",
  NULL,
  2, 1
)

## Extract timezone information from commits
initWidget.widget.timezones.commits <- function(w) {
  ## Call superclass
  w <- NextMethod(w)
  w$data <- reactive({
    ## Query all commits that have author timezones
    res <- dbGetQuery(conf$con, str_c("SELECT authorTimezones,",
                                      " COUNT(*) as count",
                                      " FROM commit WHERE",
                                      " projectId=", w$pid(),
                                      " GROUP BY authorTimezones"
                                      ))

    ## Set up a list of timezone names : 0
    tzcount <- lapply(timezones, function(x) {0})
    names(tzcount) <- timezones

    ## Process all commits and fill the list
    lst <- Reduce(function(l, i) {
      if (!is.na(res$authorTimezones[[i]])) {
        zones <- unlist(strsplit(res$authorTimezones[[i]], split=";",
                                 fixed=TRUE))
        l[zones] <- unlist(l[zones]) + res$count[[i]]
      }
      l
    }, 1:nrow(res), init=tzcount)

    ## Normalise intensity to the maximum number of commits in a time zone
    max.val <- max(unlist(lst))
    if (max.val > 0) {
      lst <- lapply(lst, function(x) { x * 1.0/max.val })
    }
    lst
  })

  return(w)
}

## Plot the time zone information
renderWidget.widget.timezones.commits <- function(w) {
  renderPlot({
    active.tz <- w$data()
    no.data <- FALSE

    ## If there is no time zone information, just plot a randomly
    ## filled world map with a "no data" marker
    no.data <- FALSE
    if (sum(unlist(active.tz)) == 0) {
      no.data <- TRUE
      active.tz <- list()
      values <- runif(length(timezones), -3, 1)

      for (tzi in 1:length(timezones)) {
        if (values[tzi] > 0) {
          active.tz[timezones[[tzi]]] <- values[tzi]
        }
      }
      active.tz["Europe/Berlin"] <- 1.0
      active.tz["Europe/London"] <- 0.0001
    }

    this.image <- get.tz.image(active.tz)
    grid.raster(this.image)

    if (no.data) {
      grid.text("No data available!", rot=20,
                gp=gpar(fontsize=40, col="darkgrey"))
    }
  })
}

