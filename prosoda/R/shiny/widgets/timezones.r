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

## Map red/green values in the map onto time zones
## "Red" values can be:
##  0x80: Time zone with no summer time
##  0x40: Time zone with summer time
##  0x41: Time zone with summer time, southern hemisphere (if ambiguous)
## "Green" values are 0x80 + (offset to GMT in quarter-hours)

# Time zones with no summer time (daylight savings time)
tz.no.dst <- character(0)
#tz.no.dst[0x80 -11*4 - 0] = "Pacific/Midway" # Not on the map
tz.no.dst[0x80 -11*4 - 0] = "Pacific/Samoa"
tz.no.dst[0x80 -10*4 - 0] = "Pacific/Honolulu"
tz.no.dst[0x80 - 9*4 - 2] = "Pacific/Marquesas"
tz.no.dst[0x80 - 9*4 - 0] = "Pacific/Gambier"
tz.no.dst[0x80 - 8*4 - 0] = "Pacific/Pitcairn"
tz.no.dst[0x80 - 7*4 - 0] = "America/Dawson_Creek"
tz.no.dst[0x80 - 6*4 - 0] = "America/Belize"
tz.no.dst[0x80 - 5*4 - 0] = "America/Atikokan"
tz.no.dst[0x80 - 4*4 - 2] = "America/Caracas"
tz.no.dst[0x80 - 4*4 - 0] = "America/Antigua"
tz.no.dst[0x80 - 3*4 - 0] = "America/Araguaina"
tz.no.dst[0x80 - 2*4 - 0] = "America/Noronha"
tz.no.dst[0x80 - 1*4 - 0] = "Atlantic/Cape_Verde"
tz.no.dst[0x80 + 0*4 + 0] = "Atlantic/Reykjavik"
tz.no.dst[0x80 + 1*4 + 0] = "Africa/Algiers"
tz.no.dst[0x80 + 2*4 + 0] = "Africa/Johannesburg"
tz.no.dst[0x80 + 3*4 + 0] = "Europe/Kaliningrad"
tz.no.dst[0x80 + 4*4 + 0] = "Europe/Moscow"
tz.no.dst[0x80 + 4*4 + 2] = "Asia/Kabul"
tz.no.dst[0x80 + 5*4 + 0] = "Asia/Samarkand"
tz.no.dst[0x80 + 5*4 + 2] = "Asia/Calcutta"
tz.no.dst[0x80 + 5*4 + 3] = "Asia/Kathmandu"
tz.no.dst[0x80 + 6*4 + 0] = "Asia/Yekaterinburg"
tz.no.dst[0x80 + 6*4 + 2] = "Asia/Rangoon"
tz.no.dst[0x80 + 7*4 + 0] = "Asia/Bangkok"
tz.no.dst[0x80 + 8*4 + 0] = "Asia/Hong_Kong"
tz.no.dst[0x80 + 9*4 + 0] = "Asia/Irkutsk"
tz.no.dst[0x80 + 9*4 + 2] = "Australia/Darwin"
tz.no.dst[0x80 +10*4 + 0] = "Asia/Yakutsk"
tz.no.dst[0x80 +11*4 + 0] = "Asia/Vladivostok"
tz.no.dst[0x80 +11*4 + 2] = "Pacific/Norfolk"
tz.no.dst[0x80 +12*4 + 0] = "Asia/Kamchatka"

## Northern or unambiguous DST
tz.dst <- character(0)
tz.dst[0x80 -10*4 - 0] = "America/Adak"
tz.dst[0x80 - 9*4 - 0] = "America/Anchorage"
tz.dst[0x80 - 8*4 - 0] = "America/Vancouver"
tz.dst[0x80 - 7*4 - 0] = "America/Denver"
tz.dst[0x80 - 6*4 - 0] = "America/Chicago"
tz.dst[0x80 - 5*4 - 0] = "America/Detroit"
tz.dst[0x80 - 4*4 - 0] = "America/Halifax"
tz.dst[0x80 - 3*4 - 2] = "Canada/Newfoundland"
tz.dst[0x80 - 3*4 - 0] = "America/Bahia"
tz.dst[0x80 - 1*4 - 0] = "Atlantic/Azores"
tz.dst[0x80 + 0*4 + 0] = "Europe/London"
tz.dst[0x80 + 1*4 + 0] = "Europe/Berlin"
tz.dst[0x80 + 2*4 + 0] = "Europe/Athens"
tz.dst[0x80 + 3*4 + 2] = "Asia/Tehran"
tz.dst[0x80 + 4*4 + 0] = "Asia/Baku"
tz.dst[0x80 + 9*4 + 2] = "Australia/Adelaide"
tz.dst[0x80 +10*4 + 0] = "Australia/Canberra"
tz.dst[0x80 +12*4 + 0] = "Pacific/Auckland"
tz.dst[0x80 +12*4 + 3] = "Pacific/Chatham"

# Southern/ambiguous DST
tz.dst.south <- character(0)
tz.dst.south[0x80 - 4*4] = "America/Santiago"
tz.dst.south[0x80 - 3*4] = "America/Sao_Paulo"

# List of timezones
timezones <- unique(c(tz.no.dst, tz.dst, tz.dst.south))
timezones <- timezones[!is.na(timezones)]

## Convert an offset specified as (hours*100 + minutes) into minutes
offset.minutes <- function(offset) {
  60 * as.integer(offset / 100) + as.integer(offset %% 100)
}

## This function transforms a POSIXct timestamp + a UTC offset into a list of
## possible timezones that had this offset or were close
## at the time of the timestamp.
## Relevant timezone information can be obtained at
## http://en.wikipedia.org/wiki/Time_Zone
timestamp.offset.to.timezone <- function(timestamp, offset.minutes, strict=FALSE) {
  if (is.null(offset.minutes)) {
    return(timezones)
  }
  ts.utc <- with_tz(timestamp, "UTC")
  tz.offsets <- sapply(timezones, function(tz) {
    local.time <- with_tz(ts.utc, tz)
    time.where.utc.has.same.value <- force_tz(local.time, "UTC")
    as.integer(difftime(time.where.utc.has.same.value, local.time, units="mins"))
  })
  zones <- tz.offsets == offset.minutes
  if (!strict && !any(zones)) {
    loginfo(paste("Unusual time zone offset: ", offset.minutes))
    diff <- abs(tz.offsets - offset.minutes)
    zones <- diff == min(diff)
  }
  return(timezones[zones])
}

## timezone.intensity should be a list of timezone names -> intensity
get.tz.image <- function(timezone.intensity) {
  ## create replacement map. The value on the right specifies the intensity
  image.int <- image*255
  new.image <- image
  where.0x80 <- image.int[,,1] == 0x80
  where.0x40 <- image.int[,,1] == 0x40
  where.0x41 <- image.int[,,1] == 0x41
  where.any <- where.0x80 | where.0x40 | where.0x41
  new.image[,,1:3][where.any] <- 0.0#0x90/255.
  new.image[,,4][where.any] <- 0.2

  for (name in names(timezone.intensity)) {
    if (name %in% tz.no.dst) {
      index.green <- which(tz.no.dst == name)
      index.red <- 0x80
    } else if (name %in% tz.dst) {
      index.green <- which(tz.dst == name)
      index.red <- 0x40
    } else if (name %in% tz.dst.south) {
      index.green <- which(tz.dst.south == name)
      index.red <- 0x41
    } else {
      stop(paste("Unknown time zone: ", name))
    }
    intensity <- timezone.intensity[[name]]*(1 - 0.2) + 0.2
    where <- (image.int[,,1] == index.red) & (image.int[,,2] == index.green)
    new.image[,,4][where] <- intensity
    new.image[,,1][where] <- 1.0
  }
  #new.image[,,1:3] <- 0x80/255.
  new.image
}

## Widget which presents a processing overview for prosoda operators
createWidgetClass(
  "widget.timezones.test1",
  "Show timezones on a map", "Shows timezones on a map",
  c("testing"),
  2, 1
)

renderWidget.widget.timezones.test1 <- function(w) {
  renderPlot({
    # this is: num [1:745, 1:1425, 1:4]
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

## Widget which presents a processing overview for prosoda operators
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
    str(active.zones)
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

## Widget which presents a processing overview for prosoda operators
createWidgetClass(
  "widget.timezones.commits",
  "Developer commit timezones", "Timezones where commits were made",
  NULL,
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
    # this is: num [1:745, 1:1425, 1:4]
    active.tz = list()
    values <- runif(length(timezones), -1, 1)
    for (tzi in 1:length(timezones)) {
      if (values[tzi] > 0) {
        active.tz[timezones[[tzi]]] <- values[tzi]
      }
    }
    active.tz["Europe/Berlin"] <- 1.0
    active.tz["Europe/London"] <- 0.0001
    this.image <- get.tz.image(active.tz)
    grid.raster(this.image)
  })
}

