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

## Time zones with no summer time (daylight savings time)
## the index is given as 0x80 + offset to UTC in quarter-hours
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
## If strict is TRUE, do not return approximate matches.
## Relevant timezone information can be obtained at
## http://en.wikipedia.org/wiki/Time_Zone
timestamp.offset.to.timezone <- function(timestamp, offset.minutes, strict=FALSE) {
  if (is.null(offset.minutes)) {
    return(timezones)
  }
  ## Express the timestamp in UTC
  ts.utc <- with_tz(timestamp, "UTC")
  tz.offsets <- sapply(timezones, function(tz) {
    ## This is the timestamp expressed in the timezone tz
    local.time <- with_tz(ts.utc, tz)
    ## This is the numerically same UTC date/time (note the force_tz)
    time.where.utc.has.same.value <- force_tz(local.time, "UTC")
    ## ..therefore this is the difference between UTC and local time
    ## given the date of the timestamp (relevant for summer time!)
    as.integer(difftime(time.where.utc.has.same.value, local.time, units="mins"))
  })
  ## Find zones where the offset is identical
  zones <- tz.offsets == offset.minutes
  ## If strict is not true and no zones are found, pick the closest ones
  if (!strict && !any(zones)) {
    loginfo(paste("Unusual time zone offset: ", offset.minutes))
    diff <- abs(tz.offsets - offset.minutes)
    zones <- diff == min(diff)
  }
  return(timezones[zones])
}

timezone.string <- function(date, timezone) {
    offset <- offset.minutes(timezone)
    timestamp <- as.POSIXct(date, origin="1970-01-01")
    zones <- timestamp.offset.to.timezone(timestamp, offset)
    do.call(paste, c(zones, list(sep=";")))
}

do.update.timezone.information <- function(conf, project.id) {
  ## Query all commits which have author timezones
  res <- dbGetQuery(conf$con, str_c("SELECT id, authorDate, authorTimeOffset",
                             " FROM commit WHERE (NOT authorTimeOffset IS ",
                             " NULL) AND projectId=", project.id))
  ## Process all commits and fill the list
  logdevinfo(paste("Processing", nrow(res), "commits for time zone info..."))

  zones <- mclapply(1:nrow(res), function(i) {
    val <- paste("(", res$id[[i]], ", '", timezone.string(res$authorDate[[i]],
                                                          res$authorTimeOffset[[i]]),
                 "')", sep="")
    return(val)
  })

  dbSendQuery(conf$con, str_c(
    "INSERT INTO commit (id, authorTimezones) VALUES ",
    do.call(paste, c(zones, list(sep=", "))),
    " ON DUPLICATE KEY UPDATE authorTimezones=VALUES(authorTimezones);"))

  logdevinfo(paste("Updated", length(zones), "timezone entries"))
}
