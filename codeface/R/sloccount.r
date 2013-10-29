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
## Copyright 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Codeface interface to sloccount
suppressPackageStartupMessages(library(stringr))
source("system.r")
source("config.r")

## Call sloccount on a given directory containing all source files
## that belong to a project. cost.per.py specifies how much a developer
## costs, in a currency of your choice.
gather.sloccount.results <- function(dir, cost.per.py) {
  return(do.system("sloccount", str_c("--personcost ", cost.per.py, " ", dir)))
}

## Append a new entry to a sloccount time series in the database
add.sloccount.ts <- function(conf, plot.id, commit.date, values) {
  if (nrow(values$metrics) > 0) {
    dat <- cbind(plotId=plot.id, time=commit.date, values$metrics)
    res <- dbWriteTable(conf$con, "sloccount_ts", dat, append=TRUE, row.names=FALSE)

    if (!res) {
      stop("Internal error: Could not write sloccount timeseries into database!")
    }
  }
}

## Extract a named group match from a given string for a given regexpr results.
## matched is a result of regexpr() with named groups,
## str is a character vector containing the raw data. group specifies
## the group name to extract from the match.
get.matched.part <- function(str, matched, group) {
  res <- lapply(1:length(str), function(i) {
    if (nrow(attr(matched, "capture.start")) == 0 ||
        nrow(attr(matched, "capture.length")) == 0) {
      return(NULL)
    }

    return(substr(str[i], attr(matched, "capture.start")[i, group],
                  attr(matched, "capture.start")[i, group] +
                  attr(matched, "capture.length")[i, group] - 1))
  })

  return(do.call(c, res))
}

## Given a list of characters and a pattern, select all matching lines
## and then apply the regexp on them.
grep.and.match <- function(dat, pattern) {
  m <- grep(pattern, dat, perl=TRUE)
  matched <- regexpr(pattern, dat[m], perl=TRUE)

  return(list(idx=m, match=matched))
}

## Sloccount returns numbers as 1,234.56 when it really means 1234.56.
## Fix up such numbers (since the result may come in as a factor, we
## convert it to character before parsing the float)
fixup.number <- function(num) {
  if (is.null(num)) {
    return(NULL)
  }

  return(as.numeric(as.character(gsub(",", "", num))))
}

## Given the output of sloccount, create a data frame that contains
## two columns: One specifies which language is used, and the other
## one the line count for the language.
## This is inaccurate when a language is used (or filename suffixes
## are employed) that sloccount does not recognise: In this case, the
## corresponding code is simply ignored.
parse.sloccount.langs <- function(output) {
  ## Pattern for the language list given by sloccount
  pattern <- "^(?<lang>[^:]+):[[:blank:]]+(?<lines>[[:digit:]]+) \\("
  matched <- grep.and.match(output, pattern)

  ## Extract the matched groups to combine them into a data frame
  res <- data.frame(lang=get.matched.part(output[matched$idx],
                                          matched$match, "lang"),
                    lines=get.matched.part(output[matched$idx],
                                           matched$match, "lines"))
  res$lines <- as.integer(as.character(res$lines))

  res$fraction <- res$lines/sum(res$lines)

  return(res)
}

## Parse all money- and time related statistics offered by sloccount.
## Note that we don't interpret the currency unit (although sloccount assumes
## it to be dollars, most likely US). Pick your favourite.
parse.sloccount.metrics <- function(output) {
  num.regex <- "[[:digit:]\\.,]" # sloccount numbers include . and , besides digits
  pattern.effort <- str_c("Person-Years \\(Person-Months\\) = ",
                          "(?<person_years>", num.regex, "+) ",
                          "\\((?<person_months>", num.regex, "+)\\)")
  pattern.total.cost <- str_c("Cost to Develop[[:blank:]]+= ",
                              "\\$ (?<total_cost>", num.regex, "+)$")
  pattern.schedule <- str_c("Schedule Estimate[^=]+= ",
                            "(?<schedule_years>", num.regex, "+) ",
                            "\\((?<schedule_months>", num.regex, "+)\\)")
  pattern.avg.devel <- str_c("Average Number of Developers[^=]+= ",
                             "(?<avg_developers>", num.regex, "+)")

  matched <- grep.and.match(output, pattern.effort)
  person.months <- fixup.number(get.matched.part(output[matched$idx], matched$match,
                                                 "person_months"))

  matched <- grep.and.match(output, pattern.total.cost)
  total.cost <- fixup.number(get.matched.part(output[matched$idx], matched$match,
                                                 "total_cost"))

  matched <- grep.and.match(output, pattern.schedule)
  schedule.months <- fixup.number(get.matched.part(output[matched$idx],
                                                   matched$match,
                                                   "schedule_months"))

  matched <- grep.and.match(output, pattern.avg.devel)
  avg.devel <- fixup.number(get.matched.part(output[matched$idx], matched$match,
                                                   "avg_developers"))

  return(data.frame(person.months=person.months, total.cost=total.cost,
                    schedule.months=schedule.months, avg.devel=avg.devel))
}

## Return a list with two data frames: lang.info contains language
## related information, metrics has information about time and money
## aspects.
parse.sloccount <- function(output) {
  return(list(lang.info=parse.sloccount.langs(output),
              metrics=parse.sloccount.metrics(output)))
}

do.sloccount.analysis <- function(code.dir, cost.per.py=60000) {
  output <- gather.sloccount.results(code.dir, cost.per.py)
  res <- parse.sloccount(output)

  return(res)
}
