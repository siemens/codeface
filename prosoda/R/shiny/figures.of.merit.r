# This file is part of prosoda.  prosoda is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>

## Status codes as factors from one of good, warn, bad, error
## factors obtained with as.status obey good > warn > bad > error
## You can therefore use status <- min(status, status.warn)
## to only update a status value towards "bad"/"error"
## as well as using median() or mean() to summarize errors.
status.codes <- c("error", "bad", "mostly.bad", "warn", "mostly.good", "good")
status.codes.colors <- c(color.neutral, color.bad, color.bad, color.warn, color.good, color.good)

as.status <- function(x) { factor(x, levels=status.codes, ordered=TRUE) }
status.good <- as.status("good")
status.mostly.good <- as.status("mostly.good")
status.warn <- as.status("warn")
status.mostly.bad <- as.status("mostly.bad")
status.bad <- as.status("bad")
status.error <- as.status("error")

#' Fit a line to the plot given by name in the given period
#'
#' In the time period from the last timestamp in the timeseries given by name
#' and the number of days given in period.in.days before that, fit a line
#' to the values in the time series.
#'
#' @param pid the ID of the project
#' @param name the name of the time series to fit
#' @param period.in.days the time before the last timestamp to analyse
#' @return a list with the relative increase per year measured by the mean,
#'         and the significance in terms of the standard deviation (sigma)
#'
#' If 'Sigma in terms of the standard deviation' is an unfamiliar quantity, see:
#' http://en.wikipedia.org/wiki/Statistical_significance#In_terms_of_.CF.83_.28sigma.29
fit.plot.linear <- function(pid, name, period.in.days) {
  period <- period.in.days*3600*24 # seconds/day = 3600*24
  plot.id <- get.plot.id.con(conf$con, pid, name)
  ts.orig <- query.timeseries(conf$con, plot.id)
  ## Remove NA entries in the time series (where do they come from?
  ts.orig <- data.frame(value=ts.orig$value[!is.na(ts.orig$time)], time=ts.orig$time[!is.na(ts.orig$time)])
  ts.x <- xts(x=ts.orig$value, order.by=ts.orig$time)
  ## Restrict to given time period
  ts.x <- ts.x[paste(ts.orig$time[length(ts.orig$time)] - period, "/", sep="")]
  ## Summarize per week
  ts <- apply.weekly(ts.x, function(x) { mean(x) })
  ts <- data.frame(time=index(ts), value=coredata(ts))
  ## Check if we have any data left
  if (length(ts$time) < 2) {
    return(rel.increase.per.year=NA, sigma=NA)
  }
  ## Fit with linear model
  m1 <- lm(value ~ time, ts)
  s <- summary(m1)
  #print(paste("Fit to", name))
  #print(s)
  increase <- s$coefficients[[2]]
  increase.stderr <- s$coefficients[[4]]
  increase.sigma <- abs(increase/increase.stderr)
  ## Calculate the fit at the last timestamp (y = c + <increase>*t)
  fit.value.now <- s$coefficients[[1]] + as.integer(ts$time[length(ts$time)]) * increase
  list(rel.increase.per.year=increase * 3600 * 24 * 365 / fit.value.now,
       sigma=increase/increase.stderr,
       fit.value.now=fit.value.now
       )
}


figure.of.merit.collaboration <- function(pid) {
  n.commits <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM commit WHERE projectId=", pid))[[1]]
  n.persons <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM person WHERE projectId=", pid))[[1]]
  #n.issues <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM issue WHERE projectId=", pid))[[1]]
  msg <- list()
  status <- status.good
  if (n.commits == 0) {
    msg <- c(msg, "No commits are in the database!")
    status <- min(status, status.error)
  } else if (n.commits < 100) {
    msg <- c(msg, "There seem to be very few commits in this project.")
    status <- min(status, status.bad)
  } else if (n.commits < 1000) {
    msg <- c(msg, "The project has not had many commits so far.")
    status <- min(status, status.warn)
  } else {
    msg <- c(msg, "There is a sizeable number of commits in the project repository.")
  }
  if (n.persons == 0) {
    msg <- c(msg, "The commit analysis seems to have failed.")
    status <- min(status, status.error)
  } else if (n.persons == 1) {
    msg <- c(msg, "There seems to be only a single committer in this project.")
    status <- min(status, status.bad)
  } else if (n.persons < 12) {
    msg <- c(msg, "Less than an dozen people have contributed to this project.")
    status <- min(status, status.warn)
  } else {
    msg <- c(msg, "Some ", as.character(n.persons), " people have contributed to this project.")
  }
  list(status=status, why=do.call(paste, msg))
}

figure.of.merit.communication <- function(pid) {
  n.mail.threads <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM mail_thread WHERE projectId=", pid))[[1]]
  ml.plots <-  dbGetQuery(conf$con, str_c("SELECT id, name FROM plots WHERE projectId=", pid, " AND releaseRangeId IS NULL AND name LIKE '%activity'"))
  if (length(n.mail.threads) == 0 || length(ml.plots) == 0) {
    return(list(status=status.error, why="No mailing list to analyse."))
  }

  ## Do a linear fit over the last year
  res <- lapply(ml.plots$name, function(name) {
    fit.plot.linear(pid, name, 365)
  })
  sigma <- sapply(res, function(x) {x$sigma})
  inc <- sapply(res, function(x) {x$rel.increase.per.year})
  val <- sapply(res, function(x) {x$fit.value.now})
  ## Remove values from inc where the fit failed
  selected <- !(is.nan(inc) | is.na(inc) | is.null(inc))
  sigma <- sigma[selected]
  inc <- inc[selected]
  val <- val[selected]
  ## If no values are left, we have to abort.
  if (length(inc) == 0) {
    return(list(status=status.error, why="Mailing list activity could not be fitted. This probably indicates a problem with the analysis."))
  }
  ## Anything less than 5 sigma is not a discovery, especially not
  ## since the fit will in general be very bad.
  ## We therefore set the magnitude to zero, meaning "no detectable change"
  inc[sigma < 5] <- 0.0
  ## Select the most active mailing list
  max.index <- which.max(val)
  max.plot <- ml.plots$name[max.index]
  increase.per.year <- inc[max.index]
  if (max(val)*30 < 12) { # less than 12 mails per month is "dead"
    list(status=status.bad,
         why=str_c("The ", max.plot, " is less than a dozen mails per month. This may indicate that the project is not actively developed, that mostly private or other channels are used for communication."))
  } else if (increase.per.year < -0.50) {
    list(status=status.bad,
         why=str_c("The ", max.plot, " is falling by ~", format(increase.per.year*100, digits=1), "% per year. This may indicate that the project is being abandoned or that the means of communication are changing."))
  } else if (increase.per.year < -0.10) {
    list(status=status.warn,
         why=str_c("The ", max.plot, " is falling by ~", format(increase.per.year*100, digits=1), "% per year. This may indicate decreasing development activity or that the means of communication are changing."))
  } else if (increase.per.year < 0.10) {
    list(status=status.good,
         why=str_c("The ", max.plot, " is approximately constant."))
  } else {
    list(status=status.good,
         why=str_c("The ", max.plot, " is increasing by ~", format(increase.per.year*100, digits=1), "% per year. This indicates increasing development activity."))
  }
}

figure.of.merit.construction <- function(pid) {
  plot.ids <- dbGetQuery(conf$con, str_c("SELECT id FROM plots WHERE name LIKE 'Release TS distance' AND projectId=", pid))
  if (length(plot.ids) == 0) {
    return(list(status=status.error, why="Release information not available. This usually means that the analysis has not yet completed."))
  }
  plot.id <- plot.ids$id[[1]]
  release.distance <- query.timeseries(conf$con, plot.id)
  l <- length(release.distance$time)
  if (l == 0) {
    return(list(status=status.error, why="Release information not available. This usually means that the analysis has not yet completed."))
  } else if (l < 3) {
    return(list(status=status.bad, why="Very few releases have been made. It is not yet possible to quantify the construction."))
  }
  ## create xts time series
  ts.x <- xts(x=release.distance$value, order.by=release.distance$time)
  ## Restrict to given time period
  period <- 3600*24*365 * 5 # Look at the past five years only
  ts.x <- ts.x[paste(release.distance$time[l] - period, "/", sep="")]
  median.distance <- median(ts.x)
  if (median.distance > 0.5) {
    return(list(status=status.bad, why="The release structure seems highly irregular."))
  } else if (median.distance > 0.2) {
    return(list(status=status.warn, why="The release structure seems to be irregular."))
  } else {
    return(list(status=status.good, why="The release structure seems to be regular."))
  }
}

figure.of.merit.complexity <- function(pid) {
  ## First, check if any complexity analysis plots exist
  understand.plots <- dbGetQuery(conf$con, str_c("SELECT id, name FROM plots WHERE name LIKE 'Understand%' AND projectId=", pid))
  if (length(understand.plots) == 0) {
    return(list(status=status.error, why="No complexity analysis plots were found.")) # Cannot return figure of merit if no complexity analysis was done
  }

  ## Do a linear fit over the last year
  res <- lapply(understand.plots$name, function(name) {
    fit.plot.linear(pid, name, 365)
  })
  sigma <- sapply(res, function(x) {x$sigma})
  inc <- sapply(res, function(x) {x$rel.increase.per.year})
  ## Remove values from inc where the fit failed
  sigma <- sigma[!(is.nan(inc) | is.na(inc))]
  inc <- inc[!(is.nan(inc) | is.na(inc))]
  ## If no values are left, we have to abort.
  if (length(inc) == 0) {
    return(list(status=status.error, why="Complexity analysis plots could not be ",
                "fitted. Probably the complexity analysis failed."))
  }
  ## Anything less than 5 sigma is not a discovery, especially not
  ## since the fit will in general be very bad.
  ## We therefore set the magnitude to zero, meaning "no detectable change"
  inc[sigma < 5] <- 0.0
  max.val <- max(inc)
  if (max.val < -0.05) {
    max.plot <- understand.plots$name[which.max(inc)]
    list(status=status.good,
         why=str_c("The Complexity metric ", max.plot, " is falling by ~", format(max.val*100, digits=1), "% per year. This seems to be a good sign for maintainability."))
  } else if (max.val < 0.05) {
    list(status=status.good,
         why=str_c("The maximal complexity metrics have not changed much in the last year."))
  } else if (max.val < 0.40) {
    max.plot <- understand.plots$name[which.max(inc)]
    list(status=status.warn,
         why=str_c("The Complexity metric ", max.plot, " is increasing by ~", format(max.val*100, digits=1), "% per year. This may make it harder to maintain the project."))
  } else {
    max.plot <- understand.plots$name[which.max(inc)]
    list(status=status.bad,
         why=str_c("The Complexity metric ", max.plot, " is increasing by ~", format(max.val*100, digits=1), "% per year. This is quite a lot and may make maintenance hard."))
  }
}
