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


fit.plot.linear <- function(pid, name, period.in.days) {
  period <- period.in.days*3600*24
  plot.id <- get.plot.id.con(conf$con, pid, name)
  ts.orig <- query.timeseries(conf$con, plot.id)
  ts.x <- xts(x=ts.orig$value, order.by=ts.orig$time)
  ## Restrict to given time period
  ts.x <- ts.x[paste(ts.orig$time[length(ts.orig$time)] - period, "/", sep="")]
  ## Summarize per week
  ts <- apply.weekly(ts.x, function(x) { mean(x) })
  ts <- data.frame(time=index(ts), value=coredata(ts))
  ## Fit with linear model
  m1 <- lm(value ~ time, ts)
  s <- summary(m1)
  print(s)
  mean.last.period <- mean(ts$value)
  increase <- s$coefficients[[2]]
  increase.stderr <- s$coefficients[[4]]
  increase.sigma <- abs(increase/increase.stderr)
  list(rel.increase.per.year=increase * 3600 * 24 * 365 / mean.last.period,
       sigma=increase/increase.stderr,
       increase=increase,
       increase.stderr=increase.stderr)
}



figure.of.merit.collaboration.warn <- 0.75
figure.of.merit.collaboration.bad <- 0.5
figure.of.merit.collaboration <- function(pid) {
  n.commits <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM commit WHERE projectId=", pid))[[1]]
  n.persons <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM person WHERE projectId=", pid))[[1]]
  #n.issues <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM issue WHERE projectId=", pid))[[1]]
  min(n.commits/1000., n.persons/20.)
}

figure.of.merit.communication.warn <- 500
figure.of.merit.communication.bad <- 100
figure.of.merit.communication <- function(pid) {
  n.mail.threads <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM mail_thread WHERE projectId=", pid))[[1]]
}

figure.of.merit.construction.warn <- 1
figure.of.merit.construction.bad <- 0
figure.of.merit.construction <- function(pid) {
  n.tsplots <- dbGetQuery(conf$con, str_c("SELECT COUNT(*) FROM plots WHERE name LIKE 'Progress%' AND projectId=", pid))[[1]]
  n.tsplots
}

figure.of.merit.complexity.warn <- 1.0
figure.of.merit.complexity.bad <- 0
figure.of.merit.complexity <- function(pid) {
  understand.plots <- dbGetQuery(conf$con, str_c("SELECT id, name FROM plots WHERE name LIKE 'Understand%' AND projectId=", pid))
  if (length(understand.plots) == 0) {
    return(NA) # Cannot return figure of merit if no complexity analysis was done
  }
  res <- lapply(understand.plots$name, function(name) {
    fit.plot.linear(pid, name, 365)
  })
  sigma <- sapply(res, function(x) {x$sigma})
  inc <- sapply(res, function(x) {x$rel.increase.per.year})
  sigma[is.nan(sigma) | is.na(sigma)] <- 0.0
  print(inc)
  print(sigma)
  inc[sigma < 10] <- 0.0 # if sigma < 10 we cannot reliably say anything
  max.val <- max(inc)

  if (max.val < -0.05) {
    max.plot <- understand.plots$name[which.max(inc)]
    list(status="good",
         why=str_c("The Complexity metric ", max.plot, " is falling by ~", format(max.val*100, digits=1), "% per year. This seems to be a good sign for maintainability."))
  } else if (max.val < 0.05) {
    list(status="good",
         why=str_c("The maximal complexity metrics have not changed much in the last year."))
  } else if (max.val < 0.40) {
    max.plot <- understand.plots$name[which.max(inc)]
    list(status="warn",
         why=str_c("The Complexity metric ", max.plot, " is increasing by ~", format(max.val*100, digits=1), "% per year. This may make it harder to maintain the project."))
  } else {
    max.plot <- understand.plots$name[which.max(inc)]
    list(status="bad",
         why=str_c("The Complexity metric ", max.plot, " is increasing by ~", format(max.val*100, digits=1), "% per year. This is quite a lot and may make maintenance hard."))
  }
}
