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

## Time series analysis based on the output of ts.py
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(yaml))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(gridExtra))
source("utils.r")
source("plot.r")
source("config.r")

## Omit time series elements that exceed the given range
trim.series <- function(series, start, end) {
  series <- series[which(index(series) < end),]
  series <- series[which(index(series) > start),]
  
  return(series)
}

## Unidirectional version of the above function
trim.series.start <- function(series, start) {
  return(series[which(index(series) > start),])
}

get.series.boundaries <- function(conn) {
  ## Read the first (comment) line of the time series connection,
  ## and split it into start/end date timestamp, and possibly also
  ## the release candidate start time stamp (the first entry is
  ## the hash mark of the comment line)
  elems = str_split(readLines(conn, n=1), "\t")[[1]][-1:0]

  boundaries <- data.frame(date.start = tstamp_to_date(elems[1]),
                           date.end = tstamp_to_date(elems[2]))

  if (length(elems) == 3) {
    boundaries$date.rc_start= tstamp_to_date(elems[3])
  } else {
    boundaries$date.rc_start = NA
  }

  return (boundaries)
}

boundaries.include.rc <- function(boundaries) {
  return (!is.na(boundaries$rc_start))
}

## Given a list of time series file names, compute the total time series
gen.full.ts <- function(ts.file.list) {
  full.series <- vector("list", length(ts.file.list))
  releases <- vector("list", length(ts.file.list))
  
  for (i in 1:length(ts.file.list)) {
    conn <- file(ts.file.list[[i]], "r")
    boundaries <- get.series.boundaries(conn)
    
    full.series[[i]] <- read.zoo(conn, FUN=tstamp_to_date)[,1]    
    full.series[[i]] <- trim.series(full.series[[i]], boundaries$date.start,
                                    boundaries$date.end)

    releases[[i]] <- data.frame(date.release=boundaries$date.end,
                                date.rc_start=boundaries$date.rc_start)
    close(conn)
  }

  full.series <- do.call(c, full.series)
  releases <- data.frame(do.call(rbind, releases))
  
  return (list(series=full.series, releases=releases))
}

## Convert a time series into a data frame
gen.df.from.ts <- function(ts, type) {
  df <- data.frame(time=index(ts), value=coredata(ts),
                   value.scaled=scale.data(coredata(ts)), type=type)

  return(df)
}

gen.rev.list <- function(revisions) {
 rev.list <- vector("list", length(revisions)-1)

 for (i in 2:length(revisions)) {
    rev.list[[i-1]] <- paste(revisions[[i-1]], "-", revisions[[i]], sep="")
  }

 return (rev.list)
}

gen.ts.file.list <- function(resdir, revisions) {
  ts.file.list <- vector("list", length(revisions)-1)

  revs <- gen.rev.list(revisions)
  for (i in 1:length(revs)) {
    ts.file.list[[i]] <- paste(resdir, "/ts/raw_", revs[[i]], ".dat", sep="")
  }

  return(ts.file.list)
}

gen.commit.file.list <- function(resdir, revisions) {
  ts.file.list <- vector("list", length(revisions)-1)

  revs <- gen.rev.list(revisions)
  for (i in 1:length(revs)) {
    ts.file.list[[i]] <- paste(resdir, "/", revs[[i]], "/commits.txt", sep="")
  }

  return(ts.file.list)
}

## NOTE: width is the width of the rolling window, so series.monthly
## does _not_ mean that there is one data point per month, but one
## month's worth of data points go into the calculation of one smoothed
## data point. Using the robust median instead of mean considerably
## reduces the amount of outliers
gen.series.df <- function(series) {
  duration <- end(series) - start(series)

  ## We compute the window lengths based on natural time units
  ## to avoid dependencies on the lifetime of the project, or on the
  ## project's relative activity
  ## Assume that a month has roughly 30 days.
  width.monthly <- length(series)/as.numeric(duration/7)
  width.daily <- length(series)/as.numeric(duration)
  series.daily <- rollapply(series, width=width.daily, median, align="center")
  series.monthly <- rollapply(series, width=width.monthly, median, align="center")
  series.cumulative <- cumsum(series)

  ## NOTE: R can only merge two data frames at once
  series.merged <- merge(gen.df.from.ts(series.daily,
                                        type="Averaged (small window)"),
                         gen.df.from.ts(series.monthly,
                                        type="Averaged (large window)"), all=T)
  series.merged <- merge(series.merged,
                         gen.df.from.ts(series.cumulative,
                                        type="Cumulative"), all=T)

  return(series.merged)
}

plot.commit.info <- function(dat, plot.types, graphdir, revision) {
<<<<<<< HEAD
  MIN.COMMITS <- 10
  if (dim(dat)[1] < MIN.COMMITS) {
    cat("NOTE: Revision", revision, " contains less than",
        MIN.COMMITS, "commits. Skipping.\n"))
  } else {
    plot.list <- plot.splom(plot.types, dat)
    pdf(paste(graphdir, paste("commits_", revision, ".pdf", sep=""), sep="/"))
    do.call(grid.arrange, c(plot.list, list(nrow=length(plot.types),
                                            ncol=length(plot.types))))
    dev.off()
  }
=======
  pdf(paste(graphdir, paste("commits_", revision, ".pdf", sep=""), sep="/"))
  do.call(grid.arrange, plot.splom(plot.types, dat))
  dev.off()
>>>>>>> a8ebfa86a05ce0b00ae656e03f99e4a7f85c688e
}

do.commit.analysis <- function(resdir, graphdir, conf) {
  commit.file.list <- gen.commit.file.list(resdir, conf$revisions)

  ## Stage 1: Prepare summary statistics for each release cycle,
  ## and prepare the time series en passant
  ts <- vector("list", length(conf$revisions)-1)
  tstamps <- read.table(paste(resdir, "/ts/timestamps.txt", sep=""),
                              header=T, sep="\t")
  tstamps <- tstamps[tstamps$type=="release",]
  subset <- c("CmtMsgBytes", "ChangedFiles", "DiffSize", "NumTags", "inRC")

  for (i in 1:length(commit.file.list)) {
    dat <- read.table(commit.file.list[[i]], header=TRUE, sep="\t")
    dat <- normalise.commit.dat(dat, subset)

<<<<<<< HEAD
    status(paste("Plotting commit information for revisison",
                 conf$revisions[[i+1]]))
=======
>>>>>>> a8ebfa86a05ce0b00ae656e03f99e4a7f85c688e
    plot.types <- c("CmtMsgBytes", "ChangedFiles", "DiffSize")
    if (sum(dat$NumSignedOffs) > 0) {
      ## The data does contain tagging information
      plot.types <- c(plot.types, "NumTags")
    }

    ts[[i]] <- cbind(data.frame(revision=tstamps$tag[[i+1]],
                                date=tstamps$date[[i+1]]), dat)

    plot.commit.info(dat, plot.types, graphdir, tstamps$tag[[i+1]])
  }

<<<<<<< HEAD
  status("Plotting the commit information time series")
=======
>>>>>>> a8ebfa86a05ce0b00ae656e03f99e4a7f85c688e
  ## Stage 2: Plot the complete commit information time series
  ts <- do.call(rbind, ts)
  ts$date <- tstamp_to_date(ts$date)

  ts.molten <- melt(ts[c("revision", "date", subset)],
                    id=c("revision", "inRC", "date"))
<<<<<<< HEAD
  levels(ts.molten$inRC) <- c("No", "Yes")
=======
>>>>>>> a8ebfa86a05ce0b00ae656e03f99e4a7f85c688e

  ## TODO: Does not work when date is used instead of revision,
  ## which is the desirable alternative
  g <- ggplot(data=ts.molten, aes(x=revision, y=value, colour=inRC)) +
    geom_boxplot(fill="NA") + scale_y_log10() +
    facet_wrap(~variable, scales="free") + xlab("Revision") +
      ylab("Value (log. scale)") +
<<<<<<< HEAD
      scale_colour_discrete("Release\nCandidate")
=======
      scale_colour_discrete("Release\nCandidate", values=c("black", "red"))
>>>>>>> a8ebfa86a05ce0b00ae656e03f99e4a7f85c688e
  ggsave(paste(graphdir, "ts_commits.pdf", sep="/"), g, width=12, height=8)
}

do.ts.analysis <- function(resdir, graphdir, conf) {
  status("Creating time series plots")
  ts.file.list <- gen.ts.file.list(resdir, conf$revisions)
  
  ## Dispatch the calculations and create result data frames
  full.ts <- gen.full.ts(ts.file.list)
  series.merged <- gen.series.df(full.ts$series)
  
  ## Prepare y ranges for the different graph types
  ## Compute min/max value per type
  ranges <- ddply(series.merged, .(type), summarise,
                  ymin=min(value), ymax=max(value))
  
  num.types <- length(ranges)
  res <- vector("list", num.types)
  for (i in 1:num.types) {
    res[[i]] <- cbind(full.ts$releases, ranges[i,])
  }
  full.ts$releases <- do.call(rbind, res)

  ## Release cycles without release candidates must be removed
  ## (otherwise, we run into plotting problems)
  dat.rc <- na.omit(full.ts$releases)
  dat.rc$date.rc_start <- tstamp_to_date(dat.rc$date.rc_start)

  ## Visualisation
  ## TODO: log and sqrt transform are reasonable for the averaged, but not
  ## for the cumulative series
  g <- ggplot(series.merged, aes(x=time, y=value)) + geom_line() +
    facet_grid(type~., scale="free_y") +
    geom_vline(aes(xintercept=as.numeric(date.release), colour="red"),
               data=full.ts$releases) +
    scale_fill_manual(values = alpha(c("blue", "red"), .1)) +
    xlab("Time") + ylab("Amount of changes") +
    ggtitle(paste("Code changes for project '", conf$description, "'", sep=""))

  if (dim(dat.rc)[1] > 0) {
    ## Only plot release candidate regions if there are any, actually
    g <- g + geom_rect(aes(NULL, NULL, xmin=date.rc_start,
                           xmax=date.release, ymin=ymin, ymax=ymax, fill="blue"),
                       data=dat.rc)

  }

  ggsave(paste(graphdir, "ts.pdf", sep="/"), g, width=16, height=7)


  ## Create annual versions of the plots
  min.year <- year(min(series.merged$time))
  max.year <- year(max(series.merged$time))

  dummy <- sapply(seq(min.year, max.year), function(year) {
    status(paste("Creating yearly time series for", year))
    g.year <- g + xlim(dmy(paste("1-1-", year, sep=""), quiet=T),
                       dmy(paste("31-12-", year, sep=""), quiet=T)) +
              ggtitle(paste("Code changes in ", year, " for project '",
                            conf$description, "'", sep=""))

    ggsave(paste(graphdir, "/ts_", year, ".pdf", sep=""), g.year,
           width=16, height=7)
  })
  ## All-in-one graph with scaled axes (ugly)
#  g <- ggplot(series.merged, aes(x=time, y=value.scaled, colour=type)) +
#    geom_line() + geom_vline(aes(xintercept=as.numeric(rel.date)),
#                             data=full.ts$releases)
#  print(g)
}

######################### Dispatcher ###################################
parser <- OptionParser(usage = "%prog resdir config")
arguments <- parse_args(parser, positional_arguments = TRUE)

if (length(arguments$args) != 2) {
  cat("Please specify result directory and configuration file\n")
  print_help(parser)
  stop()
} else {
  resdir <- arguments$args[1]
  config.file <- arguments$args[2]
}

conf <- load.config(config.file)
resdir <- paste(resdir, conf$project, conf$tagging, sep="/")
graphdir <- paste(resdir, "graphs", sep="/")
dir.create(graphdir, showWarnings=FALSE, recursive=TRUE)

options(error = quote(dump.frames("error.dump", TRUE)))
do.ts.analysis(resdir, graphdir, conf)
do.commit.analysis(resdir, graphdir, conf)
