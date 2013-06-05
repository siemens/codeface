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
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(yaml))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(gridExtra))
source("utils.r")
source("config.r")
source("plot.r")
source("db.r")
source("query.r")
source("ts_utils.r")

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

compute.next.timestamp <- function(time, last.time) {
  ## Computing the time stamp is done using this seemingly bizarre way because
  ## we need to have strictly monotonic timestamps on the one hand, but want
  ## to have the distance between commits proportional to their real temporal
  ## committance distance. So we cannot just use the timestamp difference
  ## between this and the last event, but add one.

  ## Correct for identical dates
  if (time == last.time)
    time <- time + seconds(1)

  ## Correct for negative time differences (can arise from multiple
  ## consecutive identical time stamps)
  if (time < last.time)
    time <- last.time + seconds(1)

  return(time)
}

## Take a list of commits and make their date indices unique by
## adding a one second offset to identical ones.
make.index.unique <- function(dat, subset) {
  dat$commitDate <- ymd_hms(dat$commitDate, quiet=T)
  last.timestamp <- min(dat$commitDate) - seconds(1)

  for (i in 1:length(dat$commitDate)) {
    dat$commitDate[[i]] <- compute.next.timestamp(dat$commitDate[[i]],
                                                  last.timestamp)
    last.timestamp <- dat$commitDate[[i]]
  }

  return(dat)
}


## Compute the total progress time series
gen.full.ts <- function(conf) {
  boundaries <- conf$boundaries
  full.series <- vector("list", dim(boundaries)[1])

  tstamps <- conf$tstamps.release

  subset <- c("commitDate", "AddedLines", "DeletedLines")
  ts <- get.commits.by.ranges(conf, subset, make.index.unique)

  if (dim(boundaries)[1] != length(ts)) {
    stop("Internal error: Release boundaries don't match ts list length")
  }
  
  for (i in 1:length(ts)) {
    ts[[i]]$ChangedLines <- ts[[i]]$AddedLines + ts[[i]]$DeletedLines
    full.series[[i]] <- na.omit(xts(ts[[i]]$ChangedLines,
                                    order.by=ts[[i]]$commitDate))
    full.series[[i]] <- trim.series(full.series[[i]], boundaries$date.start[i],
                                    boundaries$date.end[i])
  }

  full.series <- do.call(c, full.series)
  
  return (full.series)
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

gen.cluster.file.list <- function(resdir, revisions, type) {
  file.list <- vector("list", length(revisions)-1)

  revs <- gen.rev.list(revisions)
  for (i in 1:length(revs)) {
    file.list[[i]] <- paste(resdir, "/", revs[[i]], "/", type,
                            "_cluster_stats.txt", sep="")
  }

  return(file.list)
}

## NOTE: width is the width of the rolling window, so series.monthly
## does _not_ mean that there is one data point per month, but one
## month's worth of data points go into the calculation of one smoothed
## data point. Using the robust median instead of mean considerably
## reduces the amount of outliers
process.ts <- function(series) {
  duration <- end(series) - start(series)

  ## We compute the window lengths based on natural time units
  ## to avoid dependencies on the lifetime of the project, or on the
  ## project's relative activity
  ## Assume that a month has roughly 30 days.
  width.monthly <- length(series)/as.numeric(duration/7)
  width.daily <- length(series)/as.numeric(duration)
  series.daily <- na.omit(rollapply(series, width=width.daily, median,
                                    align="center"))
  series.monthly <- na.omit(rollapply(series, width=width.monthly, median,
                                      align="center"))
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
  MIN.COMMITS <- 10
  if (dim(dat)[1] < MIN.COMMITS) {
    cat("NOTE: Revision", revision, " contains less than",
        MIN.COMMITS, "commits. Skipping.\n")
  } else {
    plot.list <- plot.splom(plot.types, dat)
    pdf(file.path(graphdir, paste("commits_", revision, ".pdf", sep="")))
    do.call(grid.arrange, c(plot.list, list(nrow=length(plot.types),
                                            ncol=length(plot.types))))
    dev.off()
  }
}


## Perform statistical analysis on the clusters. type can be "sg"
## (spin glass), or "wg" (walktrap -- random walk analysis)
do.cluster.analysis <- function(resdir, graphdir, conf, type="sg") {
  if (type != "sg" && type != "wg") {
    stop("Internal error: Specify 'wg' or 'sg' for clustering type!")
  }

  cluster.file.list <- gen.cluster.file.list(resdir, conf$revisions, type)

  clusters <- vector("list", length(conf$revisions)-1)
  clusters.summary <- vector("list", length(conf$revisions)-1)

  ## Stage 1: Perform per-release operations
  status("Preparing per-release cluster plots")
  for (i in 1:length(cluster.file.list)) {
    ## Take into account that small cycles do not necessarily have clusters
    if (!can.read.file(cluster.file.list[[i]])) {
      next
    }
    clusters[[i]] <- read.table(cluster.file.list[[i]], header=TRUE, sep="\t")
    ## Assign the date of date of the release in the cluster range
    ## as cluster date (e.g., cluster v1..v2 gets the date of v2 assigned)
    clusters[[i]] <- cbind(clusters[[i]], date=conf$boundaries$date.end[i])
    clusters[[i]]$group <- as.factor(clusters[[i]]$group)

    ## Compute summary statistics for each cluster. The ddply query
    ## determines median and sum values of interesting covariates
    ## (e.g., number of commits, number of code changes) for all cluster members
    clusters.summary[[i]] <- ddply(clusters[[i]], .(group, num.members, date),
                                   summarise,
                                   prank.median=median(prank),
                                   num.changes.median=median(total),
                                   sum.changes=sum(total),
                                   num.commits.median=median(numcommits),
                                   sum.commits=sum(numcommits))
    ## Scale the summary statistics for average statements per member
    # (pp = "per person")
    clusters.summary[[i]]$sum.commits.pp <-
      clusters.summary[[i]]$sum.commits/clusters.summary[[i]]$num.members
    clusters.summary[[i]]$sum.changes.pp <-
      clusters.summary[[i]]$sum.changes/clusters.summary[[i]]$num.members

    ## Create release-specific plots
    g <- ggplot(clusters[[i]], aes(x=group, y=prank)) +
      geom_boxplot(position="dodge") + scale_y_log10() + xlab("Cluster No.") +
        ylab("Page rank")
    ggsave(file.path(graphdir, paste("cluster_prank_", conf$boundaries$tag[i],
                                     ".pdf", sep="")),
           g, width=7, height=7)

    g <- ggplot(clusters[[i]], aes(x=group, y=total)) +
      geom_boxplot(position="dodge") + scale_y_log10() + xlab("Cluster No.") +
        ylab("Amount of code changes (add+del)")
    ggsave(file.path(graphdir, paste("cluster_code_changes_",
                                     conf$boundaries$tag[i], ".pdf", sep="")),
           g, width=7, height=7)
  }


  ## Stage 2: Perform global operations on all releases
  ## TODO: Augment the date labels with release specifications; additionally,
  ## sort the clusters by average page rank per release
  status("Preparing global cluster plots")
  clusters.all <- do.call(rbind, clusters)
  clusters.summary.all <- do.call(rbind, clusters.summary)
  clusters.all$date.factor <- as.factor(clusters.all$date)
  clusters.summary.all$date.factor <- as.factor(clusters.summary.all$date)

  g <- ggplot(clusters.all, aes(x=group, y=prank)) +
    geom_boxplot(position="dodge") + scale_y_log10() + xlab("Cluster No.") +
      ylab("Page rank") + facet_wrap(~date.factor)
  ggsave(file.path(graphdir, "cluster_prank_ts.pdf"), g, width=12, height=8)

  clusters.molten <- melt(clusters.all, id.vars=c("group", "date"),
                          measure.vars=c("prank", "total", "numcommits"))
  clusters.molten$date <- as.factor(clusters.molten$date)

  g <- ggplot(clusters.molten, aes(x=group, y=value)) +
    geom_boxplot(position="dodge") + scale_y_log10() + xlab("Cluster No.") +
      ylab("Magnitude of covariate") + facet_grid(variable~date, scales="free_y")
  ggsave(file.path(graphdir, "cluster_comparison_ts.pdf"), g,
         width=12, height=8)

  ## Plots from scalars that range over all releases
  ## That one, actually, is pretty pointless because the groups in
  ## adjacent releases are not related to the previous groups by
  ## just sharing the identifier. Continued groups should be
  ## detected by similarity measures
#  g <- ggplot(clusters.summary.all, aes(x=date, y=num.members)) +
#    geom_point(aes(colour=group)) + geom_line(aes(colour=group))
#  print(g)
#  blub

  ## TODO: Produce the following plots for each release.
  ##  ggplot(dat, aes(x=group, y=added)) + geom_boxplot() + scale_y_log10()
  ##  ggplot(dat, aes(x=group, y=members)) + geom_histogram(scale="identity") + scale_y_log10()

  ## TODO: Create a time series that shows how the collective scalar
  ## properties of all clusters propagate

  ## TODO: It's most important to match clusters to see how stable the
  ## communities are across time. For this, we will need the full time series
  ## information, though. This allows us to generate more detailed time
  ## series than is possible with simple all-cluster scalar properties,
  ## for instance to detect variations within individual stable groups.
}

do.commit.analysis <- function(resdir, graphdir, conf) {
  ## Stage 1: Prepare summary statistics for each release cycle,
  ## and prepare the time series en passant
  tstamps <- conf$tstamps.release

  subset <- c("CmtMsgBytes", "ChangedFiles", "DiffSize", "NumTags", "inRC")
  ts <- get.commits.by.ranges(conf, subset, normalise.commit.dat)

  for (i in 1:(length(ts))) {
    status(paste("Plotting commit information for revision",
                 conf$revisions[[i+1]]))
    plot.types <- c("CmtMsgBytes", "ChangedFiles", "DiffSize")
    if (sum(ts[[i]]$NumSignedOffs) > 0) {
      ## The data do contain tagging information
      plot.types <- c(plot.types, "NumTags")
    }

    plot.commit.info(ts[[i]], plot.types, graphdir, tstamps$tag[[i+1]])
  }

  status("Plotting the commit information time series")
  ## Stage 2: Plot the complete commit information time series
  ts <- do.call(rbind, ts)

  ts.molten <- melt(ts[c("revision", "date", subset)],
                    id=c("revision", "inRC", "date"))
  levels(ts.molten$inRC) <- c("No", "Yes")

  ## TODO: Does not work when date is used instead of revision,
  ## which is the desirable alternative
  g <- ggplot(data=ts.molten, aes(x=revision, y=value, colour=inRC)) +
    geom_boxplot(fill="NA") + scale_y_log10() +
    facet_wrap(~variable, scales="free") + xlab("Revision") +
      ylab("Value (log. scale)") +
      scale_colour_discrete("Release\nCandidate")
  ggsave(file.path(graphdir, "ts_commits.pdf"), g, width=12, height=8)

  ## Export the SVG representation to the data base
  ggsave(file.path(graphdir, "ts_commits.svg"), g, width=12, height=8)
  dat.svg <- readLines(file.path(graphdir, "ts_commits.svg"))
  ## TODO: Do the actual DB export
  file.remove(file.path(graphdir, "ts_commits.svg"))


  ## Stage 3: Plot annual versions of the commit time series
  min.year <- year(min(ts.molten$date))
  max.year <- year(max(ts.molten$date))

  dummy <- sapply(seq(min.year, max.year), function(year) {
    if (dim(ts.molten[year(ts.molten$date)==year,])[1] == 0) {
      status(paste("Skipping annual commit time series for", year, "(no release)"))
      return(NA)
    }
    status(paste("Creating annual commit time series for", year))
    g <- ggplot(data=ts.molten[year(ts.molten$date)==year,],
                aes(x=revision, y=value, colour=inRC)) +
                  geom_boxplot(fill="NA") + scale_y_log10() +
                  facet_wrap(~variable, scales="free") + xlab("Revision") +
                  ylab("Value (log. scale)") +
                  scale_colour_discrete("Release\nCandidate") +
                  ggtitle(paste("Commit time series for year", year))
    ggsave(file.path(graphdir, paste("ts_commits_", year, ".pdf", sep="")),
           g, width=12, height=8)
    })
}

do.ts.analysis <- function(resdir, graphdir, conf) {
  ## Prepare the raw time series as input to the smoothing
  ## algorithms
  full.ts <- gen.full.ts(conf)
  series.merged <- process.ts(full.ts)
  
  ## Prepare y ranges for the different graph types
  ## Compute min/max value per type, and prepare a special
  ## version of boundaries used for plotting which includes
  ## the boundaries
  ranges <- ddply(series.merged, .(type), summarise,
                  ymin=min(value), ymax=max(value))
  
  num.types <- length(unique(ranges$type))
  res <- vector("list", num.types)
  for (i in 1:num.types) {
    res[[i]] <- cbind(conf$boundaries, ranges[i,])
  }
  boundaries.plot <- do.call(rbind, res)

  ## Visualisation
  ## TODO: log and sqrt transform are reasonable for the averaged, but not
  ## for the cumulative series
  g <- ggplot(series.merged, aes(x=time, y=value)) + geom_line() +
    facet_grid(type~., scale="free_y") +
    geom_vline(aes(xintercept=as.numeric(date.end), colour="red"),
               data=boundaries.plot) +
    scale_fill_manual(values = alpha(c("blue", "red"), .1)) +
    xlab("Time") + ylab("Amount of changes") +
    ggtitle(paste("Code changes for project '", conf$description, "'", sep=""))

  ## na.omit is required to remove all cycles that don't contain
  ## rc regions.
  if (dim(na.omit(boundaries.plot))[1] > 0) {
    ## Only plot release candidate regions if there are any, actually
    g <- g + geom_rect(aes(NULL, NULL, xmin=date.rc_start,
                           xmax=date.end, ymin=ymin, ymax=ymax, fill="blue"),
                       data=na.omit(boundaries.plot))

  }

  ggsave(file.path(graphdir, "ts.pdf"), g, width=16, height=7)

  ## Store the complete time series information into the database
  status("Storing time series data into database")
  for (type in unique(series.merged$type)) {
    plot.name <- str_c("Progress TS [", type, "]")
    plot.id <- get.plot.id(conf, plot.name)

    series.sub <- series.merged[series.merged$type==type,]

    dat <- data.frame(time=as.character(series.sub$time),
                      value=series.sub$value,
                      value_scaled=series.sub$value.scaled,
                      plotId=plot.id)
    res <- dbWriteTable(conf$con, "timeseries", dat, append=T, row.names=F)
    if (!res) {
      stop("Internal error: Could not write timeseries into database!")
    }
  }

  ## Create annual versions of the plots
  min.year <- year(min(series.merged$time))
  max.year <- year(max(series.merged$time))

  dummy <- sapply(seq(min.year, max.year), function(year) {
    status(paste("Creating annual time series for", year))
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

## Perform analyses that concern the per-release structure of projects
do.release.analysis <- function(resdir, graphdir, conf) {
  series.merged <- query.series.merged(conf)

  ## Check how similar the release cycles are to each other. Independent
  ## of the actual shape of the cycle, having similar cycles shows that
  ## the project follows a certain well-defined process
  plot.name <- "Release TS distance"
  plot.id <- get.plot.id(conf, plot.name)

  dat <- compute.release.distance(series.merged, conf)
  dat <- data.frame(time=as.character(conf$boundaries$date.end[-1]), value=res,
                    value_scaled=res, plotId=plot.id)

  res <- dbWriteTable(conf$con, "timeseries", dat, append=T, row.names=F)
  if (!res) {
    stop("Internal error: Could not write release distance TS into database!")
  }

  ## TODO: Compute the difference between release cycles and a
  ## per-determined, desirable shape of the release curve.
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
global.conf <- load.global.config("prosoda.conf")
resdir <- file.path(resdir, conf$project, conf$tagging)
graphdir <- file.path(resdir, "graphs")
dir.create(graphdir, showWarnings=FALSE, recursive=TRUE)

conf <- init.db(conf, global.conf)

## TODO: Turn this into a proper pipeline, or some plugin-based
## analysis mechanism?
if (!interactive()) {
  options(error = quote(dump.frames("error.dump", TRUE)))
} else {
  options(error=recover)
}

do.ts.analysis(resdir, graphdir, conf)
do.commit.analysis(resdir, graphdir, conf)
do.cluster.analysis(resdir, graphdir, conf)
do.release.analysis(resdir, graphdir, conf)
