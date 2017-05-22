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
suppressPackageStartupMessages(library(logging))

source("utils.r")
source("config.r")
source("plot.r")
source("query.r")
source("ts_utils.r")
source("clusters.r")
source("commits.r")
source("timezones.r")
source("mc_helpers.r")

get.series.boundaries <- function(conn) {
  ## Read the first (comment) line of the time series connection,
  ## and split it into start/end date timestamp, and possibly also
  ## the release candidate start time stamp (the first entry is
  ## the hash mark of the comment line)
  elems = str_split(readLines(conn, n=1), "\t")[[1]][-1:0]

  boundaries <- data.frame(date.start = tstamp.to.POSIXct(elems[1]),
                           date.end = tstamp.to.POSIXct(elems[2]))

  if (length(elems) == 3) {
    boundaries$date.rc.start= tstamp.to.POSIXct(elems[3])
  } else {
    boundaries$date.rc.start = NA
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
    time <- time + dseconds(1)

  ## Correct for negative time differences (can arise from multiple
  ## consecutive identical time stamps)
  if (time < last.time)
    time <- last.time + dseconds(1)

  return(time)
}

## Take a list of commits and make their date indices unique by
## adding a one second offset to identical ones.
make.index.unique <- function(dat, subset) {
  dat$commitDate <- ymd_hms(dat$commitDate, quiet=TRUE)
  last.timestamp <- min(dat$commitDate) - dseconds(1)

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

  full.series <- full.series[sapply(full.series, length)!=0]
  full.series <- do.call(c, full.series)

  return (full.series)
}

gen.rev.list <- function(revisions) {
 rev.list <- vector("list", length(revisions)-1)

 for (i in 2:length(revisions)) {
    rev.list[[i-1]] <- paste(revisions[[i-1]], "-", revisions[[i]], sep="")
  }

 return (rev.list)
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
  series.weekly <- na.omit(apply.weekly(series, median))
  series.monthly <- na.omit(apply.monthly(series, median))
  series.cumulative <- cumsum(series)

  ## NOTE: R can only merge two data frames at once
  series.merged <- merge(gen.df.from.ts(series.weekly,
                                        type="Averaged (small window)"),
                         gen.df.from.ts(series.monthly,
                                        type="Averaged (large window)"), all=TRUE)
  series.merged <- merge(series.merged,
                         gen.df.from.ts(series.cumulative,
                                        type="Cumulative"), all=TRUE)

  return(series.merged)
}


## Given a release range, compute a summary statistics for _all_ clusters
## in the release for a given clustering method and page rank technique
compute.release.clusters.stats <- function(conf, range.id,
                                           cluster.method=cluster.methods[1],
                                           technique=0) {
  if (!cluster.method.valid(cluster.method)) {
    stop("Internal error: Specify a supported clustering type!")
  }

  cluster.ids <- query.cluster.ids(conf, range.id, cluster.method)

  ## NOTE: cluster.ids can be NULL when no clusters have been detected.
  clusters.stats <- lapply(cluster.ids, function(cluster.id) {
    ## We query the statistics resolved per-cluster, per-person,
    ## and then average over all persons
    cluster.stats <- query.cluster.person.stats(conf$con, cluster.id,
                                                person.id=NULL)
    num.members <- dim(cluster.stats)[1]
    cluster.stats$num.members <- num.members

    return(cluster.stats)
  })

  return(do.call(rbind, clusters.stats))
}

## Given statistics for all clusters of a cycle (i.e., the output of
## compute.release.clusters.stats), compute a summarised version
## that reduces each cluster to one single contributing row
summarise.clusters.stats <- function(clusters.stats) {
  ## Computing summaries for empty clusters is easy
  if (is.null(clusters.stats)) {
    return(NULL)
  }

  res <- ddply(clusters.stats, .(cluster.id, num.members), summarise,
               prank.median=median(rankValue),
               num.changes.median=median(total),
               sum.changes=sum(total),
               num.commits.median=median(numcommits),
               sum.commits=sum(numcommits))

  ## Scale the summary statistics for average statements per member
  ## (pp = "per person")
  res$sum.commits.pp <- res$sum.commits/res$num.members
  res$sum.changes.pp <- res$sum.changes/res$num.members

  return(res)
}


## Perform statistical analyses on the clusters.
## This pass is about computing descriptive cluster statistics.
## NOTE: The database operations are performed on views; the first
## time selection currently takes a fairly long time. The side effect
## of later speedups is, however, important because on-demand
## queries in the web frontend will then proceed much faster.
do.cluster.analysis <- function(resdir, graphdir, conf,
                                cluster.method=cluster.methods[1]) {
  if (!cluster.method.valid(cluster.method)) {
    stop("Internal error: Specify a supported clustering method!")
  }

  cycles <- get.cycles(conf)
  clusters.all <- vector("list", dim(cycles)[1])
  clusters.summary <- vector("list", dim(cycles)[1])

  ## Stage 1: Perform per-release operations
  logdevinfo("Preparing per-release cluster plots", logger="analyse_ts")
  cycles <- get.cycles(conf)

  clusters.stats.list <- mclapply.db(conf, seq_along(cycles$range.id),
                                     function(conf, i) {
    clusters.stats <- compute.release.clusters.stats(conf, cycles$range.id[[i]],
                                                     cluster.method)

    ## NOTE: clusters.stats can return an empty list, which happens when
    ## there are no clusters. Return a NULL element in this case.
    if(!is.null(clusters.stats)) {
        clusters.stats$cycle <- cycles$cycle[[i]]
    }

    return (clusters.stats)
  })

  dummy <- mclapply(seq_along(clusters.stats.list), function(i) {
    range.id <- cycles$range.id[[i]]
    clusters.stats <- clusters.stats.list[[i]]

    ## Create release-specific plots
    ## NOTE: We need to decide if it's statistically acceptable
    ## to sqrt-transform a boxplot. Purists would deem it inappropriate,
    ## but I'm not sure if we should just ignore these voices in our heads.
    if (!is.null(clusters.stats)) {
        g <- ggplot(clusters.stats, aes(x=cluster.id, y=rankValue)) +
            geom_boxplot(position="dodge") + scale_y_log10() +
                xlab("Cluster No.") + ylab("Page rank (median)")
        ggsave(file.path(graphdir, paste("cluster_prank_",
                                         conf$boundaries$tag[i],
                                         ".pdf", sep="")),
               g, width=7, height=7)

        g <- ggplot(clusters.stats, aes(x=cluster.id, y=total)) +
            geom_boxplot(position="dodge") + scale_y_sqrt() +
                xlab("Cluster No.") + ylab("Amount of code changes (add+del)")
        ggsave(file.path(graphdir, paste("cluster_code_changes_",
                                         conf$boundaries$tag[i], ".pdf",
                                         sep="")),
               g, width=7, height=7)
    }
  })


  ## Stage 2: Perform global operations on all releases
  ## TODO: Augment the date labels with release specifications; additionally,
  ## sort the clusters by average page rank per release
  logdevinfo("Preparing global cluster plots", logger="analyse_ts")

  ## Only consider releases that do have clusters
  valid.idx <- sapply(clusters.stats.list, function(x) {
      length(x$cluster.id) > 0 })
  clusters.stats.list <- clusters.stats.list[valid.idx]
  clusters.all <- do.call(rbind, clusters.stats.list)

  clusters.summary.all <- do.call(rbind, lapply(clusters.stats.list,
                                                summarise.clusters.stats))

  ## For very small projects, it can happen that there is not even a single
  ## subcluster for all releases
  if (!all(sapply(clusters.all, is.null))) {
    g <- ggplot(clusters.all, aes(x=cluster.id, y=rankValue)) +
      geom_boxplot(position="dodge") + scale_y_log10() + xlab("Cluster No.") +
        ylab("Page rank") + facet_wrap(~cycle, scales="free_x")
    ggsave(file.path(graphdir, "cluster_prank_ts.pdf"), g, width=12, height=8)

    clusters.molten <- melt(clusters.all, id.vars=c("cluster.id", "cycle"),
                            measure.vars=c("rankValue", "total", "numcommits"))
    clusters.molten$cycle <- as.factor(clusters.molten$cycle)

    g <- ggplot(clusters.molten, aes(x=cluster.id, y=value)) +
      geom_boxplot(position="dodge") + scale_y_log10() + xlab("Cluster No.") +
        ylab("Magnitude of covariate") + facet_grid(variable~cycle, scales="free")
    ggsave(file.path(graphdir, "cluster_comparison_ts.pdf"), g,
           width=12, height=8)
  }
}


## Given two cluster ids, retrieve the persons in each cluster from
## the database, and compute an overlap measure (size of intersecting
## members divided by the size of the larger cluster).
## If less than MIN.SIMILARITY (in percent) of the members of
## the _smaller_ cluster are contained in the intersection, we regard
## the clusters as totally disjoint and assign a zero similarity.
MIN.SIMILARITY <- 10
clusters.simple.similarity <- function(conf, c1.id, c2.id) {
  c1.members <- query.cluster.members(conf, c1.id)
  c2.members <- query.cluster.members(conf, c2.id)

  smaller.size <- min(length(c1.members), length(c2.members))
  larger.size <- max(length(c1.members), length(c2.members))

  clust.isct <- intersect(c1.members, c2.members)

  if (length(clust.isct) < MIN.SIMILARITY/100*smaller.size) {
    return (0.0)
  }

  return(length(clust.isct)/larger.size)
}

## Compute a mapping which clusters belong together across releases
determine.cluster.mapping <- function(conf, cluster.method=cluster.methods[1]) {
  if (!cluster.method.valid(cluster.method)) {
    stop("Internal error: Specify a supported clustering type!")
  }
  ## Clusters that are classified to match across two releases
  ## are identified by the same numerical label.

  ## For each release range, obtain a data frame that contains all
  ## cluster ids for the release and a (yet undefined) label.
  range.ids <- query.range.ids(conf)

  if (length(range.ids) != dim(conf$boundaries)[1]) {
    stop("Internal error: Different # of range ids than rows in conf$boundaries!")
  }

  res <- lapply(1:length(range.ids), function(i) {
    ## TODO: Honour the type argument
    ## new.cluster is set to TRUE is there is no linkage to a cluster
    ## in the preceeding release (appropriate FALSE will be set during
    ## processing)
    range.id <- range.ids[[i]]
    cluster.ids <- query.cluster.ids(conf, range.id, "Spin glass community")

    ## boundaries.index can be used as index into conf$boundaries
    ## to determine
    return(data.frame(new.cluster=TRUE, label=NA, cluster.id=cluster.ids,
                      boundaries.index=i))
  })

  ## Labelling the clusters in the first release is simple: Just use
  ## consecutive numbers. Every cluster is necessarily new.
  res[[1]]$label = 0:(length(res[[1]]$label)-1)
  next.label <- length(res[[1]]$label)

  ## Compute similarities between all clusters in range i and the
  ## clusters in range i+1.
  for (i in 1:(length(res)-1)) {
    logdevinfo(paste("Computing for range", i), logger="analyze_ts")
    clust.sim <- expand.grid(c1=res[[i]]$cluster.id,
                             c2=res[[i+1]]$cluster.id)
    clust.sim$sim <- sapply(1:dim(clust.sim)[1], function(j) {
      c.ids <- clust.sim[j,]
      ## We could save some effort by querying the cluster members once
      ## and then re-using the results.
      return(clusters.simple.similarity(conf, c.ids$c1, c.ids$c2))
    })

    ## Remove total mismatches, and sort the matches by decreasing strength
    clust.sim <- clust.sim[clust.sim$sim>0,]
    clust.sim <- clust.sim[sort(clust.sim$sim, index.return=TRUE, decreasing=TRUE)$ix,]

    ## Systematically pick the best matches
    for (j in 1:dim(clust.sim)[1]) {
      ## Given the assignment from c1 to c2, determine which label c1 has in
      ## the previous range
      c1 = clust.sim[j,]$c1
      c2 = clust.sim[j,]$c2

      label.c1 <- res[[i]][which(res[[i]]$cluster.id == c1),]$label

      ## If c2 does not yet have a label (labels that were assigned earlier
      ## have higher prio because they stem from higher similarity values)
      ## _and_ if the label is not yet used, assign it to c2
      label.c2 <- res[[i+1]][which(res[[i+1]]$cluster.id == c2),]$label

      if(is.na(label.c2) & !(label.c1 %in% res[[i+1]]$label)) {
        res[[i+1]][which(res[[i+1]]$cluster.id == c2),]$label <- label.c1
      }
    }

    ## Finally, assign new labels to all clusters that have not been
    ## labelled yet.
    unlabelled.idx <- which(is.na(res[[i+1]]$label))
    labelled.idx <- which(!is.na(res[[i+1]]$label))

    if (length(unlabelled.idx) > 0) {
      res[[i+1]][unlabelled.idx,]$label <-
        next.label:(next.label+length(unlabelled.idx)-1)
    }

    next.label <- next.label + length(unlabelled.idx)

    ## Mark all clusters that have been carried over from the
    ## previous release
    if (length(labelled.idx) > 0) {
      res[[i+1]][labelled.idx,]$new.cluster <- FALSE
    }
  }

  return(res)
}

do.commit.analysis <- function(resdir, graphdir, conf) {
  subset <- c("CmtMsgBytes", "ChangedFiles", "DiffSize", "NumTags", "inRC")
  ts <- get.commits.by.ranges(conf, subset, normalise.commit.dat)

  logdevinfo("Plotting the commit information time series", logger="analyse_ts")
  ## Stage 1: Plot the complete commit information time series
  ts <- do.call(rbind, ts)

  ## Same test for RC cycles as before, but on the global data set
  plot.types <- c("CmtMsgBytes", "ChangedFiles", "DiffSize")
  id.types <- c("revision", "date")
  subset <- c("CmtMsgBytes", "ChangedFiles", "DiffSize")

  has.tags <- (sum(ts$NumTags) > 0)
  has.rcs <- (length(unique(ts$inRC)) > 1)

  if (has.tags) {
    plot.types <- c(plot.types, "NumTags")
    subset <- c(subset, "NumTags")
  }

  if (has.rcs) {
    id.types <- c(id.types, "inRC")
    subset <- c(subset, "inRC")
  }

  ts.molten <- melt(ts[c("revision", "date", subset)],
                    id=id.types)
  if (has.rcs) {
    levels(ts.molten$inRC) <- c("No", "Yes")
  }

  ## TODO: Does not work when date is used instead of revision,
  ## which is the desirable alternative
  if (has.rcs) {
    g <- ggplot(data=ts.molten, aes(x=revision, y=value, colour=inRC))
  } else {
    g <- ggplot(data=ts.molten, aes(x=revision, y=value))
  }

  g <- g + geom_boxplot(fill="NA") + scale_y_log10() +
    facet_wrap(~variable, scales="free") + xlab("Revision") +
      ylab("Value (log. scale)") +
      scale_colour_discrete("Release\nCandidate")
  ggsave(file.path(graphdir, "ts_commits.pdf"), g, width=12, height=8)

  ## Export the SVG representation to the data base
  ggsave(file.path(graphdir, "ts_commits.svg"), g, width=12, height=8)
  dat.svg <- readLines(file.path(graphdir, "ts_commits.svg"))
  ## TODO: Do the actual DB export
  file.remove(file.path(graphdir, "ts_commits.svg"))


  ## Stage 2: Plot annual versions of the commit time series
  min.year <- year(min(ts.molten$date))
  max.year <- year(max(ts.molten$date))

  dummy <- sapply(seq(min.year, max.year), function(year) {
    if (dim(ts.molten[year(ts.molten$date)==year,])[1] == 0) {
      logdevinfo(paste("Skipping annual commit time series for", year, "(no release)"), logger="analyse_ts")
      return(NA)
    }
    logdevinfo(paste("Creating annual commit time series for", year), logger="analyse_ts")
    dat <- ts.molten[year(ts.molten$date)==year,]

    # Don't plot tagging information if there are no tags
    has.tags <- sum(dat[dat$variable=="NumTags",]$value)

    if (!has.tags) {
      dat <- dat[dat$variable!="NumTags",]
    }

    if (has.rcs) {
      g <- ggplot(data=dat, aes(x=revision, y=value, colour=inRC))
    } else {
      g <- ggplot(data=dat, aes(x=revision, y=value))
    }
    g <- g + geom_boxplot(fill="NA") + scale_y_log10() +
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
    g <- g + geom_rect(aes(NULL, NULL, xmin=date.rc.start,
                           xmax=date.end, ymin=ymin, ymax=ymax, fill="blue"),
                       data=na.omit(boundaries.plot))

  }

  ggsave(file.path(graphdir, "ts.pdf"), g, width=16, height=7)

  ## Store the complete time series information into the database
  logdevinfo("Storing time series data into database", logger="analyse_ts")
  for (type in unique(series.merged$type)) {
    plot.name <- str_c("Progress TS [", type, "]")
    plot.id <- get.clear.plot.id(conf, plot.name)

    series.sub <- series.merged[series.merged$type==type,]

    dat <- data.frame(time=as.character(series.sub$time),
                      value=series.sub$value,
                      value_scaled=series.sub$value.scaled,
                      plotId=plot.id)
    res <- dbWriteTable(conf$con, "timeseries", dat, append=TRUE, row.names=FALSE)
    if (!res) {
      stop("Internal error: Could not write timeseries into database!")
    }
  }

  ## Create annual versions of the plots
  min.year <- year(min(series.merged$time))
  max.year <- year(max(series.merged$time))

  dummy <- sapply(seq(min.year, max.year), function(year) {
    logdevinfo(paste("Creating annual time series for", year), logger="analyse_ts")
    g.year <- g + xlim(dmy(paste("1-1-", year, sep=""), quiet=TRUE, tz = "UTC"),
                       dmy(paste("31-12-", year, sep=""), quiet=TRUE, tz = "UTC")) +
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
  plot.id <- get.clear.plot.id(conf, plot.name)

  dat <- compute.release.distance(series.merged, conf)
  if (!is.na(dat)) { # if too few revisions are present, skip further analysis
    dat <- data.frame(time=as.character(conf$boundaries$date.end[-1]), value=dat,
                      value_scaled=dat, plotId=plot.id)

    res <- dbWriteTable(conf$con, "timeseries", dat, append=TRUE, row.names=FALSE)
    if (!res) {
      stop("Internal error: Could not write release distance TS into database!")
    }
  }

  ## TODO: Compute the difference between release cycles and a
  ## per-determined, desirable shape of the release curve.
}


## Given the sloccount time series fragments in dat, create
## coherent time series for a particular type and store the resulting
## time series in the data base
process.sloccount.ts <- function(con, pid, dat, type="total.cost") {
    if (!(type %in% c("person.months", "total.cost", "schedule.months",
                      "avg.devel"))) {
        type <- "total.cost"
    }

    ## Adaptively change values to suitable units
    if (type =="total.cost") {
        if (max(dat$total.cost) > 5000*1000) {
            dat$total.cost <- dat$total.cost/(1000*1000)
            total.cost.unit <- "MEUR"
        } else if (max(dat$total.cost) > 5000) {
            dat$total.cost <- dat$total.cost/1000
            total.cost.unit <- "kEUR"
        } else {
            total.cost.unit <- "EUR"
        }
    }

    if (type=="person.months" || type=="schedule.months") {
        if (max(dat[,type]) > 24) {
            dat[,type] <- dat[,type]/12
            duration.unit <- "Years"
        } else {
            duration.unit <- "Months"
        }
    }

    label <- switch(type,
                    person.months = str_c("Person ", duration.unit),
                    total.cost = str_c("Cost [", total.cost.unit, "]"),
                    avg.devel = "Average # of Developers",
                    schedule.months = str_c("Scheduled duration [", duration.unit, "]"))

    ## It's admissible to remove outliers for the ressource plots because
    ## all measured/derived quantities are supposed to be continuous.
    ## Since we randomly select commits to obtain snapshots that are analysed,
    ## outliers are possible when we hit a commit on a branch that is substantially
    ## different than the surrounding main development state.
    dat.ts <- xts(dat[,type], order.by=dat$time)
    dat.ts <- na.omit(apply.monthly(dat.ts, median))
    ts.df <- ts.to.df(dat.ts)

    return(list(ts.df=ts.df, label=label))
}

## Dispatch sloccount time series construction for the various possible
## alternatives
do.sloccount.analysis <- function(conf, pid) {
    if (conf$sloccount == FALSE) {
      return(NULL)
    }

    ## The plot id has already been created in complexity.r
    plot.id <- get.plot.id(conf, "sloccount")
    dat <- query.sloccount.ts(conf$con, plot.id)

    for (type in c("person.months", "total.cost", "schedule.months",
                   "avg.devel")) {
        res <- process.sloccount.ts(conf$con, pid, dat, type)

        plot.name <- str_c("sloccount (", type, ")")
        plot.id <- get.clear.plot.id(conf, plot.name, labely=res$label)

        dat.out <- data.frame(time=res$ts.df$t, value=res$ts.df$val,
                              value_scaled=0, plotId=plot.id)

        res <- dbWriteTable(conf$con, "timeseries", dat.out, append=TRUE,
                            row.names=FALSE)
        if (!res) {
            stop("Internal error: Could not sloccount TS into database!")
        }
    }
}

## Compute time serie based on understand complexity analysis results
do.understand.analysis <- function(conf, pid) {
    if (conf$understand == FALSE) {
      return(NULL)
    }

    ## The plot id for the raw data has already been created in complexity.r
    plot.id.base <- get.plot.id(conf, "understand_raw")

    ## Choose two metrics as example. Other suitable metrics
    ## should be determined.
    for (type in c("RatioCommentToCode", "CountPath")) {
        dat <- query.understand.ts(conf$con, plot.id.base, type)

        plot.name <- str_c("Understand (", type, ")")
        plot.id <- get.clear.plot.id(conf, plot.name, labely=type)

        dat.out <- data.frame(time=dat$time, value=dat$value,
                              value_scaled=0, plotId=plot.id)

        res <- dbWriteTable(conf$con, "timeseries", dat.out, append=TRUE,
                            row.names=FALSE)
        if (!res) {
            stop("Internal error: Could not write understand TS into database!")
        }
    }
}


######################### Dispatcher ###################################
config.script.run({
  conf <- config.from.args(positional.args=list("resdir"),
                           require.project=TRUE)
  resdir <- conf$resdir
  graphdir <- file.path(resdir, "graphs")
  logdevinfo(paste("graphdir is", graphdir), logger="analyze_ts")
  dir.create(graphdir, showWarnings=FALSE, recursive=TRUE)

  if (conf$profile) {
    ## R cannot store line number profiling information before version 3.
    if (R.Version()$major >= 3) {
      Rprof(filename="ts.rprof", line.profiling=TRUE)
    } else {
      Rprof(filename="ts.rprof")
    }
  }

  conf <- init.mc(conf)
  do.ts.analysis(resdir, graphdir, conf)
  logdevinfo("-> Finished time series base analysis", logger="analyse_ts")
  ## NOTE: The processed (smoothed, cumulated) time series are available in the
  ## database only after do.ts.analysis(), so this task does not commute
  ## with the ones below.

  do.commit.analysis(resdir, graphdir, conf)
  logdevinfo("-> Finished commit analysis", logger="analyse_ts")

  do.cluster.analysis(resdir, graphdir, conf)
  logdevinfo("-> Finished cluster analysis", logger="analyse_ts")

  do.release.analysis(resdir, graphdir, conf)
  logdevinfo("-> Finished release analysis", logger="analyse_ts")

  do.update.timezone.information(conf, conf$pid)
  logdevinfo("-> Finished time zone analysis", logger="analyse_ts")

  do.sloccount.analysis(conf, conf$pid)
  logdevinfo("-> Finished sloccount time series analysis", logger="analyse_ts")

  do.understand.analysis(conf, conf$pid)
  logdevinfo("-> Finished complexity time series analysis", logger="analyse_ts")

  Rprof(NULL)
})
