# Some utility functions that can be loaded with source("utility.r")

# This file is part of Codeface. Codeface is free software: you can
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
# Copyright 2010, 2011 by Wolfgang Mauerer <wm@linux-kernel.net>
# Copyright 2012, 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.

## Interpret an integer as timestamp from the UNIX epoch
tstamp.to.POSIXct <- function(z) as.POSIXct(as.integer(z), origin="1970-01-01")

## Convert a time series into a data frame
## The data frame contains the timestamps (index(ts)),
## the matrix of observations (coredata(ts))
gen.df.from.ts <- function(ts, type) {
  data.frame(time=index(ts), value=coredata(ts),
             value.scaled=scale.data(coredata(ts)), type=type)
}

## Scale a given data set to the range [min,max]
scale.data <- function(dat, .min=0, .max=1) {
  ## calculate the scale factor with which to divide the data
  maxDat <- max(dat)
  scale.factor <- (maxDat - min(dat))/(.max - .min)
  ## If the data is not all the same, do apply the scale factor
  if (scale.factor > 0) {
    dat <- dat / scale.factor
    maxDat <- maxDat / scale.factor
  }
  ## Set the maximum to .max
  dat <- dat + (- maxDat + .max)
  return(dat)
}

## Given two lists from and to, create an interleaved list.
## interleave.lists(c(1,2,3), c(4,5,6)), for instance, delivers
## c(1,4,2,5,3,6).
do.interleave <- function(from, to) {
    if (length(from) != length(to)) {
        logwarn(paste("Warning: Length of lists to be interleaved differ,",
                      "recycling will happen.", logger="utils"))
    }
    return(c(rbind(from, to)))
}

## Given an igraph edge list (data frame with columns toId and fromId), create
## a weighted edge list, where the weight is the number of parallel edges.
gen.weighted.edgelist <- function(edges) {
  edges.weighted <- lapply(unique(edges$fromId), function(from.vertex) {
    to.vertices <- edges[edges$fromId==from.vertex,]$toId
    res <- do.call(rbind, lapply(unique(to.vertices), function(to.vertex) {
      return(data.frame(fromId=from.vertex, toId=to.vertex,
                        weight=sum(to.vertices==to.vertex)))
    }))

    return(res)
  })

  return(do.call(rbind, edges.weighted))
}

## Give a cluster identifier and (optionally) the page rank technique,
## (re)construct an igraph object from the DB
construct.cluster <- function(con, cluster.id, technique=0) {
  edges <- query.cluster.edges(con, cluster.id)
  members <- query.cluster.members(con, cluster.id, prank=TRUE, technique=technique)

  if (!all(unique(c(edges$toId, edges$fromId)) %in% members$person)) {
    stop("Internal error: edges for non-existent persons in cluster ", cluster.id)
  }

  if (is.null(edges)) {
    logwarn(paste("Duh: cluster without edges for id ", cluster.id, "?!\n"),
            logger="util")
    return(NULL)
  }

  g <- graph.data.frame(edges, vertices=members)
  return(g)
}


## select from a variety of output devices
## Args:
##  filename: string containing the output path and filename
##  size    : size of graphic output in inches
##  format  : string to specifiy bmp, jpeg, png, tiff formats
## Returns:
##  output of device opening function
select.graphics.dev <- function(filename, size, format="png") {
  formats <- c(bmp, jpeg, png, tiff)
  names(formats) <- c("bmp", "jpeg", "png", "tiff")

  if (!(format %in% names(formats))) {
    format <- "png"
  }

  dev <- formats[[format]](filename=filename, width=size, height=size,
                         units="in", res=320)
  return(dev)
}


get.num.cores <- function() {
  n.cores <- detectCores(logical=TRUE)
  if (is.na(n.cores)) n.cores <- 2

  return(n.cores)
}


perform.git.checkout <- function(repodir, commit.hash, code.dir, archive.file) {
  args <- str_c(" --git-dir=", repodir, " archive -o ", archive.file,
                " --format=tar --prefix='code/' ", commit.hash)
  do.system("git", args)

  args <- str_c("-C ", code.dir, " -xf ", archive.file)
  do.system("tar", args)
}

## Return the content of a file at a given revision
show.git.file <- function(repodir, commit.hash, file) {
  args <- str_c(" --git-dir=", repodir, " show ", commit.hash, ":", file, sep="")
  return(do.system("git", args))
}

## Some helper functions to ensure that functions (and the
## configuration parser) receive correct parameter values in the
## conway analysis
ensure.supported.artifact.type <- function(artifact.type) {
    if(!(artifact.type %in% c("function", "file", "feature"))) {
        stop(str_c("Internal error: Artifact type ", artifact.type,
                   " is unsupported!"))
    }
}

ensure.supported.dependency.type <- function(dependency.type) {
    if(!(dependency.type %in% c("co-change", "dsm", "feature_call", "semantic", "none"))) {
        stop(str_c("Internal error: Dependency type ", dependency.type,
                   " is unsupported!"))
    }
}

ensure.supported.quality.type <- function(quality.type) {
    if(!(quality.type %in% c("corrective", "defect"))) {
        stop(str_c("Internal error: Quality type ", quality.type,
                   " is unsupported!"))
    }
}

ensure.supported.communication.type <- function(communication.type) {
    if(!(communication.type %in% c("mail", "jira", "mail+jira"))) {
        stop(str_c("Internal error: Communication type ", communication.type,
                   " is unsupported!"))
    }
}

## Generate a directory name as basis for range-specific output files
## Form: 001--abcdef-7fbd3d, i.e., the range id filled to three positions,
## and the first 6 characters of start and end revision.
gen.range.path <- function(i, cycle) {
    revs <- strsplit(cycle, "-")[[1]]
    return(str_c(str_pad(i, width=3, side="left", pad="0"), "--",
                 substr(revs[1], 0, 6), "-", substr(revs[2], 0, 6), sep=""))
}
