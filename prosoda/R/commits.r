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

## Process commit information and create descriptive/visual statistics
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(corrgram))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(lubridate))
source("plot.r")

## Transform commit information data into a form that is easier to visualise
normalise.commit.dat <- function(dat, subset=NULL) {
  if (is.null(subset)) {
    subset <- colnames(dat)
  }
  dat.subset <- dat[subset]

  ## The size of a diff and the number of changed files can be 0, which
  ## is obviously problematic for the logarithm. Replace these cases
  ## with NA
  dat.subset$LogChangedFiles <- log(dat.subset$ChangedFiles)
  dat.subset$LogDiffSize <- log(dat.subset$DiffSize)
  dat.subset$LogDiffSize[dat.subset$LogDiffSize==-Inf]=0
  dat.subset$LogChangedFiles[dat.subset$LogChangedFiles==-Inf]=0
  dat.subset$LogCmtMsgBytes <- log(dat.subset$CmtMsgBytes)
  dat.subset$LogCmtMsgBytes[dat.subset$LogCmtMsgBytes==-Inf]=0 #NA
  dat.subset$inRC <- as.factor(dat.subset$inRC)

  ## Remove extreme outliers in the non-logged version
  dat.subset$DiffSize <- removeOutliers(dat.subset$DiffSize)
  dat.subset$ChangedFiles <- removeOutliers(dat.subset$ChangedFiles)

  return(dat.subset)
}


## Generate a scatterplot that provides an exploratory overview
## about the commit information in dat
gen.commits.splom <- function(cmt.info, plot.types) {
  plot.list <- plot.splom(plot.types, cmt.info)
  res <- do.call(grid.arrange, c(plot.list, list(nrow=length(plot.types),
                                                 ncol=length(plot.types))))

  return(res)
}


gen.commits.corrgram <- function(cmt.info, plot.types, title=NULL) {
  return(corrgram(cmt.info[,plot.types], order=FALSE,
                  lower.panel=panel.shade, upper.panel=panel.pie,
                  main=title))
}


## Obtain commit statistics for the release range identified by range.id
## (use a covariable selection suitable for most analysis and plotting passes)
gen.commits.info <- function(con, pid, range.id) {
  subset <- c("CmtMsgBytes", "ChangedFiles", "DiffSize", "NumTags", "inRC")
  cmt.info <- get.commits.by.range.con(con, pid, range.id, subset,
                                       normalise.commit.dat)

  plot.types <- c("CmtMsgBytes", "ChangedFiles", "DiffSize")
  if (sum(cmt.info$NumTags) > 0) {
    ## The data do contain tagging information
    plot.types <- c(plot.types, "NumTags")
  }

  return(list(cmt.info=cmt.info, plot.types=plot.types))
}

## Prepare base data for commits activity calculations
gen.commits.activity <- function(con, pid, range.id) {
  subset <- c("commitDate", "DiffSize")
  cmt.info <- get.commits.by.range.con(con, pid, range.id)
  cmt.info <- cmt.info[subset]

  return(cmt.info)
}

## Compute commit activity statistics resolved by week day and hour
## to get an impression when contributors work on the code
## Use FUN=sum to get the sum of all diff sizes, and FUN=length to get the
## number of commits
compute.hourly.statistics <- function(ts, FUN=length) {
  wday.labels <- c("Sun", "Mon", "Thu", "Wed", "Thu", "Fri", "Sat")
  wday.labels.mon <- c("Mon", "Thu", "Wed", "Thu", "Fri", "Sat", "Sun")

  res <- lapply(1:7, function(i) {
    ts.sub <- ts[wday(ts)==i]
    if (length(ts.sub) == 0) {
      return(NULL)
    }
    act <- aggregate(coredata(ts.sub), by=list(hour=hour(ts.sub)), FUN)
    colnames(act) <- c("hour", "size")
    return(data.frame(day=i, act))
  })

  res <- do.call(rbind, res)

  ## Convert the labels to use monday as first day of the week
  res$day <- mapvalues(res$day, 1:7, c(7, 1:6), warn_missing=FALSE)
  res <- arrange(res, res$day)
  res$day <- factor(res$day, levels=7:1, ordered=TRUE)
  res$day <- mapvalues(res$day, 1:7, wday.labels.mon, warn_missing=FALSE)

  return(res)
}
