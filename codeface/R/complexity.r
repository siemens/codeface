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
## Copyright 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Analysis pass that computes complexity and cost estimate metrics
## using understand and sloccount

suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(reshape))
source("config.r")
source("db.r")
source("query.r")
source("system.r")
source("mc_helpers.r")
source("sloccount.r")

## Sample commits from a given release range, and ensure that the sampled
## commits are properly time-ordered
MAX.SAMPLES.PER.CYCLE <- 50
sample.commits <- function(conf, range.id) {
  cycles <- get.cycles(conf)
  cycles <- cycles[cycles$range.id==range.id,]

  ## For a development cycle that took n days, sample n/7 commits
  ## (i.e., one sample per week), with an upper bound
  ## MAX.SAMPLES.PER.CYCLE per development cycle (this is a safety
  ## precaution against multi-year development cycles where only little
  ## happens)
  num.samples <- as.integer(ceiling(difftime(cycles$date.end,
                                             cycles$date.start, units="days")))
  num.samples <- num.samples/7
  if (num.samples > MAX.SAMPLES.PER.CYCLE) {
      num.samples <- MAX.SAMPLES.PER.CYCLE
  }
  if (num.samples == 0) {
    num.samples <- 1
  }

  ## If there are not enough commits for the sampling goal,
  ## reduce the number accordingly
  commits <- get.commit.hashes.by.range(conf, range.id)
  if (num.samples > nrow(commits)) {
    num.samples <- nrow(commits)
  }

  ## Instead of sampling randomly, we take every nth commit,
  ## which is deterministically reproducible. Nonetheless, it preserves
  ## the property that more commits are samples from high-intensity
  ## regions, and vice versa.
  nth <- ceiling(nrow(commits)/num.samples)
  commits.sampled.idx <- 1:nrow(commits) %% nth == 0

  return(commits[commits.sampled.idx,])
}


perform.git.checkout <- function(repodir, commit.hash, code.dir, archive.file) {
  args <- str_c(" --git-dir=", repodir, " archive -o ", archive.file,
                " --format=tar --prefix='code/' ", commit.hash)
  do.system("git", args)

  args <- str_c("-C ", code.dir, " -xf ", archive.file)
  do.system("tar", args)
}

do.understand.analysis <- function(code.dir, results.file) {
  ## TODO: This should be overwriteable by the local configuration files
  metrics.list <- c("RatioCommentToCode", "Cyclomatic", "MaxCyclomatic",
                    "AvgEssential", "MaxNesting", "CountStmt", "Cyclomatic",
                    "CountLineInactive", "AvgLineBlank", "CountPath",
                    "Knots", "AvgLineCode", "AvgCyclomatic",
                    "AvgLine", "CountLinePreprocessor", "AvgCyclomaticModified",
                    "CountDeclFunction", "CyclomaticStrict", "SumCyclomatic",
                    "CountLineCode", "PercentLackOfCohesion",
                    "CountLine", "SumCyclomaticStrict", "MinEssentialKnots",
                    "MaxEssentialKnots", "SumEssential", "MaxNesting",
                    "MaxInheritanceTree", "CountOutput")

  ## Collect the names of all files to be analysed, and store them
  ## in a file
  save.dir <- getwd()
  setwd(str_c(code.dir, "/code"))

  files <- list.files(".", recursive=TRUE)
  filenames.file <- tempfile()
  write.table(files, filenames.file, quote=FALSE, row.names=FALSE, col.names=FALSE)

  db.file <- tempfile()
  cmd <- str_c("und create -db ", db.file)
  dummy <- do.system.raw(cmd)

  und.db <- str_c("und -db ", db.file)

  cmd <- str_c(und.db, " add @", filenames.file)
  dummy <- do.system.raw(cmd)

  cmd <- str_c(und.db, " analyze")
  dummy <- do.system.raw(cmd)

  cmd <- str_c(und.db, " settings -metrics ", paste(metrics.list, collapse=" "))
  dummy <- do.system.raw(cmd)

  cmd <- str_c(und.db, " settings -MetricOutputFile ", results.file)
  dummy <- do.system.raw(cmd)

  ## Finally, export the selected metrics
  cmd <- str_c(und.db, " metrics ")
  dummy <- do.system.raw(cmd)

  setwd(save.dir)
}

parse.understand <- function(results.file, commit.info) {
  dat <- read.csv(results.file, sep=",", header=TRUE)

  ## NOTE: dat can contain zero rows when understand does not recognise any
  ## files, which happens for web-only projects, or when the sampling
  ## has picked a mostly empty branch.
  if (nrow(dat) == 0) {
    return(NULL)
  }

  dat <- cbind(commitHash=commit.info$commitHash,
               commitDate=commit.info$commitDate, dat)
  dat.molten <- melt(dat, id.vars=c("commitDate", "commitHash", "Kind", "Name"))

  return(dat.molten)
}

## Compute the statistics required for a boxplot, and save the
## results in a data frame
compute.boxplot.stats <- function(v) {
  bps <- boxplot.stats(v)
  res <- rbind(data.frame(type="q1", value=bps$stats[1]),
               data.frame(type="q2", value=bps$stats[2]),
               data.frame(type="q3", value=bps$stats[3]),
               data.frame(type="q4", value=bps$stats[4]),
               data.frame(type="q5", value=bps$stats[5]),
               data.frame(type="num.observations", value=bps$n),
               data.frame(type="conf1", value=bps$conf[1]),
               data.frame(type="conf1", value=bps$conf[2]))

  if (length(bps$out) > 0) {
    res <- rbind(res, data.frame(type="outlier", value=bps$out))
  }

  return(res)
}


## Given a project configuration with a release range id, prepare
## a sample list of commits, analyse a snapshot of the code for each
## of them with understand, parse the results and store them into
## the database.
do.complexity.analysis <- function(conf) {
  commits.list <- sample.commits(conf, conf$range.id)

  loginfo(str_c("Analysing ", nrow(commits.list), " code samples\n"),
          logger="complexity")

  ## NOTE: Temporary directories are shared between threads
  ## in an mclapply call. To make things deterministic, we
  ## obtain a tmporary directory in the beginning, and then
  ## create sub-directories by thread number 
  temp.dir <- tempdir()

  ## Obtain a plot IDs for the sloccount and understand raw time series before
  ## parallel processing commences to avoid race conditions
  sloccount.plot.id <- get.or.create.plot.id(conf, "sloccount")
  understand.plot.id <- get.or.create.plot.id(conf, "understand_raw")

  res.list <- mclapply.db(conf, 1:nrow(commits.list), function(conf, i) {
      loginfo(str_c("Analysing sample ", i, "\n"), logger="complexity")
      commit.hash <- commits.list[[i, "commitHash"]]
      commit.date <- commits.list[[i, "commitDate"]]

      archive.file <- tempfile() # tarball from "git archive"
      results.file <- tempfile() # understand database

      code.dir <- file.path(temp.dir, i)
      dir.create(code.dir, showWarnings=FALSE)

      loginfo(str_c("Checking out revision ", commit.hash, " into ",
                    code.dir, "\n"), logger="complexity")
      perform.git.checkout(conf$repodir, commit.hash, code.dir, archive.file)

      loginfo(str_c("Performing understand analysis for ", commit.hash, "\n"),
              logger="complexity")
      do.understand.analysis(code.dir, results.file)

      ## The understand output still needs to be heavily post-processed.
      ## This is faster when we can select portions of the data from the
      ## SQL database, so we store a "rough" version of the data from
      ## which the actual time series are extracted later on.
      res.understand <- parse.understand(results.file, commits.list[i,])
      commit.date <- res.understand$commitDate[1]
      res.understand <- res.understand[,c("Kind", "Name", "variable",
                                          "value")]
      if (ncol(res.understand) == 4) {
        colnames(res.understand) <- c("kind", "name",
                                      "variable", "value")

        ## TODO: It should be possible top avoid calling compute.boxplot.stats
        ## twice somehow.
        res.understand <- ddply(res.understand, ~kind+variable, summarise,
                                name=compute.boxplot.stats(value)$type,
                                value=compute.boxplot.stats(value)$value)
        res.understand <- cbind(plotId=understand.plot.id,
                                time=commit.date, res.understand)

        res <- dbWriteTable(conf$con, "understand_raw", res.understand,
                            append=TRUE, row.names=FALSE)

        if (!res) {
          stop("Internal error: Could not write sloccount timeseries into database!")
        }
      } else {
        loginfo(str_c("Warning: understand analysis failed for ", commit.hash,
                      " -- skipping this sample"), logger="complexity")
      }

      loginfo(str_c("Performing sloccount analysis for ", commit.hash, "\n"),
              logger="complexity")
      res <- do.sloccount.analysis(code.dir)
      add.sloccount.ts(conf, sloccount.plot.id, commit.date, res)
      loginfo("Finished analysing sample ", i, "\n", logger="complexity")

      return(NULL)
  })

  ## The temporary files that have been created are all located
  ## in the temporary directory and are therefore implicitely removed
  ## by the unlink call.
  unlink(temp.dir, recursive=TRUE)
}

## ################# Dispatcher ######################
config.script.run({
  conf <- config.from.args(positional.args=list("repodir", "range.id"),
                           require.project=TRUE)

  conf <- init.mc(conf)
  loginfo(str_c("Using ", conf$jobs, " jobs\n"), logger="complexity")
  do.complexity.analysis(conf)
})
