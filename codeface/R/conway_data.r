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
## Copyright 2017 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

## Helper functions to read and process result data from the Conway analysis
## TODO: Once the conway data are available in the data base, reading
## the data needs to be replaced with appropriate DB queries.

## Provide time-resolved raw data from the quality analysis
source("quality_analysis.r")

## TODO: Document return result
get.conway.artifact.data.ts <- function(conf, resdir, motif.type, keep.list=NULL) {
    cycles <- get.cycles(conf)
    res <- lapply(1:nrow(cycles), function(i) {
        logdevinfo(str_c("Analysing quality cycle ", i), logger="conway")
        res <- query.conway.artifact.data(conf, i, resdir, conf$qualityType, motif.type,
                                          keep.list, replace.labels=FALSE)
        if (is.null(res)) {
            return(NULL)
        }

        res$date <- cycles[i,]$date.end
        res$range <- i
        return(res)
    })
    res <- do.call(rbind, res)
    if (!is.null(res)) {
        res$date <- as.Date(res$date)
    }

    return(res)
}

## Obtain time-resolved correlation data for the whole analysis range.
## In contrast to get.conway.artifact.data.ts, this function does not
## provide the raw data, but computes correlations from the data
## TODO: Document data format
get.correlations.ts <- function(conf, resdir, motif.type, keep.list=NULL) {
    cycles <- get.cycles(conf)
    res <- lapply(1:nrow(cycles), function(i) {
        logdevinfo(str_c("Analysing quality file for cycle ", i), logger="conway")
        res <- compute.correlations.cycle(conf, i, resdir, conf$qualityType, motif.type, keep.list)
    })

    corr.dat <- do.call(rbind, res)
    if (!is.null(corr.dat)) {
        corr.dat$date <- as.Date(corr.dat$date)
    }

    return(corr.dat)
}

## TODO: Documentation
## If replace.labels is set to TRUE, systematic column names are replaced with human
## readable alternatives
query.conway.artifact.data <- function(conf, i, resdir, quality.type, motif.type, keep.list,
                                       replace.labels=TRUE) {
    cycles <- get.cycles(conf)
    range.resdir <- file.path(resdir, gen.range.path(i, cycles[i,]$cycle))
    quality.file <- file.path(range.resdir, "quality_analysis", motif.type,
                              conf$communicationType, "quality_data.csv")

    if (!file.exists(quality.file)) {
        return(NULL)
    }
    artifacts.dat <- read.csv(quality.file)
    artifacts.dat <- augment.artifact.data(artifacts.dat, quality.type)

    corr.elements <- gen.conway.artifact.columns(quality.type, keep.list)
    artifacts.dat <- artifacts.dat[, corr.elements$names]
    if (replace.labels) {
        colnames(artifacts.dat) <- corr.elements$labels
    }

    if (nrow(artifacts.dat) == 0) {
        return(NULL)
    }

    return(artifacts.dat)
}

compute.correlations.cycle <- function(conf, i, resdir, quality.type, motif.type, keep.list) {
    cycles <- get.cycles(conf)

    ## Always remove motif count and anti-motif count (and the
    ## normalised counts) because these quantities are represented
    ## equally well by motif ratio and percent difference as far
    ## as correlations are concerned
    keep.list <- keep.list[!(keep.list %in% c("motif.count", "motif.anti.count",
                                              "motif.count.norm", "motif.anti.count.norm"))]

    artifacts.subset <- query.conway.artifact.data(conf, i, resdir, quality.type, motif.type, keep.list)
    if (is.null(artifacts.subset)) {
        return(NULL)
    }

    cycle <- cycles[i, ]
    corr.mat <- cor(artifacts.subset, use="pairwise.complete.obs",
                    method="spearman")
    corr.test <- cor.mtest(artifacts.subset) ## Note: can be NULL

    ## Compute labels for the correlated quantities (A:B to indicate correlation
    ## between A and B)
    corr.combinations <- expand.grid(colnames(corr.mat), rownames(corr.mat))
    corr.combinations <- str_c(corr.combinations$Var1, ":", corr.combinations$Var2)
    corr.combinations <- matrix(corr.combinations, nrow=nrow(corr.mat), ncol=ncol(corr.mat))

    return(data.frame(date=cycle$date.end, range=i,
                      combination=corr.combinations[upper.tri(corr.combinations)],
                      value=corr.mat[upper.tri(corr.combinations)]))
}
#####################################


## Combine all per-range raw motif analysis data
## TODO: Document output result
read.motif.results <- function(conf, resdir, motif.type) {
    cycles <- get.cycles(conf)
    res <- do.call(rbind, lapply(1:nrow(cycles), function(i) {
        range.resdir <- file.path(resdir, gen.range.path(i, cycles[i,]$cycle))
        motif.file <- file.path(range.resdir, "motif_analysis", motif.type,
                                conf$communicationType, "raw_motif_results.txt")

        logdevinfo(str_c("Analysing motif file ", motif.file), logger="conway")
        if (!file.exists(motif.file)) {
            return(NULL)
        }
        null.model.dat <- read.table(motif.file, header=TRUE)
        null.model.dat$date <- cycles[i,]$date.end
        null.model.dat$range <- i

        return(null.model.dat)
    }))

    res$date <- as.Date(res$date)
    return(res)
}


## Given data from get.conway.artifact.data.ts, select the covariables relevant
## for reasoning about motifs and developers, and prepare them for time series
## analysis/plotting.
## TODO: Document output format
prepare.abs.ts <- function(res) {
    dat <- res[,c("motif.count", "motif.anti.count", "motif.ratio", "dev.count",
                  "date", "range")]
    dat.molten <- melt(dat, measure.vars=c("motif.count", "motif.anti.count", "motif.ratio"))
    return(dat.molten)
}

## Given data from get.conway.artifact.data.ts, select the covariables relevant
## for reasoning about bugs, and prepare them for time series analysis/plotting.
## TODO: Document output format
prepare.abs.bug.ts <- function(res) {
    dat <- res[,c("motif.count", "motif.anti.count", "motif.ratio",
                  "Churn", "BugIssueCount", "date", "range")]
    dat.molten <- melt(dat, measure.vars=c("motif.count", "motif.anti.count",
                                "motif.ratio"))
    return(dat.molten)
}

## Given the artifact data, compute some derived quantites like ratios, percentages
## and so on. The data are augmented wiht the following covariables:
## motif.percent.diff -- 2|A-M|/(A+M)
## motif.ratio -- M/(A+M)
## motif.count.norm -- # of anti-motifs per developer
## motif.anti.count.norm -- # of anti-motifs per developer
## bug.density (only for jira data) -- number of jira ug issues per LoC (protected against singularities)
augment.artifact.data <- function(artifacts.dat, quality.type) {
    artifacts.dat$motif.percent.diff <- 2 * abs(artifacts.dat$motif.anti.count -
                                                artifacts.dat$motif.count) /
                    (artifacts.dat$motif.anti.count + artifacts.dat$motif.count)
    artifacts.dat$motif.percent.diff.sign <- 2 * (artifacts.dat$motif.anti.count -
                                                 artifacts.dat$motif.count) /
                    (artifacts.dat$motif.anti.count + artifacts.dat$motif.count)
    artifacts.dat$motif.ratio <- artifacts.dat$motif.anti.count /
        (artifacts.dat$motif.count + artifacts.dat$motif.anti.count)
    artifacts.dat$motif.count.norm <- artifacts.dat$motif.count /
        artifacts.dat$dev.count
    artifacts.dat$motif.anti.count.norm <- artifacts.dat$motif.anti.count /
        artifacts.dat$dev.count

    if (quality.type=="defect") {
        artifacts.dat$bug.density <- artifacts.dat$BugIssueCount /
            (artifacts.dat$CountLineCode+1)
    }

    return(artifacts.dat)
}
