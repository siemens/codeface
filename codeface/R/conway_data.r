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

## Collect any constant analysis parameters here
get.conway.params <- function() {
    return(list(person.role="developer", file.limit=30,
                historical.limit <- ddays(365)))
}

## Given a set of type specifications (communication, motif etc.), construct
## a path component like triangle/jira/xyz/... that uniquely identifies
## the type combination.
## This will not be required any more once the data are properly stored in the
## database (appropriate table elements can be used in this case)
gen.conway.path <- function(types, omit.motif=FALSE) {
    dir <- file.path(types$artifact, types$dependency,
                      types$communication, types$quality)
    if (!omit.motif) {
        if (is.null(types$motif)) {
            logwarning("No motif specified in gen.conway.path despite omit.motif=FALSE!",
                       logger="conway_data")
        } else {
            dir <- file.path(dir, types$motif)
        }
    }

    return(dir)
}

types.from.conf <- function(conf) {
    types <- list()
    types$artifact <- conf$artifactType
    types$quality <- conf$qualityType
    types$communication <- conf$communicationType
    types$dependency <- conf$dependencyType

    return(types)
}


## TODO: Document return result
get.conway.artifact.data.ts <- function(conf, resdir, types, keep.list=NULL) {
    cycles <- get.cycles(conf)
    res <- lapply(1:nrow(cycles), function(i) {
        logdevinfo(str_c("Analysing quality cycle ", i), logger="conway")
        res <- query.conway.artifact.data(conf, i, resdir, types,
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
get.correlations.ts <- function(conf, resdir, types, keep.list=NULL,
                                temporal.type="isochronous") {
    cycles <- get.cycles(conf)
    res <- lapply(1:nrow(cycles), function(i) {
        logdevinfo(str_c("Analysing quality file for cycle ", i), logger="conway")
        res <- compute.correlations.cycle(conf, i, resdir, types, keep.list,
                                          temporal.type=temporal.type)
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
query.conway.artifact.data <- function(conf, i, resdir, types, keep.list,
                                       replace.labels=TRUE, temporal.type="isochronous") {
    if (!(temporal.type %in% c("isochronous", "advanced", "retarded"))) {
        stop("Internal error: Unsupported temporal type in query.conway.artifact.data")
    }

    cycles <- get.cycles(conf)
    range.resdir <- file.path(resdir, gen.range.path(i, cycles[i,]$cycle))
    quality.file <- file.path(range.resdir, gen.conway.path(types),
                              str_c("quality_data_", temporal.type, ".csv"))

    if (!file.exists(quality.file)) {
        return(NULL)
    }
    artifacts.dat <- read.csv(quality.file)
    artifacts.dat <- augment.artifact.data(artifacts.dat, types$quality)

    corr.elements <- gen.conway.artifact.columns(types$quality, keep.list)
    artifacts.dat <- artifacts.dat[, corr.elements$names]
    if (replace.labels) {
        colnames(artifacts.dat) <- corr.elements$labels
    }

    if (nrow(artifacts.dat) == 0) {
        return(NULL)
    }

    return(artifacts.dat)
}

compute.correlations.cycle <- function(conf, i, resdir, types, keep.list,
                                       temporal.type="isochronous") {
    cycles <- get.cycles(conf)

    ## Always remove motif count and anti-motif count (and the
    ## normalised counts) because these quantities are represented
    ## equally well by motif ratio and percent difference as far
    ## as correlations are concerned
    keep.list <- keep.list[!(keep.list %in% c("motif.count", "motif.anti.count",
                                              "motif.count.norm", "motif.anti.count.norm"))]

    artifacts.subset <- query.conway.artifact.data(conf, i, resdir, types, keep.list,
                                                   temporal.type=temporal.type)
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
read.motif.results <- function(conf, resdir, types) {
    cycles <- get.cycles(conf)
    res <- do.call(rbind, lapply(1:nrow(cycles), function(i) {
        motif.file <- file.path(resdir, gen.range.path(i, cycles[i,]$cycle),
                                gen.conway.path(types), "raw_motif_results.txt")

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


## Combine quality (defect, corrections) information with motif/anti-motif counts
## and data obtained from the VCS analysis
merge.conway.data <- function(conf, cycles, i, types, global.resdir,
                              do.variants=FALSE) {
    cycle <- cycles[i, ]
    start.date <- cycle$date.start
    end.date <- cycle$date.end
    params <- get.conway.params()

    motif.dir <- file.path(global.resdir, gen.range.path(i, cycles[i,]$cycle),
                           gen.conway.path(types))
    ## Query required base data
    motif.file <- file.path(motif.dir, "entity_motifs.txt")
    if (!file.exists(motif.file)) {
        loginfo(str_c("Neither motifs nor anti-motifs in cycle ", i, ", exiting early"))
        return()
    }
    compare.motifs <- read.table(motif.file)

    ## Query developer-artifact relationships and determine amout of developers per entity
    vcs.dat <- query.dependency(conf, params$file.limit, start.date, end.date)
    vcs.dat$author <- as.character(vcs.dat$author)
    file.dev.count.df <- ddply(vcs.dat, .(entity),
                               function(df) data.frame(entity=unique(df$entity),
                                                       dev.count=length(unique(df$id))))

    ## Finally, load data about defects/corrective changes.
    ## We consider three types of relationships between social
    ## interactions/communication and bugs/corrections: isochronous
    ## (bugs from the same temporal interval as the communication are
    ## considered), advanced (bugs that appeared in the temporal
    ## interval before the communication are considered), and
    ## retarded (bugs follow communication by one interval)
    relevant.entity.list <- unique(vcs.dat$entity)

    i <- as.numeric(i)
    if (do.variants) {
        ## We cannot compute advanced/retarded combinations if we're considering
        ## the first/last analysis cycle. Prepare a list of combinations without
        ## these corner cases.
        variants <- data.frame(cycle=c(i-1, i, i+1),
                               label=c("advanced", "isochronous", "retarded"))
        variants <- variants[variants$cycle <= nrow(cycles) & variants$cycle>0,]
    } else {
        ## Only consider the current cycle
        variants <- data.frame(cycle=c(i), label=c("isochronous"))
    }

    res <- lapply(1:nrow(variants), function(j) {
        idx <- variants$cycle[j]
        defect.filename <- file.path(global.resdir, gen.range.path(idx, cycles[idx, ]$cycle),
                                     "changes_and_issues_per_file.csv")
        start.date <- cycles[idx, ]$date.start
        end.date <- cycles[idx, ]$date.end

        if (types$quality=="defect") {
            quality.dat <- load.defect.data(defect.filename, relevant.entity.list,
                                            start.date, end.date)
        } else {
            quality.dat <- get.corrective.count(conf$con, conf$pid, start.date,
                                                end.date, types$artifact)
        }


        ## #####################################################################
        ## Combine the previously created data sets and save the result
        artifacts.dat <- merge(quality.dat, compare.motifs, by="entity")
        artifacts.dat <- merge(artifacts.dat, file.dev.count.df, by="entity")

        ## quality_data.csv contains the following columns:
        ## entity -- analysed entity (filename)
        ## BugIssueCount (only for jira data)
        ## Churn (only for jira data)
        ## CountLineCode -- LoC of the entity at the time of the snapshot
        ## motif.count -- number of motifs detected in the entity
        ## motif.anti.count -- number of anti-motifs detected in the entity
        ## dev.count -- developers participating in the development of the entity
        write.csv(artifacts.dat, file.path(motif.dir, str_c("quality_data_",
                                                            variants$label[j], ".csv")))
        return(artifacts.dat)
    })

    names(res) <- variants$label
    return(res)
}
