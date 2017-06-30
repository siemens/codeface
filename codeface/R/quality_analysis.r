source("db.r")
s <- suppressPackageStartupMessages
s(library(stringr))
s(library(plyr))
s(library(lubridate))
s(library(hashmap))

get.corrective.count <- function(con, project.id, start.date, end.date,
                                 entity.type) {
    entity.type <- tolower(entity.type)
    if (entity.type=="file") {
        group.by <- "GROUP BY file"
    } else if (entity.type=="function") {
        group.by <- "GROUP BY file, entityId"
    }

    query <- str_c("SELECT file, entityId, entityType,
                  SUM(corrective) as corrective, COUNT(*) as total",
                  "FROM commit_dependency, commit",
                  "WHERE commit.id = commit_dependency.commitId",
                  "AND commit.projectId=", project.id,
                  "AND commit_dependency.entityType='Function'",
                  "AND commit.commitDate >=", sq(start.date),
                  "AND commit.commitDate <", sq(end.date),
                  group.by, sep=" ")

    dat <- dbGetQuery(con, query)
    dat$norm.corrective <- dat$corrective / dat$total

    if(nrow(dat) > 0) {
        if (entity.type == "function") {
            dat$entity <- apply(dat[,cols], 1, paste, collapse="/")
        } else if (entity.type == "file") {
            dat$entity <- dat[, "file"]
        }
    }

    cols <- c('file', 'entityId')
    dat <- dat[, !names(dat) %in% cols]

    return(dat)
}

load.defect.data <- function(filename, relevant.files, start.date, end.date) {
    defect.dat <- read.csv(filename, header=TRUE, stringsAsFactors=FALSE)

    defect.dat$commitDate <- ymd_hms(defect.dat$commitDate)
    start.date <- ymd(start.date)
    end.date <- ymd(end.date)

    defect.dat <- defect.dat[defect.dat$commitDate >= start.date & defect.dat$commitDate < end.date,]

    ## Determine the LoC of each file when it was last modified during the
    ## range. This value is used to represent LoC for the range.
    file.loc.max <- ddply(defect.dat, .(filePath),
                          function(df) {
                              return(df[which.max(df$commitDate),
                                  c("filePath", "CountLineCode")])
                           })

    defect.dat$churn <- defect.dat$linesAdded + defect.dat$linesRemoved
    defect.dat.sub <- defect.dat[, c("filePath", "isBug", "churn")]
    defect.dat.sub[is.na(defect.dat.sub)] <- 0
    ## Sum up the total number of bug commits on each file, and compute the
    ## total size of all changes (line added+removed) on each file during the
    ## release range
    defect.dat.sub <- ddply(defect.dat.sub[, c("filePath", "isBug", "churn")],
                        .(filePath), colwise(sum))

    defect.dat <- merge(defect.dat.sub, file.loc.max, by="filePath")

    defect.dat <- defect.dat[defect.dat$filePath %in% relevant.files, ]
    colnames(defect.dat) <- c("entity", "BugIssueCount", "Churn", "CountLineCode")
    defect.dat$CountLineCode <- as.integer(defect.dat$CountLineCode)

    return(defect.dat)
}

gen.correlation.columns <- function(quality.type, prune=FALSE) {
    ## Generate a list of quantities of interest to compute correlations between
    ## (including a human readable label column)

    ## Map internal names to human-readable labels. Both lists must be in identical order,
    ## of course.
    labels.map <- hashmap(c("motif.ratio", "motif.percent.diff", "motif.percent.diff.sign", "dev.count",
                            "correct", "bug.density", "BugIssueCount", "Churn", "CountLineCode"),
                          c("M/A-M", "MDiff", "MDiff (sign)", "Devs",
                            "Corrective", "BugDens", "Bugs", "Churn", "LoC"))

    ## Standard quantities that are available for all analysis types
    corr.cols <- c("motif.ratio", "motif.percent.diff", "motif.percent.diff.sign", "dev.count")

    ## quality type "defect" produces some additional covariables
    if (quality.type=="defect") {
        corr.cols <- c(corr.cols, c("bug.density", "BugIssueCount", "Churn", "CountLineCode"))
    } else {
        corr.cols <- c(corr.cols, "corrective")
    }

    ## Prune the entries if a simpler (and easier to plot) selection of covariables
    ## is desired
    if (prune) {
        rm.cols <- c("motif.percent.diff", "BugIssueCount", "CountLineCode")
        corr.cols <- corr.cols[!(corr.cols %in% rm.cols)]
    }

    return(list(names=corr.cols, labels=labels.map$find(corr.cols)))
}

cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1

    ## cor.test needs at least three observations. Exit early if there
    ## are not sufficiently many.
    if (length(mat[, 1]) < 3) {
        return(NULL)
    }

    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level,
                            method="spearman")
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            ##lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
            ##uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}
