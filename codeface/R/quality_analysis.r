source("db.r")
s <- suppressPackageStartupMessages
s(library(stringr))
s(library(plyr))

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

    defect.dat$commitDate <- str_c(defect.dat$committerDate, " ", defect.dat$committeHour)
    defect.dat <- defect.dat[defect.dat$commitDate >= start.date & defect.dat$commitDate < end.date,]

    ## Determine the LoC of each file when it was last modified during the
    ## range. This value is used to represent LoC for the range.
    file.loc.max <- ddply(defect.dat, .(filePath),
                           function(df) {
                               df[which.max(as.Date(df$commitDate)),
                                  c("filePath", "CountLineCode")]
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

    ## "Normalize" filenames to the oddball Titan convention
    defect.dat$entity <- sapply(defect.dat$file,
                                function(filename) {
                                        #filename <- sprintf("src.java.%s_java", filename)
                                    filename <- gsub("/", ".", filename, fixed=T)
                                        #filename <- gsub("_", ".", filename, fixed=T)
                                    return(filename)})

    defect.dat$filePath <- NULL
    defect.dat <- defect.dat[defect.dat$entity %in% relevant.files, ]
    colnames(defect.dat) <- c("BugIssueCount", "Churn", "CountLineCode", "entity")
    defect.dat$CountLineCode <- as.integer(defect.dat$CountLineCode)

    return(defect.dat)
}

gen.correlation.columns <- function(quality.type) {
    ## Generate correlation plot (omitted correlation quantities: BugIsseChurn,
    ## IssueCommits,  motif.count.norm, motif.anti.count.norm, motif.ratio, motif.percent.diff
    corr.cols <- c("motif.ratio", "motif.percent.diff", "dev.count")
    corr.cols.labels <- c("M/A-M", "MDiff", "Devs")

    if (quality.type=="defect") {
        corr.cols <- c(corr.cols, c("bug.density", "BugIssueCount", "Churn", "CountLineCode"))
        corr.cols.labels <- c(corr.cols.labels, c("BugDens", "Bugs", "Churn", "LoC"))
    } else {
        corr.cols <- c(corr.cols, "corrective")
        corr.cols.labels <- c(corr.cols.labels, "Correct")
    }
    return(list(names=corr.cols, labels=corr.cols.labels))
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
