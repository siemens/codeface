source("db.r")

get.corrective.count <- function(con, project.id, start.date, end.date,
                                 entity.type) {
  entity.type <- tolower(entity.type)
  if (entity.type=="file") {
    entity.type.db <- "function"
    group.by <- "GROUP BY file"
  } else if (entity.type=="function") {
    group.by <- "GROUP BY file, entityId"
  }

  query <- str_c("SELECT file, entityId, entityType,
                  SUM(corrective) as corrective, COUNT(*) as total",
                 "FROM commit_dependency, commit",
                 "WHERE commit.id = commit_dependency.commitId",
                 "AND commit.projectId=", project.id,
                 "AND commit_dependency.entityType=", sq(entity.type.db),
                 "AND commit.commitDate >=", sq(start.date),
                 "AND commit.commitDate <", sq(end.date),
                 "GROUP BY file, entityId", sep=" ")

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

load.defect.data <- function(filename, relavent.files, start.date, end.date) {
  defect.dat <- read.csv(filename, header=TRUE, stringsAsFactors=FALSE)

  ## Check if time resolved or not
  if ("commitDate" %in% names(defect.dat)) {
    defect.dat <- subset(defect.dat, commitDate >= start.date &
                                     commitDate < end.date)
#    file.size <- ddply(defect.dat, .(file),
#                       function(df) {
#                         df[which.max(as.Date(df$commitDate)),
#                                      c("file", "fileSize")]
#                       })

#    colnames(file.size) <- c("file", "CountLineCode")
#    defect.dat$churn <- defect.dat$linesAdded + defect.dat$linesRemoved
    defect.dat <- defect.dat[, c("file", "isBug")]#, "churn")]
    defect.dat[is.na(defect.dat)] <- 0
    defect.dat <- ddply(defect.dat[, c("file", "isBug")], #"churn")],
                        .(file), colwise(sum))

    #defect.dat <- merge(defect.dat, file.size, by="file")
  }

  ## Normalize filenames
  defect.dat$entity <- sapply(defect.dat$file,
      function(filename) {
        #filename <- sprintf("src.java.%s_java", filename)
        #filename <- gsub("/", ".", filename, fixed=T)
        #filename <- gsub("_", ".", filename, fixed=T)
        return(filename)})

  defect.dat$file <- NULL
  defect.dat <- defect.dat[defect.dat$entity %in% relavent.files, ]
  colnames(defect.dat) <- c("BugIssueCount", "entity")# "Churn", "CountLineCode", "entity")
  #defect.dat$CountLineCode <- as.integer(defect.dat$CountLineCode)

  return(defect.dat)
}
