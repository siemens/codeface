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

load.defect.data <- function(filename) {
  defect.dat <- read.csv(filename, header=TRUE)

  ## Normalize filenames
  defect.dat$entity <- sapply(defect.dat$Filename,
      function(filename) {
        filename <- sprintf("src.java.%s_java", filename)
        filename <- gsub(".", "/", filename, fixed=T)
        filename <- gsub("_", ".", filename, fixed=T)
        return(filename)})

  return(defect.dat)
}
