source("db.r")

get.corrective.count <- function(con, project.id, start.date, end.date,
                                 entity.type) {

  query <- str_c("SELECT file, entityId, entityType,
                  SUM(corrective) as corrective, COUNT(*) as total",
                 "FROM commit_dependency, commit",
                 "WHERE commit.id = commit_dependency.commitId",
                 "AND commit.projectId=", project.id,
                 "AND commit_dependency.entityType=", sq(entity.type),
                 "AND commit.commitDate >=", sq(start.date),
                 "AND commit.commitDate <", sq(end.date),
                 "GROUP BY file, entityId", sep=" ")

  dat <- dbGetQuery(con, query)
  dat$norm.corrective <- dat$corrective / dat$total

  cols <- c('file', 'entityId')
  dat$entity <- apply(dat[,cols], 1, paste, collapse="/")
  dat <- dat[, !names(dat) %in% cols]

  return(dat)
}