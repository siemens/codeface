s <- suppressPackageStartupMessages
s(library(igraph))

load.jira.edgelist <- function(conf, jira.filename, start.date, end.date) {
  ## Load jira userId to IssueId mapping
  jira.dat <- read.csv(jira.filename, header=TRUE)

  ## Cast to date
  date <- ldply(jira.dat$CommentTimestamp,
                function(date.str) {
                  date.str <- as.character(date.str)
                  date.str <- strsplit(date.str, ", ")[[1]][2]
                  res <- as.Date(date.str, "%d %b %Y")
                  return(res)
                })

  ## Remove rows that are outside the date range
  keep.row <- date$V1 >= start.date & date$V1 <= end.date
  jira.dat <- jira.dat[keep.row, ]

  ## Map user emails to codeface Ids
  jira.dat$personId <- sapply(jira.dat$userEmail,
                              function(author.email) {
                                do.normalise(conf, author.email)
                              })

  ## Remove Ids that could not be generated
  jira.dat <- jira.dat[!is.na(jira.dat$personId), ]
  jira.dat <- jira.dat[, c("IssueID", "personId")]

  ## Perform bipartite projection
  g.bi <- graph.data.frame(jira.dat)
  V(g.bi)$type <- V(g.bi)$name %in% jira.dat[,2]
  g <- bipartite.projection(g.bi, which=TRUE)
  edgelist <- as.data.frame(get.edgelist(g))
  edgelist$weight <- 1

  return(edgelist)
}
