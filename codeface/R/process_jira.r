library(igraph)

load.jira.edgelist <- function(conf, jira.filename) {
  ## Load jira userId to IssueId mapping
  jira.dat <- read.csv(jira.filename, header=TRUE)

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
