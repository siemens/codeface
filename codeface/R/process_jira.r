library(igraph)

load.jira.edgelist <- function(jira.filename, codeface.filename) {
  ## Load jira userId to IssueId mapping
  jira.dat <- read.csv(jira.filename, header=TRUE)
  colnames(jira.dat) <- c("IssueId", "userId")

  ## Load file containng jira user id to codeface person ids
  codeface.dat <- read.csv(codeface.filename, header=TRUE)

  ## Merge data frames
  merged.dat <- merge(jira.dat, codeface.dat, by="userId")[, c("IssueId" ,"id")]
  colnames(merged.dat) <- c("IssueId", "personId")

  ## Perform bipartite projection
  g.bi <- graph.data.frame(merged.dat)
  V(g.bi)$type <- V(g.bi)$name %in% merged.dat[,2]
  g <- bipartite.projection(g.bi, which=TRUE)
  edgelist <- as.data.frame(get.edgelist(g))
  edgelist$weight <- 1

  return(edgelist)
}
