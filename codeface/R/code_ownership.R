source("db.r")
source("config.r")
source('dependency_analysis.r')

require(igraph)

query.max.owner <- function(con, start.date, end.date) {

  query <- str_c('SELECT name, file, entityId, size ',
                 'FROM commit, commit_dependency d1, person ',
                 'WHERE commit.id = d1.commitId ',
                 'AND person.id = commit.author ',
                 'AND entityId !=', sq('FILE_LEVEL'), ' ',
                 'AND commit.commitDate >= ', sq(start.date), ' ',
                 'AND commit.commitDate < ', sq(end.date), ' ',
                 'AND size=(SELECT max(size) ',
                            'FROM commit_dependency d2 ',
                            'WHERE d1.file = d2.file ',
                            'AND d1.entityId= d2.entityId)')

  dat <- dbGetQuery(con, query)
  cols <- c('file', 'entityId')

  if(nrow(dat) > 0) {
    dat$entity <- apply(dat[,cols], 1, paste, collapse="/")
  }

  dat <- dat[, !names(dat) %in% cols]

  return(dat)
}


run.analysis <- function(project.id, start.date, end.date) {
  if(!exists('dbcon')) {
    con <- connect.db("../../codeface.conf")$con
    dbcon <- TRUE
  }

  ## Get ownership relationships
  owner.df <- query.max.owner(con, start.date, end.date)
  owner.edgelist <- subset(owner.df, select = c(name, entity))
  owner.list <- unique(owner.df$name)
  entity.owner.dic <-split(owner.df$name, owner.df$entity)

  ## Get frequent item set relationships
  learning.span <- ddays(365)
  learning.start.date <- start.date - learning.span
  freq.items <- get.frequent.item.sets(con, project.id, learning.start.date, end.date)
  freq.items.edgelist <- compute.item.sets.edgelist(freq.items)

  ## Remove edges that don't have evolutionary link
  keep.row <- owner.edgelist$entity %in% unique(unlist(freq.items))
  owner.edgelist.trimmed <- owner.edgelist[keep.row,]

  ## Add Evolutionary Links
  keep.row <- apply(freq.items.edgelist, 1,
                    function(x) {
                      all(x %in% owner.edgelist.trimmed$entity)})

  freq.items.edgelist.trimmed <- freq.items.edgelist[keep.row,]
  colnames(owner.edgelist.trimmed) <- c('X1', 'X2')
  owner.evol.edgelist <- rbind(owner.edgelist.trimmed, freq.items.edgelist.trimmed)

  ## Find problem dependencies
  problem <- apply(owner.evol.edgelist, 1,
                    function(r) {
                      if(!r[1] %in% owner.list){
                        dev1 <- entity.owner.dic[r[1]]
                        dev2 <- entity.owner.dic[r[2]]
                        length(union(dev1, dev2)) > 1
                      } else {FALSE}})

  #colnames(owner.evol.edgelist) <- c('From', 'To')
  g <- graph.data.frame(owner.evol.edgelist)
  V(g)$type <- V(g)$name %in% owner.list

  V(g)[!type]$name <- sapply(V(g)[!type]$name, function(x) {
                                   s <- unlist(strsplit(x,'/'))
                                   l <- length(s)
                                   name <- paste(s[l-1], s[l], sep='/')})
  ## Set Entity size
  V(g)[!type]$size <- 2
  ## Set Developer size
  V(g)[type]$size <- 10
  V(g)[type]$color <- 'green'
  E(g)$arrow.size <- 0.25
  E(g)[problem]$color <- 'red'
  E(g)[!problem]$color <- 'black'
  V(g)[!type]$label.cex <- 0.8
  V(g)[!type]$label.dist <- 0.15
  V(g)[type]$label.dist <- 0
  space <- 3
  #l <- layout.fruchterman.reingold(g, niter=500, area=vcount(g)^space,
  #                                 repulserad=vcount(g)^space)
  l <- layout.kamada.kawai(g, niter=1000)

}


start.date <- ymd('2013-01-01')
end.date <- ymd('2014-01-01')

run.analysis(3, start.date, end.date)