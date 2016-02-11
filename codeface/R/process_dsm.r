library(XLConnect)
library(igraph)

load.dsm.edgelist <- function(dsm.filname) {
  ## Load excel file
  wb <- loadWorkbook(dsm.filename)
  dsm <- readWorksheet(wb, sheet="Sheet0", header=FALSE)

  ## First column is an index and second is the filename 
  dsm.vertices <- dsm[, 2]
  names(dsm.vertices) <- dsm[, 1]
  dsm.vertices <- sapply(dsm.vertices,
                         function(filename) {
                           #filename <- gsub(".", "/", filename, fixed=T)
                           #filename <- gsub("_", ".", filename, fixed=T)
                           return(filename)})

  ## Drop filename column
  dsm <- dsm[, -2]

  ## Assign matrix row/col names
  dsm.matrix <- as.matrix(dsm)
  rownames(dsm.matrix) <- sapply(dsm[, 1],
                                 function(i) dsm.vertices[as.character(i)])
  colnames(dsm.matrix) <- sapply(dsm[1 ,],
                                 function(i) dsm.vertices[as.character(i)])

  ## Remove row/col index
  dsm.matrix <- dsm.matrix[-1, ]
  dsm.matrix <- dsm.matrix[, -1]

  ## Convert string elements to integers
  dsm.matrix[!is.na(dsm.matrix)] <- 1
  dsm.matrix[is.na(dsm.matrix)] <- 0
  diag(dsm.matrix) <- 0

  ## Convert to edge list
  dsm.graph <- graph.adjacency(dsm.matrix, mode="undirected")
  dsm.edgelist <- as.data.frame(get.edgelist(dsm.graph))

  return(dsm.edgelist)
}
