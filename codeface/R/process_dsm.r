library(XLConnect)
library(readxl)
library(igraph)

load.dsm.edgelist <- function(dsm.filname, relavent.files) {
  ## Load excel file
  dsm <- read_excel(dsm.filename, sheet="Sheet0", col_names=FALSE)
  dsm[1,] <- as.integer(dsm[1,])
  #wb <- loadWorkbook(dsm.filename)
  #dsm <- readWorksheet(wb, sheet="Sheet0", header=F)

  ## First column is an index and second is the filename
  file.col.num <- 2
  dsm.vertices <- dsm[, file.col.num]
  names(dsm.vertices) <- dsm[, 1]
  dsm.vertices <- sapply(dsm.vertices,
                         function(filename) {
                           #filename <- gsub(".", "/", filename, fixed=T)
                           filename <- gsub("_", ".", filename, fixed=T)
                           return(filename)})

  ## Remove files that have not been edited
  keep.file <- dsm.vertices %in% relavent.files
  keep.file[1] <- TRUE # keep index row
  dsm <- dsm[c(keep.file), ]
  dsm <- dsm[, c(T, keep.file)]

  ## Drop filename column
  dsm <- dsm[, -file.col.num]

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
