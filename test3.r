library(openNLP)
library(tm)
con <- file("repos/gmane.linux.kernel/00055", "r", blocking=FALSE)
txt <- readLines(con)
