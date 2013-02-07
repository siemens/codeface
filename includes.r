## Included files and libraries must be collected in this header so that
## we can use the list on cluster nodes
source("local.r")
source("analysis.r")
source("project.spec.r")
source("keyword.list.r")
snatm.path <- "/home/wolfgang/projects/zk/QuantArch/src/snatm/pkg/R"
sourceDir(snatm.path)

library(tm)
library(tm.plugin.mail)
library(sna)
#library(wordnet)
library(ggplot2)
library(igraph)
library(lsa)
library(Rgraphviz) # See www.bioconductor.org/packages/2.9/bioc/html/Rgraphviz.html
library(lubridate)
