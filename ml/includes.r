## Included files and libraries must be collected in this header so that
## we can use the list on cluster nodes
s <- suppressPackageStartupMessages

s(source("local.r"))
s(source("analysis.r"))
s(source("project.spec.r"))
s(source("keyword.list.r"))
s(source.files(snatm.path))

s(library(tm))
s(library(tm.plugin.mail))
s(library(sna))
s(library(ggplot2))
s(library(igraph))
s(library(lsa))
s(library(Rgraphviz)) # See www.bioconductor.org/packages/2.9/bioc/html/Rgraphviz.html
s(library(lubridate))
s(library(xtable))
s(library(reshape))
s(library(parallel))
s(library(plyr))

rm(s)
