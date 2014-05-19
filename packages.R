source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

install.packages(c("statnet", "ggplot2", "tm", "tm.plugin.mail", "optparse",
                   "igraph", "zoo", "xts", "lubridate", "xtable",
                   "reshape", "wordnet", "stringr", "yaml", "plyr",
                   "scales", "gridExtra", "scales", "RMySQL",
                   "RCurl", "mgcv", "shiny", "dtw", "httpuv", "devtools",
                   "corrgram", "logging", "png", "rjson", "lsa"),
                         dependencies=T)
install.packages("snatm", repos="http://R-Forge.R-project.org")
devtools::install_github("shiny-gridster", "wch")
