filter.installed.packages <- function(packageList)  {
    if("-f" %in% commandArgs(trailingOnly = TRUE)) {
        return(packageList)
    } else {
        return(packageList[which(packageList %in% installed.packages()[,1] == FALSE)])
    }
}

p <- filter.installed.packages(c("BiRewire", "graph", "Rgraphviz"))
if(length(p) > 0) {
    source("http://bioconductor.org/biocLite.R")
    biocLite(p)
}

p <- filter.installed.packages(c("statnet", "ggplot2", "tm", "optparse",
                   "igraph", "zoo", "xts", "lubridate", "xtable",
                   "reshape", "wordnet", "stringr", "yaml", "plyr",
                   "scales", "gridExtra", "scales", "RMySQL",
                   "RCurl", "mgcv", "shiny", "dtw", "httpuv", "devtools",
                   "corrgram", "logging", "png", "rjson", "lsa", "RJSONIO"))
if(length(p) > 0) {
    install.packages(p ,dependencies=T)
}


p <- filter.installed.packages(c("snatm", "tm-plugin-mail"))
if(length(p) > 0) {
    install.packages(p, repos="http://R-Forge.R-project.org")
}


p <- filter.installed.packages(c("shinyGridster"))
if(length(p) > 0) {
    devtools::install_github("wch/shiny-gridster")
}
