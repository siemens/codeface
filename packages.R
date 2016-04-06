filter.installed.packages <- function(packageList)  {
    if("-f" %in% commandArgs(trailingOnly = TRUE)) {
        return(packageList)
    } else {
        return(packageList[which(packageList %in% installed.packages()[,1] == FALSE)])
    }
}

## Remove package from all libraries (i.e., .libPaths())
remove.installed.packages <- function(pack) {
    for (path in .libPaths()) {
        # try to remove package (hard stop() otherwise, if not existing)
        tryCatch({
            remove.packages(pack, path)
            print(paste("removed previously installed package", pack))
        }, error = function(e) {
            # silently ignore errors (the reason would be that a package
            # is not installed)
        })
    }
}

## (re-)install a package from github
reinstall.package.from.github <- function(package, url) {

    ## if package is installed, remove it completely from all libraries
    p <- filter.installed.packages(c(package))
    if(length(p) == 0) {
        remove.installed.packages(package)
    }

    ## Re-install packages
    devtools::install_github(url)
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


## Install following packages from different sources
## and update existing installations, if needed
reinstall.package.from.github("tm.plugin.mail", "wolfgangmauerer/tm-plugin-mail/pkg")
reinstall.package.from.github("snatm", "wolfgangmauerer/snatm/pkg")
reinstall.package.from.github("shinyGridster", "wch/shiny-gridster")
reinstall.package.from.github("shinybootstrap2", "rstudio/shinybootstrap2")
reinstall.package.from.github("Rgraphviz", "mitchell-joblin/Rgraphviz")
