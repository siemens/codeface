## This file is part of Codeface. Codeface is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2014 by Roger Meier <roger@bufferoverflow.ch>
## Copyright 2015 by Andreas Ringlstetter <andreas.ringlstetter@gmail.com>
## Copyright 2015 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## Copyright 2015 by Claus Hunsen <hunsen@fim.uni-passau.de>
## All Rights Reserved.

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
    devtools::install_github(url, quiet=T)
}

p <- filter.installed.packages(c("BiRewire", "graph"))
if(length(p) > 0) {
    source("http://bioconductor.org/biocLite.R")
    biocLite(p)
}

p <- filter.installed.packages(c("statnet", "tm", "optparse", "arules", "data.table", "plyr",
                                 "igraph", "zoo", "xts", "lubridate", "xtable", "ggplot2",
                                 "reshape", "wordnet", "stringr", "yaml", "ineq",
                                 "scales", "gridExtra", "scales", "RMySQL", "svglite",
                                 "RCurl", "mgcv", "shiny", "dtw", "httpuv", "devtools",
                                 "corrgram", "logging", "png", "rjson", "lsa", "RJSONIO"))
if(length(p) > 0) {
    install.packages(p, dependencies=T, verbose=F, quiet=T)
}

## Install following packages from different sources
## and update existing installations, if needed
reinstall.package.from.github("tm.plugin.mail", "wolfgangmauerer/tm-plugin-mail/pkg")
reinstall.package.from.github("snatm", "wolfgangmauerer/snatm/pkg")
reinstall.package.from.github("shinyGridster", "wch/shiny-gridster")
reinstall.package.from.github("shinybootstrap2", "rstudio/shinybootstrap2")
reinstall.package.from.github("Rgraphviz", "mitchell-joblin/Rgraphviz")
