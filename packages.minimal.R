# this file installs the minimal set of dependencies, which needs to be installed by source
# (because either not available on apt-get or because we need the latest version)
source("http://bioconductor.org/biocLite.R")
biocLite(c("BiRewire", "graph", "Rgraphviz"))

install.packages(c("statnet","logging", "corrgram"),
                         dependencies=T)
install.packages(c("snatm", "tm-plugin-mail"),
                         repos="http://R-Forge.R-project.org")
devtools::install_github("shiny-gridster", "wch")
