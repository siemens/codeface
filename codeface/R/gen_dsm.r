#! /usr/bin/env Rscript
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
## Copyright 2016, Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

## Analysis pass that uses understand to obtain architectural information
## in the form of static dependencies

suppressPackageStartupMessages(library(stringr))
source("config.r")
source("db.r")
source("query.r")
source("system.r")
source("utils.r")

do.titan.analysis <- function(conf) {
    ## Create a temporary directory to check out the state of the repository
    ## at the give commit
    code.dir <- tempdir()
    archive.file <- tempfile()

    ## Check out the repository at the final commit state of the revision
    ## range under consideration
    logdevinfo(str_c("Checking out revision ", conf$revhash, " into ",
                     code.dir, "\n"), logger="titan")
    perform.git.checkout(conf$repodir, conf$revhash, code.dir, archive.file)

    ## Understand output and the inferred CSV file to into the range specific directory
    resdir <- conf$resdir
    dir.create(resdir, showWarnings=FALSE, recursive=TRUE)

    languages <- "java c++"

    db.file <- file.path(resdir, "project.udb")
    csv.file <- file.path(resdir, "static_file_dependencies.csv")

    cmd <- str_c("und create -db", db.file, "-languages", languages, sep=" ")
    dummy <- do.system.raw(cmd)
    
    cmd <- str_c("und -db ", db.file, "add", code.dir, sep=" ")
    dummy <- do.system.raw(cmd)
    
    cmd <- str_c("und settings -MetricMetrics all ", db.file)
    dummy <- do.system.raw(cmd)
    
    cmd <- str_c("und settings -MetricFileNameDisplayMode FullPath ", db.file)
    dummy <- do.system.raw(cmd)
    
    cmd <- str_c("und analyze ", db.file)
    dummy <- do.system.raw(cmd)
    
    cmd <- str_c("und export -dependencies file csv", csv.file, db.file, sep=" ")
    dummy <- do.system.raw(cmd)
    
    str_c("und metrics ", db.file)
    dummy <- do.system.raw(cmd)

    ## The temporary files that have been created are all located
    ## in the temporary directory and are therefore implicitely removed
    ## by the unlink call.
    unlink(code.dir, recursive=TRUE)
    unlink(archive.file, recursive=TRUE)
}


## ################# Dispatcher ######################
config.script.run({
  conf <- config.from.args(positional.args=list("repodir", "resdir", "revhash"),
                           require.project=TRUE)

  do.titan.analysis(conf)
})
