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

generate.dsm <- function(conf) {
    ## Create a temporary directory to check out the state of the repository
    ## at the give commit
    code.dir <- tempdir()
    archive.file <- file.path("/tmp/", str_c(conf$revhash, ".tar"))

    ## NOTE: We deliberately don't store the understand db file in resdir,
    ## and deliberately use only a file name in hierarchy depth 1 because
    ## understand is known to cause issues otherwise...
    db.file <- file.path("/tmp/", str_c(conf$revhash, ".udb"))

    ## Check out the repository at the final commit state of the revision
    ## range under consideration
    logdevinfo(str_c("Checking out revision ", conf$revhash, " into ",
                     code.dir, "\n"), logger="titan")
    perform.git.checkout(conf$repodir, conf$revhash, code.dir, archive.file)

    ## Understand output and the inferred CSV file to into the range specific directory
    resdir <- conf$resdir
    dir.create(resdir, showWarnings=FALSE, recursive=TRUE)

    csv.file <- file.path(resdir, "static_file_dependencies.csv")
    xml.file <- file.path(resdir, "static_file_dependencies.xml")

    cmd <- str_c("und create -db", db.file,
                 "-languages Ada COBOL C# Java Pascal Python Web C++ Fortran Jovial Plm VHDL Web",
                 "add", file.path(code.dir, "code"),
                 "settings -MetricMetrics all",
                 "settings -MetricFileNameDisplayMode FullPath",
                 "analyze", sep=" ")
    dummy <- do.system.raw(cmd)

    cmd <- str_c("und export -dependencies file csv", csv.file, db.file, sep=" ")
    dummy <- do.system.raw(cmd)

    cmd <- str_c("und export -dependencies file cytoscape ", xml.file, db.file, sep=" ")
    dummy <- do.system.raw(cmd)
    
    ## The temporary files that have been created are all located
    ## in the temporary directory and are therefore implicitely removed
    ## by the unlink call.
    unlink(code.dir, recursive=TRUE)
    unlink(archive.file, recursive=TRUE)
    unlink(db.file, recursive=TRUE)
}


## ################# Dispatcher ######################
config.script.run({
  conf <- config.from.args(positional.args=list("repodir", "resdir", "revhash"),
                           require.project=TRUE)

  generate.dsm(conf)
})
