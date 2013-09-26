#! /usr/bin/env Rscript

# This file is part of prosoda.  prosoda is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2013 by Siemens AG, Albert Eckert <albert.eckert@siemens.com>

#
# tests
#

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(logging))
suppressPackageStartupMessages(library(RJSONIO))
source("config.r", chdir=TRUE)
source("query.r", chdir=TRUE)
conf <<- config.from.args(require_project=FALSE)
projects.list <<- query.projects(conf$con)


## ALTERNATIVE TESTING: dummy projects.list (for demonstration if no database available)
##projects <- list("4" = "qed", "6" = "linux core", "7" = "github")
##id <- c(4, 6, 7)
##name <- c("qed","linux core", "github")
##projects.list <- data.frame(id, name)

source("shiny/nav/breadcrumb.config.r", chdir=TRUE)
source("shiny/nav/breadcrumb.shiny.r", chdir=TRUE)

cat("TESTS")
cat("=====\n")
cat("\nprojects list from database")
print(projects.list)
bcd <- breadcrumbPanelData("details","projectid=4")
cat("R data structure")
print(bcd)
cat("\nJSON data structure")
cat(toJSON(bcd, pretty = TRUE))
cat("\nBootstrap Html code")
cat(as.character(breadcrumbPanel(bcd)))
#cat("\nBrandville Html code")
#cat(as.character(breadcrumbBrandville(bcd)))

