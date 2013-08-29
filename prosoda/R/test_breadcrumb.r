#! /usr/bin/env Rscript

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
bcd <- breadcrumbPanelData("contributors","projectid=4")
cat("R data structure")
print(bcd)
cat("\nJSON data structure")
cat(toJSON(bcd, pretty = TRUE))
cat("\nBootstrap Html code")
cat(as.character(breadcrumbPanel(bcd)))
#cat("\nBrandville Html code")
#cat(as.character(breadcrumbBrandville(bcd)))

