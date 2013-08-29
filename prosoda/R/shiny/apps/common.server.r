#! /usr/bin/env Rscript

## This file is part of prosoda.  prosoda is free software: you can
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
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

s <- suppressPackageStartupMessages
s(library(shiny))
s(library(igraph))
s(library(logging))
s(library(corrgram))
s(library(ggplot2))
rm(s)
source("../../config.r", chdir=TRUE)
source("../../utils.r", chdir=TRUE)
source("../../query.r", chdir=TRUE)
source("../../commits.r", chdir=TRUE)
source("../../vis.ports.r", chdir=TRUE)

## Global variables
conf <- config.from.args(require_project=FALSE)
projects.list <- query.projects(conf$con)

## breadcrumb
source("../nav/breadcrumb.shiny.r", chdir = TRUE)
