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
## Copyright 2016 by Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## Copyright 2016 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

s <- suppressPackageStartupMessages
s(library(igraph))

load.jira.edgelist <- function(conf, jira.filename, start.date, end.date) {
    ## Load jira userId to IssueId mapping
    jira.dat <- read.csv(jira.filename, header=TRUE)

    ## Convert jira comment timestampe to a more useful date format
    date <- ldply(jira.dat$CommentTimestamp,
                  function(date.str) {
                      date.str <- as.character(date.str)
                      date.str <- strsplit(date.str, ", ")[[1]][2]
                      res <- as.Date(date.str, "%d %b %Y")
                      return(res)
                  })

    ## Remove rows that are outside the date range
    keep.row <- date$V1 >= start.date & date$V1 <= end.date
    jira.dat <- jira.dat[keep.row, ]

    ## If there are no issues within the desired range, exit early
    if (nrow(jira.dat) == 0) {
        return(NULL)
    }

    ## Map user emails to codeface Ids
    jira.dat$personId <- sapply(jira.dat$userEmail,
                                function(author.email) {
                                    do.normalise(conf, author.email)
                                })

    ## Remove jira ids that could not be mapped to persons known to codeface
    ## (we cannot make socio-techncical statements about such persons).
    ## Only keep the connection between jira issue and person ID in the
    ## data frame.
    jira.dat <- jira.dat[!is.na(jira.dat$personId), ]
    jira.dat <- jira.dat[, c("IssueID", "personId")]

    ## If no person remains, exit early
    if (nrow(jira.dat) == 0) {
        return(NULL)
    }

    ## Perform bipartite projection, that is, create a one-mode network
    ## from the existing two-mode network
    g.bi <- graph.data.frame(jira.dat)
    ## Every vertex represents either a person or a bug; construct the type
    ## attribute to indicate which
    V(g.bi)$type <- V(g.bi)$name %in% jira.dat$personId
    ## Select the vertices for which type==TRUE, that is, all persons
    g <- bipartite.projection(g.bi, which=TRUE)
    edgelist <- as.data.frame(get.edgelist(g))

    ## If no edges (connections between persons) remain, exit early
    if (nrow(edgelist) == 0) {
        return(NULL)
    }
    edgelist$weight <- 1

    return(edgelist)
}
