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
## Copyright 2016 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## Copyright 2016 by Carlos Andrade <carlos.andrade@acm.org>
## All Rights Reserved.

## Creates a data frame that connects files with jira issues and
## churn information.

s <- suppressPackageStartupMessages
s(library(data.table))
s(library(stringr))
source("query.r")
source("config.r")

do.generate.conway.metrics <- function(global.resdir, range.resdir) {
    commitFiles <- fread(file.path(range.resdir, "file_dev.csv"))

    ## Extract issue IDs
    commitFiles$issueId <- str_match(commitFiles$description,'[A-Z]+-[0-9]+')

    ## The data in jira-comment-authors-with-email.csv deliver a list of jira
    ## issued for the project, and a connection between authors (ids and emails
    ## addresses) and issues.
    jiraIssues <- fread(file.path(global.resdir, "jira-comment-authors-with-email.csv"))

    ## Group table to only contain the issues. We don't need the comments to identify
    ## the issue type.
    jiraIssues <- unique(jiraIssues[,.(IssueID,IssueType)])

    ## Connect files with jira issues.
    ## Notice this assigns NA when there was no issue ID on
    ## the commit since the type cannot be inferred without an issue id.
    dt <- merge(commitFiles, jiraIssues, by.x = "issueId", by.y="IssueID", all.x=TRUE)

    ## Create a column with value 1 if the issue is of type bug, and
    ## 0 otherwise. If issue type is NA, it will also have NA here.
    dt$isBug <- ifelse(dt$IssueType=="Bug", 1, 0)

    ## Combine the data with information from the git log, especially lines added/removed.
    gitlog.dat <- fread(file.path(range.resdir, "file_metrics.csv"))
    dt.final <- merge(dt, gitlog.dat, by.x = c("commitHash","file"),
                      by.y=c("commitHash","filePath"), all.x=TRUE)
    write.csv(dt.final, file=file.path(range.resdir, "time_based_metrics.csv"),
              row.names=FALSE)
}

config.script.run({
    conf <- config.from.args(positional.args=list("project_resdir", "range_resdir"),
                             require.project=TRUE)
    if (conf$profile) {
        ## R cannot store line number profiling information before version 3.
        if (R.Version()$major >= 3) {
            Rprof(filename="ts.rprof", line.profiling=TRUE)
        } else {
            Rprof(filename="ts.rprof")
        }
    }

    do.generate.conway.metrics(conf$project_resdir, conf$range_resdir)
})
