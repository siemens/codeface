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
    gitlog.dat <- fread(file.path(range.resdir, "file_metrics.csv"))

    commitFiles <- gitlog.dat[,c("commitHash", "filePath", "description")]

    ## Extract issue IDs
    ## The method should be adapted per project because they use
    ## different conventions to refer to bugs.
    ## Other projects might store this information in the commit description,
    ## so we should also include this information somewhere
    ## TODO: A commit can also refer to multiple issues
    commitFiles$issueId <- str_match(commitFiles$description, '[A-Z]+-[0-9]+')

    ## The data in jira_issue_comments.csv deliver a list of comments on jira
    ## issues, together with authors (id and email) information.
    jiraIssues <- fread(file.path(global.resdir, "jira_issue_comments.csv"))

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
    dt.final <- merge(dt, gitlog.dat, by.x = c("commitHash","filePath", "description"),
                      by.y=c("commitHash","filePath", "description"), all.x=TRUE)

    ## The results are stored for every release range, and contains an entry
    ## for every change to a file in every commit
    ## commitHash: id of the commit introducing the change
    ## filePath: full relative path of the file (e.g., core/sched/deadline.c)
    ## description: Commit subject line
    ## issueId: JIRA issue ID (e.g., HIVE-1937)
    ## IssueType (e.g., Bug, New Feature, ...) NOTE: Braindead upper/lower
    ##                                         case convention for these fields...
    ## isBug: Binary indicator if a change is linked to a bug issue (0/1/NA)
    ## linesAdded,linesRemoved: # of code lines added/removed by the commit (numeric)
    ## CountLineCode: LoC (raw lines of code at the current state)
    ## commitDate, authorDate (format: e.g., 2015-12-22 19:58:02 -0800)
    write.csv(dt.final, file=file.path(range.resdir, "changes_and_issues_per_file.csv"),
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
