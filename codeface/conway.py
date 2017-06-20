#! /usr/bin/env python

# This file is part of Codeface. Codeface is free software: you can
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
# Copyright 2016 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
# Copyright 2016 by Carlos Andrade <carlos.andrade@acm.org>
# All Rights Reserved.

# Data gathering and preparation steps for Conway analysis

import yaml
import os.path
import argparse
import codecs
import time
import csv
import sys
from datetime import datetime
from logging import getLogger; log = getLogger(__name__)
from progressbar import ProgressBar, Percentage, Bar, ETA

from .VCS import gitVCS
from .dbmanager import DBManager
from .util import execute_command
from jira import JIRA
from os import listdir

import xml.etree.cElementTree as ET
import jira as jr
import pandas as pd
import numpy as np

# Given a user id and a jira instance, determine the email address associated with the
# user id.
def get_email_from_jira(userid, jira):
    '''Given an authenticated jira instance and an userid on JIRA, returns (user_id, user_email)'''
    def fix_email_format(email_string):
        '''Fix e-mail formatting defined by Apache Jira'''
        email = user.emailAddress.split()
        for i in range(len(email)):
            if email[i] == 'dot':
                email[i] = '.'
            elif email[i] == 'at':
                email[i] = '@'
        email = ''.join(email)
        return email
    user = jira.user(id=userid,expand=["name","emailAddress"])
    user_data = (user.name,fix_email_format(user.emailAddress))
    return user_data


def parse_jira_issues(xmldir, resdir, jira_url, jira_user, jira_password):
    file_names = [ f for f in listdir(xmldir) if os.path.isfile(os.path.join(xmldir, f)) ]

    # Iterate over all Jira XML issue files as obtained by Titan, and obtain
    # author information for each issue
    issue_list=[]
    for file_name in file_names:
        try:
            tree = ET.ElementTree(file=os.path.join(xmldir, file_name))
        except ET.ParseError:
            continue
        root = tree.getroot()
        for channel in root:
            for channel_element in channel:
                if channel_element.tag == "item":
                    issue_elements = channel_element.getchildren()
                    issue_key = None
                    issue_type = None
                    for issue in issue_elements:
                        if issue.tag == "key":
                            issue_key = issue.text
                        if issue.tag == "type":
                            issue_type = issue.text
                        if issue.tag  == "comments":
                             for comment in issue:
                                 issue_comment_author = None
                                 issue_comment_author = comment.get('author').encode('utf-8')
                                 issue_comment_timestamp = comment.get('created')
                                 issue={'IssueID': issue_key, 'IssueType': issue_type,
                                       'AuthorID': issue_comment_author,
                                       'CommentTimestamp': issue_comment_timestamp }
                                 issue_list.append(issue)

    comment_authors_df = pd.DataFrame(issue_list,
                                      columns=("IssueID", "IssueType",
                                               "AuthorID", "CommentTimestamp"))
    user_ids = comment_authors_df['AuthorID'].unique()
    jira = JIRA(server=jira_url, basic_auth=(jira_user, jira_password))

    total = len(user_ids)

    widgets = ['Parsing jira issues: ', Percentage(), ' ', Bar(), ' ', ETA()]
    pbar = ProgressBar(widgets=widgets, maxval=total).start()
    email_list = []

    for i, userid in enumerate(user_ids):
        requested_tuple = None
        try:
            requested_tuple = get_email_from_jira(userid, jira)
        except jr.exceptions.JIRAError:
            log.devinfo('User ID {} not found'.format(userid))
        except UnicodeDecodeError:
            log.devinfo('Unicode Decoding problem, most likely due to faulty encoding. ' +
                  'Ignoring user ID {}.'.format(userid))
        if requested_tuple == None:
            pass
        else:
            email_list.append(np.array(requested_tuple))

        pbar.update(i)

    # Add a new column with the inferred email addresses to the data frame
    email_df = pd.DataFrame(email_list, columns=('AuthorID', 'userEmail'))
    merged = pd.merge(comment_authors_df, email_df, on='AuthorID')

    # ... and store the results as CSV file (TODO: Place this in the codeface DB)
    # Contains the columns
    # IssueID (e.g., HIVE-1937)
    # IssueType (e.g., Bug, New Feature, ...)
    # AuthorID (alphanumeric jira id, e.g., cwstein)
    # CommentTimestamp (format: Tue, 1 Feb 2011 01:47:11 +0000)
    # userEmail (pure address without name, e.g., abc@apache.org)
    merged.to_csv(os.path.join(resdir, "jira_issue_comments.csv"), index=False)


def dispatch_jira_processing(resdir, titandir, conf):
    xmldir = os.path.join(resdir, "issues_xml")
    dbm = DBManager(conf)

    dbm = DBManager(conf)
    projectID = dbm.getProjectID(conf["project"], conf["tagging"])
    (date_start, date_end) = dbm.getProjectTimeRange(projectID)

    if (os.path.exists(xmldir)):
        log.info("Jira issues already present in directory {}, "\
                 "skipping download".format(xmldir))
    else:
        try:
            os.makedirs(xmldir)
        except os.error as e:
            log.exception("Could not create output dir {0}: {1}".
                    format(xmldir, e.strerror))
            raise

        log.info("Downloading JIRA issues from {} to {} into directory {}".
                 format(date_start, date_end, xmldir))
        cmd = []
        cmd.append("java")
        cmd.extend(("-jar", "{}/downloadJiraIssues.jar".format(titandir)))
        cmd.append(conf["issueTrackerProject"])
        cmd.append(date_start)
        cmd.append(date_end)
        cmd.append(xmldir)
        # TODO: This raises an error when $DISPLAY is set since it tries to open a GUI windo
        execute_command(cmd)

    if (os.path.isfile(os.path.join(resdir, "jira_issue_comments.csv"))):
        log.info("Jira result file already exists, skipping generation")
    else:
        parse_jira_issues(xmldir, resdir, conf["issueTrackerURL"], conf["issueTrackerUser"],
                          conf["issueTrackerPassword"])
    return

def getLoC(repo, gitHash, filePath):
    cmd_git = "git --git-dir={0} show {1}:{2}".format(repo, gitHash, filePath).split()
    try:
        lines = execute_command(cmd_git, silent_errors=True).splitlines()
    except:
        ## When the git query fails, assume that the file is size 0 (this happens
        ## when a commit deletes a file). We don't check for other errors because
        ## if there are other issues with git, these would have already been caught
        ## in a previous pass.
        return(0)

    return(len(lines))

def prepare_base(l, commit_map):
    line = l.split()

    commitHash = line[0]
    commitDate = " ".join(line[1:4])
    authorDate = " " .join(line[4:7])
    description = commit_map[commitHash]

    # Prepare output line for the files associated to the commit
    commit = [None, None, None, None, commitHash,
              commitDate, authorDate, description]
    return(commit)

def parseGitLogOutput(dat, dat_hashes, repo, outfile):
    commitFileLOC = []
    fileSizeHash = {} # Store current LoC after each commit, and identify if it is
                      # the first ocurrence of the file (when there is no hash entry yet)
    commit = None

    # Create a hash table that maps commits hashes to commit subjects
    commit_map = dict([(h, subject) for (h, subject) in [line.split(' ', 1) for line in dat_hashes]])

    for change_block in dat:
        change_lines = change_block.splitlines()

        # Line 0 has format
        # "97e131d5a2d4140fec02aa3a05b5554b6fc289f4 2016-04-15 11:50:40 +0900 2016-04-15 11:50:40 +0900"
        commit = prepare_base(change_lines[0], commit_map)

        # All other lines have format
        # <added> <deleted> <filename>, for instance
        #"3	13	path/to/file.java"
        for change in change_lines[1:]:
            if change.count('\t') != 2:
                commit = prepare_base(change, commit_map)
                continue

            line = change.split()

            linesAdded = line[0]
            linesRemoved = line[1]
            filePath = " ".join(line[2:]) # Note: line[2] is not sufficient because the file name
                                          # can contain spaces

            # If file can't be parsed by git (usually binary files), a
            # "-" is used as prefix. We skip these files.
            if linesAdded == "-":
                continue

            # Complete output line for the files associated to the commit
            commit[0] = filePath
            commit[1] = linesAdded
            commit[2] = linesRemoved

            # If first ocurrence of file, then determine LoC of the file
            # by inspecting the raw content
            if filePath not in fileSizeHash:
                loc = getLoC(repo, commit[4], filePath)
                fileSizeHash[filePath] = loc
                commit[3] = loc
            else:
                # We need to just linesAdded and linesRemoved to know the
                # file LOC after the commit
                fileSizeHash[filePath] = str(int(fileSizeHash[filePath]) +
                                         int(linesAdded) - int(linesRemoved))
                commit[3] = fileSizeHash[filePath]

            commitFileLOC.append(tuple(commit))

    log.devinfo("Storing git log output in {}".format(outfile))
    with open(outfile, 'w') as out:
        csv_out = csv.writer(out)
        csv_out.writerow(['filePath', 'linesAdded', 'linesRemoved',
                          'CountLineCode', 'commitHash', 'commitDate',
                          'authorDate', 'description'])
        for row in commitFileLOC:
            csv_out.writerow(row)

def createFileDevTable(dbm, project_id, range_id, outfile):
    dat = dbm.get_file_dev(project_id, range_id)

    with open(outfile, 'w') as out:
        csv_out = csv.writer(out, delimiter="\t")
        csv_out.writerow(['id', 'commitHash', 'commitDate', 'author', 'description',
                          'file', 'commitId', 'fileSize'])
        for row in dat:
            csv_out.writerow(row)

def parseCommitLoC(conf, dbm, project_id, range_id, start_rev, end_rev, outdir, repo):
    """Given a release range by its boundaries, compute the amount
    of changes for each file"""
    if not os.path.exists(outdir):
        try:
            os.makedirs(outdir)
        except os.error as e:
            log.exception("Could not create output dir {0}: {1}".
                    format(outdir, e.strerror))
            raise

    cmd_git = "git --git-dir={0} log --numstat --reverse --no-merges ".format(repo).split()
    cmd_git.append("--pretty=format:%H% ci %ai")
    cmd_git.append("{0}..{1}".format(start_rev, end_rev))
    change_blocks = execute_command(cmd_git).split("\n\n")

    cmd_git = "git --git-dir={0} log --reverse --no-merges ".format(repo).split()
    cmd_git.append("--pretty=format:%H %s")
    cmd_git.append("{0}..{1}".format(start_rev, end_rev))
    hash_description = execute_command(cmd_git).splitlines()

    # file_metrics.csv contains information about changes to individual files
    # over time. One line describes the action of one commit acting on the file.
    # It contains the following columns:
    # filePath: full relative path of the file (e.g., core/sched/deadline.c)
    # linesAdded,linesRemoved: # of code lines added/removed by the commit, numeric
    # CountLineCode: LoC (raw lines of code at the current state)
    # commitHash: id of the commit introducing the change
    # commitDate, authorDate (format: e.g., 2015-12-22 19:58:02 -0800)
    # description: Commit subject line
    # The file is created for each release range
    parseGitLogOutput(change_blocks, hash_description, repo,
                      os.path.join(outdir, "file_metrics.csv"))

if __name__ == "__main__":
    # NOTE: When the script is executed manually via command line, we
    # assume that the issues are already present as XML files.
    # We just perform the postprocessing step in this case
    if len(sys.argv) < 5:
        sys.exit("Usage: %s jira-bug-xml-dir jira-user jira-password " \
                 "jira_url output-dir" % sys.argv[0])

    xml_dir = sys.argv[1]
    jira_user = sys.argv[2]
    jira_password = sys.argv[3]
    jira_url = sys.argv[4]
    output_dir = sys.argv[5]

    parse_jira_issues(xml_dir, output_dir, jira_url, jira_user, jira_password)
