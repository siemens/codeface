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
# Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
# All Rights Reserved.

import unittest
from subprocess import check_call
from tempfile import NamedTemporaryFile
from textwrap import dedent
from os.path import dirname, join as pathjoin
from .example_projects import example_project_func
from prosoda.project import project_analyse, mailinglist_analyse
from prosoda.configuration import Configuration
from prosoda.dbmanager import DBManager

pid_tables = [
    "cluster",
    "commit",
    "freq_subjects",
    "issue",
    "mail_thread",
    "mailing_list",
    "per_cluster_statistics",
    "person",
    "plots",
    "release_range",
    "release_timeline",
    "thread_density",
    "url_info",
]

# TODO: Check if these should be filled
ignore_tables = [
    "issue",
    "per_cluster_statistics",
    "thread_density",
    "url_info",
    "cc_list",
    "commit_communication",
    "issue_comment",
    "issue_dependencies",
    "issue_duplicates",
    "issue_history",
    "plot_bin",
]

other_tables = [
    "author_commit_stats",
    "cc_list",
    "cluster_user_mapping",
    "commit_communication",
    "edgelist",
    "initiate_response",
    "issue_comment",
    "issue_dependencies",
    "issue_duplicates",
    "issue_history",
    "pagerank",
    "pagerank_matrix",
    "plot_bin",
    "project",
    "thread_responses",
    "timeseries",
    "twomode_edgelist",
    "twomode_vertices",
]


class EndToEndTestSetup(unittest.TestCase):
    '''End to end test of a prosoda analysis'''
    add_ignore_tables = []

    def setup_with_p(self, p):
        path = self.p.directory
        self.gitdir = dirname(path)
        self.resdir = pathjoin(path, ".git", "results")
        self.mldir = pathjoin(path, ".git")
        self.project_conf = self.p.prosoda_conf
        self.no_report = False
        self.loglevel = "devinfo"
        self.logfile = pathjoin(path, ".git", "log")
        self.recreate = False
        # This config_file is added in the prosoda test command handler
        self.prosoda_conf = self.config_file
        conf = Configuration.load(self.prosoda_conf, self.project_conf)
        dbm = DBManager(conf)
        for table in pid_tables + other_tables:
            dbm.doExecCommit("DELETE FROM {}".format(table))

    def analyseEndToEnd(self):
        project_analyse(self.resdir, self.gitdir, self.prosoda_conf,
                        self.project_conf, self.no_report, self.loglevel,
                        self.logfile, self.recreate)

    def mlEndToEnd(self):
        mailinglist_analyse(self.resdir, self.mldir, self.prosoda_conf,
                            self.project_conf, self.loglevel, self.logfile,
                            jobs=2)

    def getResults(self):
        conf = Configuration.load(self.prosoda_conf, self.project_conf)
        dbm = DBManager(conf)
        project_id = dbm.getProjectID(conf["project"], self.tagging)
        self.assertGreaterEqual(project_id, 0)
        results = {}
        for table in pid_tables + other_tables + ignore_tables:
            dbm.doExec("SELECT * FROM {table}".format(table=table))
            results[table] = dbm.doFetchAll()
        return results

    def checkResult(self):
        results = self.getResults()
        for table, res in results.iteritems():
            if table in ignore_tables + self.add_ignore_tables:
                if len(res) == 0:
                    print ("Table not filled (expected): ", table)
            else:
                self.assertGreaterEqual(len(res), 1, msg="Table '{}' not filled!".
                                                    format(table))

    def checkClean(self):
        conf = Configuration.load(self.prosoda_conf, self.project_conf)
        dbm = DBManager(conf)
        project_id = dbm.getProjectID(conf["project"], self.tagging)
        dbm.doExecCommit("DELETE FROM project WHERE id={}".format(project_id))
        for table in pid_tables:
            res = dbm.doExec("SELECT * FROM {table} WHERE projectId={pid}".
                             format(table=table, pid=project_id))
            self.assertEqual(res, 0, msg="Table '{}' still dirty!".
                                 format(table))
        for table in other_tables:
            res = dbm.doExec("SELECT * FROM {table}".format(table=table))
            self.assertEqual(res, 0,  msg="Table '{}' still dirty!".format(table))

class TestEndToEnd(object):
    def testEndToEnd(self):
        self.p = example_project_func[self.example_project](self.tagging)
        with self.p:
            self.setup_with_p(self.p)
            self.analyseEndToEnd()
            self.mlEndToEnd()
            self.checkResult()
            self.checkClean()

class TestEndToEndExample1Tag(EndToEndTestSetup, TestEndToEnd):
    example_project = 1
    tagging = "tag"

class TestEndToEndExample1C2A(EndToEndTestSetup, TestEndToEnd):
    example_project = 1
    tagging = "committer2author"

class TestEndToEndExample1Proximity(EndToEndTestSetup, TestEndToEnd):
    example_project = 1
    tagging = "proximity"
    add_ignore_tables = ["edgelist"]


class TestEndToEndExample2Tag(EndToEndTestSetup, TestEndToEnd):
    example_project = 2
    tagging = "tag"
    testEndToEnd = unittest.expectedFailure(TestEndToEnd.testEndToEnd)

class TestEndToEndCaseInsensitivity(EndToEndTestSetup):
    example_project = 1
    tagging = "tag"
    def testCaseInsensitivity(self):
        self.p = example_project_func[self.example_project](self.tagging,
                    randomise_email_case=False)
        with self.p:
            self.setup_with_p(self.p)
            self.analyseEndToEnd()
            self.mlEndToEnd()
            self.checkResult()
            result_normalcase = self.getResults()
            self.checkClean()
        self.p = example_project_func[self.example_project](self.tagging,
                    randomise_email_case=True)
        with self.p:
            self.setup_with_p(self.p)
            self.analyseEndToEnd()
            self.mlEndToEnd()
            self.checkResult()
            result_randcase = self.getResults()
            self.checkClean()
        self.assertEqual(result_normalcase, result_randcase)
