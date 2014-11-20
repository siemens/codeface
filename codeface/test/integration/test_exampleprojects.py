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
# Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>
# All Rights Reserved.

import unittest
import sys
from subprocess import check_call
from tempfile import NamedTemporaryFile
from textwrap import dedent
from os.path import dirname, join as pathjoin
from pkg_resources import load_entry_point

from codeface.logger import set_log_level, start_logfile, log
from .example_projects import example_project_func
from codeface.project import project_analyse, mailinglist_analyse
from codeface.configuration import Configuration
from codeface.dbmanager import DBManager

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
    '''End to end test of a codeface analysis'''
    add_ignore_tables = []

    def setup_with_p(self, p):
        path = self.p.directory
        self.gitdir = dirname(path)
        self.resdir = pathjoin(path, ".git", "results")
        self.mldir = pathjoin(path, ".git")
        self.project_conf = self.p.codeface_conf
        self.no_report = False
        self.loglevel = "devinfo"
        self.logfile = pathjoin(path, ".git", "log")
        self.recreate = False
        # This config_file is added in the codeface test command handler
        if hasattr(self, 'config_file'):
            self.codeface_conf = self.config_file
        else:
            self.codeface_conf = 'codeface_testing.conf'
        conf = Configuration.load(self.codeface_conf, self.project_conf)
        dbm = DBManager(conf)
        for table in pid_tables + other_tables:
            dbm.doExecCommit("DELETE FROM {}".format(table))

    def analyseEndToEnd(self):
        save_argv = sys.argv
        sys.argv = ['codeface', '-l', self.loglevel, '-f', self.logfile,
                    'run', '-c', self.codeface_conf, '-p', self.project_conf,
                    self.resdir, self.gitdir]
        try:
            load_entry_point('codeface', 'console_scripts', 'codeface')()
        finally:
            sys.argv = save_argv

    # Check that the network is correct for the given test project
    # this needs to be specific to the tagging method since each method
    # calculates edges and weight differently
    def checkEdges(self):
        conf = Configuration.load(self.codeface_conf, self.project_conf)
        dbm = DBManager(conf)
        project_id = dbm.getProjectID(conf["project"], self.tagging)
        cluster_id = dbm.get_cluster_id(project_id)
        edgelist = dbm.get_edgelist(cluster_id)
        persons  = dbm.get_project_persons(project_id)
        # Create map from id to name
        person_map = {person[0] : person[1] for person in persons}
        # Create edge list with developer names
        test_edges = [[person_map[edge[0]], person_map[edge[1]], edge[2]] for edge in edgelist]
        ## Check number of matches with known correct edges
        match_count = 0
        for test_edge in test_edges:
            if test_edge in self.correct_edges:
                match_count += 1
        res = (match_count == len(self.correct_edges))
        self.assertTrue(res, msg="Project edgelist is incorrect!")
    
    def mlEndToEnd(self):
        save_argv = sys.argv
        sys.argv = ['codeface', '-l', self.loglevel, '-f', self.logfile,
                    'ml', '-c', self.codeface_conf, '-p', self.project_conf,
                    self.resdir, self.mldir]
        try:
            load_entry_point('codeface', 'console_scripts', 'codeface')()
        finally:
            sys.argv = save_argv

    def getResults(self):
        conf = Configuration.load(self.codeface_conf, self.project_conf)
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
        conf = Configuration.load(self.codeface_conf, self.project_conf)
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
            if (self.correct_edges):
              self.checkEdges()
            self.checkResult()
            self.checkClean()

class TestEndToEndExample1Tag(EndToEndTestSetup, TestEndToEnd):
    example_project = 1
    tagging = "tag"
    correct_edges = None

class TestEndToEndExample1C2A(EndToEndTestSetup, TestEndToEnd):
    example_project = 1
    tagging = "committer2author"
    correct_edges = None

class TestEndToEndExample1Proximity(EndToEndTestSetup, TestEndToEnd):
    example_project = 1
    tagging = "proximity"
    add_ignore_tables = ["edgelist"]
    correct_edges = None

class TestEndToEndExample2Proximity(EndToEndTestSetup, TestEndToEnd):
    example_project = 2
    tagging = "proximity"
    add_ignore_tables = ["edgelist"]
    devs = ["Louie Loner", "Geoff Genius", "Bill Bully", "Max Maintainer",
            "Adam Awkward", "Peter Popular", "Clara Confident"]
    correct_edges = [[devs[0], devs[5], 4.0],
                     [devs[1], devs[6], 5.0],
                     [devs[2], devs[4], 4.0],
                     [devs[3], devs[2], 9.0],
                     [devs[3], devs[4], 24.0],
                     [devs[4], devs[2], 2.0],
                     [devs[5], devs[0], 12.0]]

class TestEndToEndExample2Tag(EndToEndTestSetup, TestEndToEnd):
    example_project = 2
    tagging = "tag"
    correct_edges = None
    testEndToEnd = unittest.expectedFailure(TestEndToEnd.testEndToEnd)

class TestEndToEndCaseInsensitivity(EndToEndTestSetup):
    example_project = 1
    tagging = "tag"
    correct_edges = None
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
