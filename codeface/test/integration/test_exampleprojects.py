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
    #"issue",
    "mail_thread",
    "mailing_list",
    #"per_cluster_statistics",
    "person",
    "plots",
    "release_range",
    "release_timeline",
    #"thread_density",
    #"url_info",
]

# TODO: Check if these should be filled
ignore_tables = [
    "issue",
    "per_cluster_statistics",
    "thread_density",
    "url_info",
    "cc_list",
    "commit_communication",
    "commit_dependency",
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
    commit_dependency = None

    def clear_tables(self):
        conf = Configuration.load(self.codeface_conf, self.project_conf)
        dbm = DBManager(conf)
        for table in self.result_tables:
            dbm.doExecCommit("DELETE FROM {}".format(table))

    def setup_with_p(self, p):
        path = self.p.directory
        self.gitdir = dirname(path)
        self.resdir = pathjoin(path, ".git", "results")
        self.mldir = pathjoin(path, ".git")
        self.project_conf = self.p.codeface_conf
        self.no_report = False
        self.result_tables = pid_tables + other_tables + ignore_tables
        self.ignore_tables = ignore_tables
        self.add_ignored_tables(self.add_ignore_tables)
        self.loglevel = "devinfo"
        self.logfile = pathjoin(path, ".git", "log")
        self.recreate = False
        # This config_file is added in the codeface test command handler
        if hasattr(self, 'config_file'):
            self.codeface_conf = self.config_file
        else:
            self.codeface_conf = 'codeface.conf'

    def add_ignored_tables(self, tables):
        self.ignore_tables = self.ignore_tables + tables

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
        persons  = dbm.get_project_persons(project_id)
        # Create map from id to name
        person_map = {person[0] : person[1] for person in persons}
        given_correct_edges = self.correct_edges
        if given_correct_edges[0][0] is str:
            # simply check the first range
            given_correct_edges = [self.correct_edges]
        release_ranges = dbm.get_release_ranges(project_id)
        i = -1
        for correct_edges in given_correct_edges:
            i += 1
            release_range = release_ranges[i]
            cluster_id = dbm.get_cluster_id(project_id, release_range)
            edgelist = dbm.get_edgelist(cluster_id)
            # Create edge list with developer names
            test_edges = [[person_map[edge[0]], person_map[edge[1]], edge[2]] for edge in edgelist]
            ## Check number of matches with known correct edges
            match_count = 0
            for test_edge in test_edges:
                if test_edge in correct_edges:
                    match_count += 1
            res = (match_count == len(correct_edges))
            self.assertTrue(
                res,
                msg="Project edgelist is incorrect for the v{}_release "
                    "to v{}_release analysis!"
                .format(i, i+1))
    
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
        for table in self.result_tables:
            dbm.doExec("SELECT * FROM {table}".format(table=table))
            results[table] = dbm.doFetchAll()
        return results

    def check_commit_dependency(self, commit_dependency_data):
        '''
        Checks if the commit_dependency table contains the expected data
        given by self.commit_dependency in the unit test.
        :param commit_dependency_data:
        The data of the actual table:
        | id  | commitId | file | entityId | entityType | size | impl |
        :return:
        '''
        if self.commit_dependency is None:
            return

        conf = Configuration.load(self.codeface_conf, self.project_conf)
        dbm = DBManager(conf)
        project_id = dbm.getProjectID(conf["project"], self.tagging)

        def get_commit_id(commit_hash):
            return dbm.getCommitId(project_id, commit_hash)

        # remove the "id" column
        # so we have (commit_id, file, entityId, type, size, impl) tuples
        data = [(res[1], res[2], res[3], res[4], res[5], res[6])
                for res in commit_dependency_data]
        data_no_impl = [res[0:5] for res in data]

        expected_data = [(get_commit_id(res[0]), res[1], res[2], res[3],
                          res[4], res[5])
                         for res in self.commit_dependency]
        for expected in expected_data:
            if expected[5] is None:
                # don't check the impl
                self.assertIn(expected[0:5], data_no_impl)
            else:
                self.assertIn(expected, data)

        self.assertEqual(len(data), len(expected_data))

    def checkResult(self):
        results = self.getResults()
        for table, res in results.iteritems():
            if table in self.ignore_tables:
                if len(res) == 0:
                    print ("Table not filled (expected): ", table)
            else:
                self.assertGreaterEqual(len(res), 1, msg="Table '{}' not filled!".
                                                    format(table))

        self.check_commit_dependency(
            [res for res in results["commit_dependency"]])

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
    pass
    #def testEndToEnd(self):
    #   self.p = example_project_func[self.example_project](self.tagging)
    #    with self.p:
    #        self.setup_with_p(self.p)
    #        self.clear_tables()
    #        self.analyseEndToEnd()
    #        self.mlEndToEnd()
    #        if (self.correct_edges):
    #          self.checkEdges()
    #        self.checkResult()
    #        self.checkClean()

class TestEndToEndExample1Tag(EndToEndTestSetup, TestEndToEnd):
    example_project = 1
    tagging = "tag"
    correct_edges = None

class TestEndToEndExample1C2A(EndToEndTestSetup, TestEndToEnd):
    example_project = 1
    tagging = "committer2author"
    correct_edges = None

class TestEndToEndExample1Proximity(EndToEndTestSetup, TestEndToEnd):
    ## Example project 1 does not create any opportunity for edges to connect
    ## developers using the proximity tagging approach
    example_project = 1
    tagging = "proximity"
    add_ignore_tables = ["edgelist", "cluster"]
    correct_edges = None

class TestEndToEndExample2Proximity(EndToEndTestSetup, TestEndToEnd):
    example_project = 2
    tagging = "proximity"
    add_ignore_tables = ["edgelist"]
    devs = ["Louie Loner", "Geoff Genius", "Bill Bully", "Max Maintainer",
            "Adam Awkward", "Peter Popular", "Clara Confident"]
    correct_edges = [[[devs[0], devs[5], 1.0],
                      [devs[1], devs[6], 1.0],
                      [devs[2], devs[4], 1.0],
                      [devs[3], devs[2], 1.0],
                      [devs[3], devs[4], 1.0],
                      [devs[4], devs[2], 1.0],
                      [devs[5], devs[0], 1.0]]]

class TestEndToEndExample2Tag(EndToEndTestSetup, TestEndToEnd):
    example_project = 2
    tagging = "tag"
    correct_edges = None
    #testEndToEnd = unittest.expectedFailure(TestEndToEnd.testEndToEnd)

class TestEndToEndCaseInsensitivity(EndToEndTestSetup):
    example_project = 1
    tagging = "tag"
    correct_edges = None
    #def testCaseInsensitivity(self):
    #    self.p = example_project_func[self.example_project](self.tagging,
    #                randomise_email_case=False)
    #    with self.p:
    #        self.setup_with_p(self.p)
    #        self.clear_tables()
    #        self.analyseEndToEnd()
    #        self.mlEndToEnd()
    #        self.checkResult()
    #        result_normalcase = self.getResults()
    #        self.checkClean()
    #    self.p = example_project_func[self.example_project](self.tagging,
    #                randomise_email_case=True)
    #    with self.p:
    #        self.setup_with_p(self.p)
    #        self.clear_tables()
    #        self.analyseEndToEnd()
    #        self.mlEndToEnd()
    #        self.checkResult()
    #        result_randcase = self.getResults()
    #        self.checkClean()
    #    self.assertEqual(result_normalcase, result_randcase)
