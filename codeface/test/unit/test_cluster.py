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
# Copyright 2014 by Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
# All Rights Reserved.

import unittest
from tempfile import gettempdir
import codeface.cluster.cluster as cluster
import codeface.fileCommit as fileCommit
import codeface.commit as commit

class TestCluster(unittest.TestCase):
    '''Test logical dependency functions'''
    # TODO This only tests ctag-style function index, not Doxygen style
    def setUp(self):
        '''
        Constructs a dictionary of fileCommit objects for testing purposes
        '''
        file_commit = fileCommit.FileCommit()
        file_commit.filename = "test_file.cpp"
        fileSnapShot = {'1':'commit1', '2':'commit2',
                        '3':'commit3', '4':'commit2',
                        '5':'commit1', '6':'commit3'}
        functionIds = {1:'function1', 2:'function2',
                       3:'function3', 4:'function4',
                       5:'function5'}
        file_commit.addFileSnapShot(4, fileSnapShot)
        file_commit.setFunctionLines(functionIds)
        self.file_commits = {0:file_commit}


    def test_computeLogicalDepends(self):
        '''
        Tests that the logical dependencies are correctly computed from the
        commit data
        '''
        fileCommit_dic = self.file_commits
        date = "01-01-2000"
        cmt_dict = {'commit1': commit.Commit(), 'commit2': commit.Commit(),
                    'commit3': commit.Commit()}
        for cmt_id in cmt_dict:
          cmt_dict[cmt_id].setCdate(date)

        computedLDs = cluster.computeLogicalDepends(fileCommit_dic, cmt_dict,
                                                    date)

        correctLDs = {'commit2': [(('test_file.cpp', 'function2'), 1),
                                  (('test_file.cpp', 'function4'), 1)],
                      'commit3': [(('test_file.cpp', 'function3'), 1),
                                  (('test_file.cpp', 'function5'), 1)],
                      'commit1': [(('test_file.cpp', 'function1'), 1),
                                  (('test_file.cpp', 'function5'), 1)]}

        result = (computedLDs == correctLDs)
        msg = 'Computation for logical dependencies is broken'
        self.assertTrue(result, msg)
