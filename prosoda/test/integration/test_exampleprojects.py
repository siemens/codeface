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
from .example_projects import get_example_project_1
from prosoda.project import project_analyse, mailinglist_analyse

class TestEndToEnd(unittest.TestCase):
    '''End to end test of a prosoda analysis'''

    def executeTest(self, p):
        with p:
            path = p.directory
            gitdir = dirname(path)
            resdir = pathjoin(path, ".git", "results")
            mldir = pathjoin(path, ".git")
            project_conf = p.prosoda_conf
            no_report = False
            loglevel = "devinfo"
            logfile = pathjoin(path, ".git", "log")
            recreate = False
            # This config_file is added in the prosoda test command handler
            prosoda_conf = self.config_file
            print("project_analyse", (resdir, gitdir, prosoda_conf, project_conf, no_report, loglevel, logfile, recreate))
            project_analyse(resdir, gitdir, prosoda_conf, project_conf, no_report, loglevel, logfile, recreate)
            mailinglist_analyse(resdir, mldir, prosoda_conf, project_conf, loglevel, logfile, jobs=2)

    def testExampleProject1(self):
        for tagging in ["tag", "committer2author", "proximity"]:
            p = get_example_project_1(tagging)
            self.executeTest(p)
