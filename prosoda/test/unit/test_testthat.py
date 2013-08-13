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
from pkg_resources import resource_filename
from prosoda.util import execute_command

class TestRCode(unittest.TestCase):
    '''Execute R tests as part of the test suite'''

    def testTestThat(self):
        path = resource_filename("prosoda", "R")
        Rcode = 'library(testthat); if (test_dir(".")$n > 0) stop("Some tests failed.")'
        cmd = ["Rscript", "-e", Rcode]
        execute_command(cmd, direct_io=True, cwd=path)
