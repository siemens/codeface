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
import argparse
from prosoda.test import hide_output
from prosoda.cli import get_parser
import sys

class TestCLI(unittest.TestCase):
    '''Test that the command line returns a sensible help'''

    def testCLI(self):
        '''Check that the parser deals with common commands'''
        parser = get_parser()
        self.assertIsInstance(parser, argparse.ArgumentParser)
        with hide_output():
            self.assertRaises(SystemExit, parser.parse_args, ["--help"])
        parser.parse_args(["--loglevel", "info", "test"])
