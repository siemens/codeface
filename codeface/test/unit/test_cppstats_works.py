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
# Copyright 2014, by Matthias Dittrich <matthi.d@gmail.com>
# All Rights Reserved.
__author__ = 'drag0on'

import unittest

from codeface.VCS import (get_feature_lines, parse_feature_line,
                          parse_line, parse_sep_line, ParseError, LineType,
                          get_feature_lines_from_file)

from operator import eq
import logging
logging.basicConfig()


class TestCppStatsWorks(unittest.TestCase):
    """Tests for the getFeatureLines function"""
    def _get_file_layout(self, file):
        splits = file.split("\n")
        d = list()
        for split in splits:
            d.append(split + '\n')
        # remove the last \n
        d[-1] = d[-1][0:-1]
        return d
    def test_file_layout(self):
        file="""
#if Test
// example
#else
// more
#endif
        """
        d = self._get_file_layout(file)
        file_new = ""
        for line in d:
            file_new += line
        self.assertEqual(file, file_new)

    def test_simple_analysis(self):
        """
        This test checks if cppstats is working as expected.
        When this test fails it is possible that cppstats doesn't have
        a working srcML binary. You can find the binaries on
        http://sdml.info/lmcrs/ . Just copy the right binary to
        cppstats/lib/srcml/{win|linux|darwin} and replace the
        existing ones, then run this test again.
        """
        file="""
#if Test
// example
#else
// more
#endif
        """
        d = self._get_file_layout(file)
        feature_dict = get_feature_lines_from_file(d, "unittest.c")

        self.assertSetEqual(feature_dict.get_line_info(1), set([]))
        self.assertSetEqual(feature_dict.get_line_info(2), set(["Test"]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["Test"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["Test"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["Test"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set(["Test"]))
        self.assertSetEqual(feature_dict.get_line_info(7), set([]))
        pass