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

import unittest

from codeface.VCS import (getFeatureLines, parseFeatureLine, parseline, parseSepLine, ParseError, LineType)
from operator import eq
import logging
logging.basicConfig()


class TestFeatureLineParsing(unittest.TestCase):
    """Tests for the getFeatureLines function"""

    def testsepline(self):
        """Check that we can parse the header"""
        self.assertEqual(",", parseSepLine("\"sep=,\""))
        self.assertEqual("-", parseSepLine("\"sep=-\""))
        self.assertEqual(",", parseSepLine("\"sep=,\"\r\n"))
        self.assertEqual(",", parseSepLine("\"sep=,\"\n\r"))
        self.assertEqual(",", parseSepLine("\"sep=,\"\n"))



        self.assertRaises(ParseError, parseSepLine, "\"sp=,\"")
        self.assertRaises(ParseError, parseSepLine, "\"sep=,")

        pass

    def testline(self):
        """Check that we can parse the first header line"""
        self.assertListEqual(["FILENAME", "LINE_START", "LINE_END", "TYPE", "EXPRESSION", "CONSTANTS"],
                             parseline(",", "FILENAME,LINE_START,LINE_END,TYPE,EXPRESSION,CONSTANTS"))
        self.assertListEqual(["FILENAME", "LINE_START", "LINE_END", "TYPE", "EXPRESSION", "CONSTANTS"],
                             parseline("-", "FILENAME-LINE_START-LINE_END-TYPE-EXPRESSION-CONSTANTS"))

        self.assertListEqual(["/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml", "1", "8", "#if", "defined(A)", "A"],
                             parseline(",", "/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml,1,8,#if,defined(A),A"))
        self.assertListEqual(["/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml", "3", "5", "#if", "(defined(A)) && ((defined(C) || defined(D)))", "A;C;D"],
                             parseline(",", "/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml,3,5,#if,(defined(A)) && ((defined(C) || defined(D))),A;C;D"))

        self.assertListEqual(["FILENAME", "LINE_START"], parseline(",", "FILENAME,LINE_START\r\n"))
        self.assertListEqual(["FILENAME", "LINE_START"], parseline(",", "FILENAME,LINE_START\n"))
        pass

    def testfeatureline(self):
        """Check that we can parse the first header line"""
        startline, endline, featurelist = parseFeatureLine(",", "/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml,3,5,#if,(defined(A)) && ((defined(C) || defined(D))),A;C;D")
        self.assertEqual(3, startline)
        self.assertEqual(5, endline)
        self.assertListEqual(["A", "C", "D"], featurelist)

        startline, endline, featurelist = parseFeatureLine(",", "/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml,1,8,#if,defined(A),A")
        self.assertEqual(1, startline)
        self.assertEqual(8, endline)
        self.assertListEqual(["A"], featurelist)

        pass

class TestFeatureLines(unittest.TestCase):
    """Tests for the getFeatureLines function"""

    def testsingleline(self):
        """Check that a single line is split as expected"""
        feature_dict = getFeatureLines([(3, 5, LineType.IF, ["A", "B"])], "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set([]))
        pass

    def testfolllowingline(self):
        """Check that a #ifdef can follow another #ifdef"""
        feature_dict = getFeatureLines([(3, 5, LineType.IF, ["A", "B"]), (6, 8, LineType.IF, ["C", "D"])], "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(7), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(8), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(9), set([]))
        pass

    def testorderdoesntmatter(self):
        """Check that a #ifdef can follow another #ifdef"""
        feature_dict = getFeatureLines([(6, 8, LineType.IF, ["C", "D"]), (3, 5, LineType.IF, ["A", "B"])], "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(7), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(8), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(9), set([]))
        pass

    def testnesting(self):
        """Check that a #ifdef can be nested in an another #ifdef"""
        feature_dict = getFeatureLines([(3, 9, LineType.IF, ["A", "B"]), (6, 8, LineType.IF, ["C", "D"])], "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set(["A", "B", "C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(7), set(["A", "B", "C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(8), set(["A", "B", "C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(9), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(10), set([]))
        pass

    def testnestingwithsamefeatures(self):
        """Check that a #ifdef can be nested in another #ifdef but have the same feature"""
        feature_dict = getFeatureLines([(3, 9, LineType.IF, ["A", "B"]), (6, 8, LineType.IF, ["A", "D"])], "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]), "line 2 should be empty")
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set(["A", "B", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(7), set(["A", "B", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(8), set(["A", "B", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(9), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(10), set([]))
        pass

    def testinvalidstartend(self):
        """Check we throw when end is before start"""
        self.assertRaises(ParseError, getFeatureLines, [(5, 3, LineType.IF, ["A", "B"])], "unittest.c")
        pass

    def testoverlapping(self):
        """Check we throw when line is used multiple times"""
        self.assertRaises(ParseError,
                          getFeatureLines, [(3, 5, LineType.IF, ["A", "B"]), (3, 6, LineType.IF, ["C"])], "unittest.c")
        pass


    def testoverlapping_2(self):
        """Check we throw when line is used multiple times"""
        self.assertRaises(ParseError,
                          getFeatureLines, [(3, 5, LineType.IF, ["A", "B"]), (5, 6, LineType.IF, ["C"])], "unittest.c")
        pass


    def testelif(self):
        """Check we throw when line is used multiple times"""
        # for example #elif "C" on line 5
        feature_dict = getFeatureLines([(3, 5, LineType.IF, ["A", "B"]), (5, 6, LineType.ELIF, ["A", "B", "C"])], "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B", "C"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set(["A", "B", "C"]))
        self.assertSetEqual(feature_dict.get_line_info(7), set([]))
        pass