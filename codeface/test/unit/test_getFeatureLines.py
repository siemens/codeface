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

import unittest

from codeface.VCS import (get_feature_lines, parse_feature_line,
                          parse_line, parse_sep_line, ParseError, LineType)
from operator import eq
import logging
logging.basicConfig()


class TestFeatureLineParsing(unittest.TestCase):
    """Tests for the getFeatureLines function"""

    def testsepline(self):
        """Check that we can parse the header"""
        self.assertEqual(",", parse_sep_line("\"sep=,\""))
        self.assertEqual("-", parse_sep_line("\"sep=-\""))
        self.assertEqual(",", parse_sep_line("\"sep=,\"\r\n"))
        self.assertEqual(",", parse_sep_line("\"sep=,\"\n\r"))
        self.assertEqual(",", parse_sep_line("\"sep=,\"\n"))

        self.assertRaises(ParseError, parse_sep_line, "\"sp=,\"")
        self.assertRaises(ParseError, parse_sep_line, "\"sep=,")

        pass

    def testline(self):
        """Check that we can parse the first header line"""
        self.assertListEqual(
            ["FILENAME", "LINE_START", "LINE_END", "TYPE", "EXPRESSION",
             "CONSTANTS"],
            parse_line(
                ",",
                "FILENAME,LINE_START,LINE_END,TYPE,EXPRESSION,CONSTANTS"))
        self.assertListEqual(
            ["FILENAME", "LINE_START", "LINE_END", "TYPE", "EXPRESSION",
             "CONSTANTS"],
            parse_line(
                "-",
                "FILENAME-LINE_START-LINE_END-TYPE-EXPRESSION-CONSTANTS"))

        self.assertListEqual(
            ["/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml",
             "1", "8", "#if", "defined(A)", "A"],
            parse_line(",", "/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml,1,8,#if,defined(A),A"))
        self.assertListEqual(
            ["/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml",
             "3", "5", "#if", "(defined(A)) && ((defined(C) || defined(D)))", "A;C;D"],
            parse_line(",", "/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml,3,5,#if,(defined(A)) && ((defined(C) || defined(D))),A;C;D"))

        self.assertListEqual(["FILENAME", "LINE_START"],
                             parse_line(",", "FILENAME,LINE_START\r\n"))
        self.assertListEqual(["FILENAME", "LINE_START"],
                             parse_line(",", "FILENAME,LINE_START\n"))
        self.assertListEqual(["FILE", "LINE_\"START"],
                             parse_line(",", "FILE,\"LINE_\"\"START\""))
        pass

    def testfeatureline(self):
        """Check that we can parse the first header line"""
        startline, endline, line_type, featurelist, feature_expression = \
            parse_feature_line(",", "/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml,3,5,#if,(defined(A)) && ((defined(C) || defined(D))),A;C;D")
        self.assertEqual(3, startline)
        self.assertEqual(5, endline)
        self.assertEqual(LineType.IF, line_type)
        self.assertListEqual(["A", "C", "D"], featurelist)
        self.assertEqual("(defined(A)) && ((defined(C) || defined(D)))", feature_expression)

        startline, endline, line_type, featurelist, feature_expression = \
            parse_feature_line(",", "/tmp/tmpVemX4s_cppstats_featurelocations/_cppstats_featurelocations/tmpuAFx3b.xml,1,8,#if,defined(A),A")
        self.assertEqual(1, startline)
        self.assertEqual(8, endline)
        self.assertEqual(LineType.IF, line_type)
        self.assertListEqual(["A"], featurelist)
        self.assertEqual("defined(A)", feature_expression)

        startline, endline, line_type, featurelist, feature_expression = \
            parse_feature_line(",", "/tmp/tmpbPbqDy_cppstats_featurelocations/_cppstats_featurelocations/tmp5pTBQ4.xml,324,335,#if,0,")
        self.assertEqual(324, startline)
        self.assertEqual(335, endline)
        self.assertEqual(LineType.IF, line_type)
        self.assertListEqual([], featurelist)
        self.assertEqual("0", feature_expression)

        startline, endline, line_type, featurelist, feature_expression = \
            parse_feature_line(",", "/tmp/tmpY5XZci_cppstats_featurelocations/_cppstats_featurelocations/tmpWwrMnP.xml,941,943,#else,\"!(GTK_CHECK_VERSION(3, 0, 0))\",")
        self.assertEqual(941, startline)
        self.assertEqual(943, endline)
        self.assertEqual(LineType.ELSE, line_type)
        self.assertListEqual([], featurelist)
        self.assertEqual("!(GTK_CHECK_VERSION(3, 0, 0))", feature_expression)

        pass


class TestFeatureLines(unittest.TestCase):
    """Tests for the getFeatureLines function"""

    def testsingleline(self):
        """Check that a single line is split as expected"""
        feature_dict, fexpr_dict = \
            get_feature_lines([(3, 5, LineType.IF, ["A", "B"], "defined(A) && defined(B)")],
                              "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set([]))

        self.assertSetEqual(fexpr_dict.get_line_info(2), set([]))
        self.assertSetEqual(fexpr_dict.get_line_info(3), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(4), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(5), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(6), set([]))

        pass

    def testfolllowingline(self):
        """Check that a #ifdef can follow another #ifdef"""
        feature_dict, fexpr_dict = \
            get_feature_lines(
                [(3, 5, LineType.IF, ["A", "B"], "defined(A) && defined(B)"),
                 (6, 8, LineType.IF, ["C", "D"], "defined(C) && defined(D)")],
                "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(7), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(8), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(9), set([]))

        self.assertSetEqual(fexpr_dict.get_line_info(2), set([]))
        self.assertSetEqual(fexpr_dict.get_line_info(3), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(4), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(5), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(6), set(["defined(C) && defined(D)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(7), set(["defined(C) && defined(D)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(8), set(["defined(C) && defined(D)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(9), set([]))
        pass

    def testorderdoesntmatter(self):
        """Check that a #ifdef can follow another #ifdef"""
        feature_dict, fexpr_dict = \
            get_feature_lines(
                [(6, 8, LineType.IF, ["C", "D"], "defined(C) && defined(D)"),
                 (3, 5, LineType.IF, ["A", "B"], "defined(A) && defined(B)")],
                "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(7), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(8), set(["C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(9), set([]))

        self.assertSetEqual(fexpr_dict.get_line_info(2), set([]))
        self.assertSetEqual(fexpr_dict.get_line_info(3), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(4), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(5), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(6), set(["defined(C) && defined(D)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(7), set(["defined(C) && defined(D)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(8), set(["defined(C) && defined(D)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(9), set([]))

        pass

    def testnesting(self):
        """Check that a #ifdef can be nested in an another #ifdef"""
        feature_dict, fexpr_dict = \
            get_feature_lines(
                [(3, 9, LineType.IF, ["A", "B"], "defined(A) && defined(B)"),
                 (6, 8, LineType.IF, ["C", "D"],
                  "(defined(A) && defined(B)) && (defined(C) && defined(D))")],
                "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6),
                            set(["A", "B", "C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(7),
                            set(["A", "B", "C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(8),
                            set(["A", "B", "C", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(9), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(10), set([]))

        self.assertSetEqual(fexpr_dict.get_line_info(2), set([]))
        self.assertSetEqual(fexpr_dict.get_line_info(3), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(4), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(5), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(6),
                            set(["(defined(A) && defined(B)) && (defined(C) && defined(D))"]))
        self.assertSetEqual(fexpr_dict.get_line_info(7),
                            set(["(defined(A) && defined(B)) && (defined(C) && defined(D))"]))
        self.assertSetEqual(fexpr_dict.get_line_info(8),
                            set(["(defined(A) && defined(B)) && (defined(C) && defined(D))"]))
        self.assertSetEqual(fexpr_dict.get_line_info(9), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(10), set([]))
        pass

    def testnestingwithsamefeatures(self):
        """Check that a #ifdef can be nested in another
        #ifdef but have the same feature"""
        feature_dict, fexpr_dict = \
            get_feature_lines(
                [(3, 9, LineType.IF, ["A", "B"], "defined(A) && defined(B)"),
                 (6, 8, LineType.IF, ["A", "D"],
                  "(defined(A) && defined(B)) && (defined(D))")],
                "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]),
                            "line 2 should be empty")
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(6),
                            set(["A", "B", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(7),
                            set(["A", "B", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(8),
                            set(["A", "B", "D"]))
        self.assertSetEqual(feature_dict.get_line_info(9), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(10), set([]))

        self.assertSetEqual(fexpr_dict.get_line_info(2), set([]),
                            "line 2 should be empty")
        self.assertSetEqual(fexpr_dict.get_line_info(3), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(4), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(5), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(6),
                            set(["(defined(A) && defined(B)) && (defined(D))"]))
        self.assertSetEqual(fexpr_dict.get_line_info(7),
                            set(["(defined(A) && defined(B)) && (defined(D))"]))
        self.assertSetEqual(fexpr_dict.get_line_info(8),
                            set(["(defined(A) && defined(B)) && (defined(D))"]))
        self.assertSetEqual(fexpr_dict.get_line_info(9), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(10), set([]))
        pass

    def testinvalidstartend(self):
        """Check we throw when end is before start"""
        self.assertRaises(ParseError, get_feature_lines,
                          [(5, 3, LineType.IF, ["A", "B"], "defined(A) && defined(B)")], "unittest.c")
        pass

    def testoverlapping(self):
        """Check we throw when line is used multiple times"""
        self.assertRaises(ParseError, get_feature_lines,
                          [(3, 5, LineType.IF, ["A", "B"], "defined(A) && defined(B)"),
                           (3, 6, LineType.IF, ["C"], "defined(C)")],
                          "unittest.c")
        pass

    def testoverlapping_2(self):
        """Check we throw when line is used multiple times"""
        self.assertRaises(ParseError, get_feature_lines,
                          [(3, 5, LineType.IF, ["A", "B"], "defined(A) && defined(B)"),
                           (5, 6, LineType.IF, ["C"], "defined(C)")],
                          "unittest.c")
        pass


    def testelif(self):
        """Check we throw when line is used multiple times"""
        # for example #elif "C" on line 5
        feature_dict, fexpr_dict = \
            get_feature_lines(
                [(3, 5, LineType.IF, ["A", "B"], "defined(A) && defined(B)"),
                 (5, 6, LineType.ELIF, ["A", "B", "C"], "(!(defined(A)) && (!(defined(B)) && defined(C)")],
                "unittest.c")
        self.assertSetEqual(feature_dict.get_line_info(2), set([]))
        self.assertSetEqual(feature_dict.get_line_info(3), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(4), set(["A", "B"]))
        self.assertSetEqual(feature_dict.get_line_info(5),
                            set(["A", "B", "C"]))
        self.assertSetEqual(feature_dict.get_line_info(6),
                            set(["A", "B", "C"]))
        self.assertSetEqual(feature_dict.get_line_info(7), set([]))

        self.assertSetEqual(fexpr_dict.get_line_info(2), set([]))
        self.assertSetEqual(fexpr_dict.get_line_info(3), set(["defined(A) && defined(B)"]), fexpr_dict.get_line_info(3))
        self.assertSetEqual(fexpr_dict.get_line_info(4), set(["defined(A) && defined(B)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(5),
                            set(["(!(defined(A)) && (!(defined(B)) && defined(C)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(6),
                            set(["(!(defined(A)) && (!(defined(B)) && defined(C)"]))
        self.assertSetEqual(fexpr_dict.get_line_info(7), set([]))
        pass
