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
# Copyright 2013 by Matthias Dittrich <matthi.d@gmail.com>
# All Rights Reserved.

import unittest
from codeface.test.integration.example_projects import example_project_func
from codeface.test.integration.test_exampleprojects \
    import EndToEndTestSetup

__author__ = 'Matthias Dittrich'


class EndToEndOnlyTaggingTestSetup(EndToEndTestSetup):

    def setup_with_p(self, p):
        EndToEndTestSetup.setup_with_p(self, p)

        self.add_ignored_tables(
            ["mail_thread", "mailing_list", "twomode_vertices",
             "initiate_response", "freq_subjects", "twomode_edgelist",
             "thread_responses"])

    pass


class TestEndToEndOnlyTagging(object):
    def testEndToEnd(self):
        self.p = example_project_func[self.example_project](self.tagging)
        with self.p:
            self.setup_with_p(self.p)
            self.clear_tables()
            self.analyseEndToEnd()
            if self.correct_edges:
                self.checkEdges()
            self.checkResult()
            self.checkClean()


#class TestEndToEndOnlyTaggingExample1Tag(
#        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
#    example_project = 1
#    tagging = "tag"
#    correct_edges = None
#
#
#class TestEndToEndOnlyTaggingExample1C2A(
#        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
#    example_project = 1
#    tagging = "committer2author"
#    correct_edges = None
#
#
#class TestEndToEndOnlyTaggingExample1Proximity(
#        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
#    example_project = 1
#    tagging = "proximity"
#    add_ignore_tables = ["edgelist"]
#    correct_edges = None

#class TestEndToEndOnlyTaggingExample1Feature(
#        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
#    example_project = 1
#    tagging = "feature"
#    add_ignore_tables = ["edgelist"]
#    correct_edges = None
#
#
#class TestEndToEndOnlyTaggingExample1Feature_File(
#        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
#    example_project = 1
#    tagging = "feature_file"
#    add_ignore_tables = ["edgelist"]
#    correct_edges = None
#
#
#class TestEndToEndOnlyTaggingExample2Feature(
#        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
#    example_project = 2
#    tagging = "feature"
#    add_ignore_tables = ["edgelist"]
#    devs = ["Louie Loner", "Geoff Genius", "Bill Bully", "Max Maintainer",
#            "Adam Awkward", "Peter Popular", "Clara Confident"]
#    correct_edges = []
#
#
#class TestEndToEndOnlyTaggingExample2Feature_File(
#        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
#    example_project = 2
#    tagging = "feature_file"
#    add_ignore_tables = ["edgelist"]
#    devs = ["Louie Loner", "Geoff Genius", "Bill Bully", "Max Maintainer",
#            "Adam Awkward", "Peter Popular", "Clara Confident"]
#    correct_edges = []


class TestEndToEndOnlyTaggingExample3Feature(
        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
    example_project = 3
    tagging = "feature"
    add_ignore_tables = ["edgelist"]
    devs = ["Louie Loner", "Geoff Genius", "Bill Bully", "Max Maintainer",
            "Adam Awkward", "Peter Popular", "Clara Confident"]

    correct_edges =\
        [   # Release 0 - Release 1
            # Explanation of the data:
            # You can see the file-state in the last commit, the comments
            # help to identify where the line comes from (blame analysis).
            # As we use the last state to calculate collaboration, we can
            # see that peter, bill and adam worked on the features A and B.
            # So they have a collaboration with each other
            [
                [devs[2], devs[5], 1.0],  # bill and peter
                [devs[5], devs[2], 1.0],  # peter and billy
                [devs[2], devs[4], 1.0],  # bill and adam
                [devs[4], devs[2], 1.0],  # adam and bill
                [devs[5], devs[4], 1.0],  # peter and adam
                [devs[4], devs[5], 1.0]],  # adam and peter
            # Release 1 - Release 2
            # Here is a list of the changes grouped by features:
            # Feature 'A':
            # Adam:  4
            # Clara:  3
            # Geoff: 1
            #
            # Feature 'B':
            # Clara: 2
            # Adam: 3
            #
            # Feature 'C':
            # Adam: 2
            # Max: 1
            # The numbers represent the number of changed lines.
            [   # Feature A
                [devs[4], devs[6], 1.0],  # adam and clara
                [devs[6], devs[4], 1.0],
                [devs[4], devs[1], 1.0],  # adam and geoff
                [devs[1], devs[4], 1.0],
                [devs[6], devs[1], 1.0],  # clara and geoff
                [devs[1], devs[6], 1.0],
                # Feature B
                # clara and adam (already in Feature A)
                # Feature C
                # Adam and max
                [devs[4], devs[3], 1.0],  # adam and max
                [devs[3], devs[4], 1.0]]
        ]

    '''
    If you came here because you broke the test suite by changing the
    feature git test project (example project 3), then you most likely
    have to update all git hash values, please also update the blame
    outputs below.

    git blame v1_release -w -C -M src/carp.c
    ^ac61e7e (Adam Awkward  2013-01-07 16:00:00 +0100 1) int main() { // Adam
    ^ac61e7e (Adam Awkward  2013-01-07 16:00:00 +0100 2)  int a; // Adam
    3fe9884f (Peter Popular 2013-01-10 15:30:00 +0100 3) #if (defined(A) || defined(B)) // Peter
    7b16cf10 (Bill Bully    2013-01-09 15:31:00 +0100 4)  int b = 0; // Bill
    f95b8047 (Adam Awkward  2013-01-08 15:00:00 +0100 5) #endif // Adam
    4be60ac5 (Louie Loner   2013-01-01 15:30:00 +0100 6)  int c = 0; // Louie
    3b7cc950 (Louie Loner   2013-01-09 15:30:00 +0100 7)  return 1; // Louie
    ^ac61e7e (Adam Awkward  2013-01-07 16:00:00 +0100 8) }; // Adam

    git blame v2_release -w -C -M src/carp.c
    ^ac61e7e (Adam Awkward  2013-01-07 16:00:00 +0100 1) int main() { // Adam
    ^ac61e7e (Adam Awkward  2013-01-07 16:00:00 +0100 2)  int a; // Adam
    3fe9884f (Peter Popular 2013-01-10 15:30:00 +0100 3) #if (defined(A) || defined(B)) // Peter
    55eec100 (Geoff Genius  2013-01-21 12:50:42 +0100 4)  int b = 6; // Geoff
    f95b8047 (Adam Awkward  2013-01-08 15:00:00 +0100 5) #endif // Adam
    2d29196c (Geoff Genius  2013-01-21 12:50:42 +0100 6)  int c = 1; // Geoff
    3b7cc950 (Louie Loner   2013-01-09 15:30:00 +0100 7)  return 1; // Louie
    ^ac61e7e (Adam Awkward  2013-01-07 16:00:00 +0100 8) }; // Adam

    git blame v2_release -w -C -M src/code.c
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100  1) int main() { // Adam
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100  2)  int a; // Adam
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100  3) #if (defined(C)) // Adam
    29b9c8bc (Max Maintainer  2013-01-17 12:50:42 +0100  4)  int b = 1; // Max
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100  5) #endif // Adam
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100  6) #if (defined(A)) // Adam
    c52343ac (Clara Confident 2013-01-18 12:42:42 +0100  7)  int c = 2; // Clara
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100  8) #elif (defined(B)) // Adam
    c52343ac (Clara Confident 2013-01-18 12:42:42 +0100  9)  int c = 3; // Clara
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100 10) #else // Adam
    c52343ac (Clara Confident 2013-01-18 12:42:42 +0100 11)  int c = 4; // Clara
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100 12) #endif // Adam
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100 13)  int d = 0; // Adam
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100 14)  return 1; // Adam
    c9b59046 (Adam Awkward    2013-01-16 12:30:42 +0100 15) }; // Adam

    Additionally, the order of commits occurring in the test project is the following:

    release 1
    - f95b8047236f75641d6d7a2b5790b9e1db869ccd
    - 458c1a53c8adb5b09268546a035235cf9a8b034d
    - 4be60ac55e29d769a40fe11f41f33e88ba7f059b // filtered due to date
    - 3b7cc950b4446fa4a3e06a6cf0992ded1a5fe923
    - 3fe9884f98487cce4603d2bd5578e94944412d3c
    - 7b16cf10845bc64e2589fa63822f3ddc49aedd4d

    release 2
    - c9b59046b6eb473b97a97cb31aded2deced29dc6
    - 29b9c8bc6955df51263201dff7a1d935f8cd6049
    - c52343ac0d17ce9a30866d296da0deb23f1567a7
    - 55eec10019857e44d80e4bec3e81d1cffb785592
    - 2d29196c16dd5a2d4bed292f055a83d1b44a58e1

    '''
    commit_dependency =\
        [
            # Release 1 (see blame data above)
            ('f95b8047236f75641d6d7a2b5790b9e1db869ccd', 'src/carp.c',
             'A', 'Feature', 1, None),
            ('f95b8047236f75641d6d7a2b5790b9e1db869ccd', 'src/carp.c',
             'B', 'Feature', 1, None),
            ('f95b8047236f75641d6d7a2b5790b9e1db869ccd', 'src/carp.c',
            '(defined(A) || defined(B))', 'FeatureExpression', 1, None),

            ('3b7cc950b4446fa4a3e06a6cf0992ded1a5fe923', 'src/carp.c',
             'Base_Feature', 'Feature', 1, None),
            ('3b7cc950b4446fa4a3e06a6cf0992ded1a5fe923', 'src/carp.c',
             'Base_Feature', 'FeatureExpression', 1, None),

            ('3fe9884f98487cce4603d2bd5578e94944412d3c', 'src/carp.c',
             'A', 'Feature', 1, None),
            ('3fe9884f98487cce4603d2bd5578e94944412d3c', 'src/carp.c',
             'B', 'Feature', 1, None),
            ('3fe9884f98487cce4603d2bd5578e94944412d3c', 'src/carp.c',
            '(defined(A) || defined(B))', 'FeatureExpression', 1, None),

            ('7b16cf10845bc64e2589fa63822f3ddc49aedd4d', 'src/carp.c',
             'A', 'Feature', 1, None),
            ('7b16cf10845bc64e2589fa63822f3ddc49aedd4d', 'src/carp.c',
             'B', 'Feature', 1, None),
            ('7b16cf10845bc64e2589fa63822f3ddc49aedd4d', 'src/carp.c',
             '(defined(A) || defined(B))', 'FeatureExpression', 1, None),

            # Release 2 (see blame data above)
            ('c9b59046b6eb473b97a97cb31aded2deced29dc6', 'src/code.c',
             'A', 'Feature', 4, None),
            ('c9b59046b6eb473b97a97cb31aded2deced29dc6', 'src/code.c',
             'B', 'Feature', 3, None),
            ('c9b59046b6eb473b97a97cb31aded2deced29dc6', 'src/code.c',
             'C', 'Feature', 2, None),
            ('c9b59046b6eb473b97a97cb31aded2deced29dc6', 'src/code.c',
             'Base_Feature', 'Feature', 5, None),
            ('c9b59046b6eb473b97a97cb31aded2deced29dc6', 'src/code.c',
            '(defined(C))', 'FeatureExpression', 2, None),
            ('c9b59046b6eb473b97a97cb31aded2deced29dc6', 'src/code.c',
            '(defined(A))', 'FeatureExpression', 1, None),
            ('c9b59046b6eb473b97a97cb31aded2deced29dc6', 'src/code.c',
            '(!((defined(A)))) && ((defined(B)))', 'FeatureExpression', 1, None),
            ('c9b59046b6eb473b97a97cb31aded2deced29dc6', 'src/code.c',
            '(!((defined(A)))) && (!((defined(B))))', 'FeatureExpression', 2, None),
            ('c9b59046b6eb473b97a97cb31aded2deced29dc6', 'src/code.c',
             'Base_Feature', 'FeatureExpression', 5, None),

            ('29b9c8bc6955df51263201dff7a1d935f8cd6049', 'src/code.c',
             'C', 'Feature', 1, None),
            ('29b9c8bc6955df51263201dff7a1d935f8cd6049', 'src/code.c',
            '(defined(C))', 'FeatureExpression', 1, None),

            ('c52343ac0d17ce9a30866d296da0deb23f1567a7', 'src/code.c',
             'A', 'Feature', 3, None),
            ('c52343ac0d17ce9a30866d296da0deb23f1567a7', 'src/code.c',
             'B', 'Feature', 2, None),
            ('c52343ac0d17ce9a30866d296da0deb23f1567a7', 'src/code.c',
            '(defined(A))', 'FeatureExpression', 1, None),
            ('c52343ac0d17ce9a30866d296da0deb23f1567a7', 'src/code.c',
            '(!((defined(A)))) && ((defined(B)))', 'FeatureExpression', 1, None),
            ('c52343ac0d17ce9a30866d296da0deb23f1567a7', 'src/code.c',
            '(!((defined(A)))) && (!((defined(B))))', 'FeatureExpression', 1, None),

            ('55eec10019857e44d80e4bec3e81d1cffb785592', 'src/carp.c',
             'A', 'Feature', 1, None),
            ('55eec10019857e44d80e4bec3e81d1cffb785592', 'src/carp.c',
             'B', 'Feature', 1, None),
            ('55eec10019857e44d80e4bec3e81d1cffb785592', 'src/carp.c',
            '(defined(A) || defined(B))', 'FeatureExpression', 1, None),

            ('2d29196c16dd5a2d4bed292f055a83d1b44a58e1', 'src/carp.c',
             'Base_Feature', 'Feature', 1, None),
            ('2d29196c16dd5a2d4bed292f055a83d1b44a58e1', 'src/carp.c',
             'Base_Feature', 'FeatureExpression', 1, None)
        ]


class TestEndToEndOnlyTaggingExample3Feature_File(
        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
    example_project = 3
    tagging = "feature_file"
    add_ignore_tables = ["edgelist"]
    devs = ["Louie Loner", "Geoff Genius", "Bill Bully", "Max Maintainer",
            "Adam Awkward", "Peter Popular", "Clara Confident"]

    correct_edges =\
        [   # Release 0 - Release 1
            # Explanation of the data:
            # The commits are analysed by taking their order into account:
            # (see also 'feature' analysis)
            [   # peter and adam (peter committed after adam)
                [devs[5], devs[4], 1.0],
                # peter and bill
                # (peter committed after bill, see commit date)
                [devs[5], devs[2], 1.0],
                # bill and adam
                [devs[2], devs[4], 1.0]],
            # Release 1 - Release 2
            # From the "src/carp.c" file we get no collaboration as there
            # was only one commit within this file
            # (within the current commit range)!
            # In the other file we can see on the last state that
            # Clara collaborated with Adam on Feature A and Feature B
            # and Max with Adam on Feature C
            [   # clara and adam (clara after adam)
                [devs[6], devs[4], 1.0],
                # max and adam (max after adam)
                [devs[3], devs[4], 1.0]]]

    # This actually produces the same data as the "feature" analysis
    # (see above for details)
    commit_dependency = \
        TestEndToEndOnlyTaggingExample3Feature.commit_dependency

#class TestEndToEndOnlyTaggingExample2Tag(
#        EndToEndOnlyTaggingTestSetup, TestEndToEndOnlyTagging):
#    example_project = 2
#    tagging = "tag"
#    correct_edges = None
#    #testEndToEnd = unittest.expectedFailure(TestEndToEndOnlyTagging.testEndToEnd)
