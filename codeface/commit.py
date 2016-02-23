# Class to represent a commit (which may be composed of multiple diffs).
# Since this class is serialised, it should be changed as little
# as possible.

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
# Copyright 2010, 2011, 2012 by Wolfgang Mauerer <wm@linux-kernel.net>
# All Rights Reserved.

"""Contains class Commit with all required members."""

from cluster.PersonInfo import PersonInfo


class Commit(object):
    """Describes a single commit.

    Attributes:
        id (str): Unique ID (hash value expected).
        cdate (int): Commit timestamp.
        adate (int): Author timestamp.
        adate_tz (int): Author timestamp timezone.
        author (str): Author name.
        author_pi (PersonInfo): PersonInfo instance for author.
        committer (str): Committer name.
        committer_pi (PersonInfo): PersonInfo instance for Committer.
        is_corrective (bool): Boolean for whether commit is corrective.
        description (str): Commit message.
        diff_info (dict): Dict of tuples (Lines added, Lines changed, Lines
            deleted), mapping from diff type to number of lines affected.
        commit_msg_info (tuple): Tuple of (Number of lines, Number of chars).
        tag_pi_list (dict): A dict of lists, mapping tag types to sets of
            PersonInfo instances.
        tag_names_list (dict): A dict of lists, mapping tag types to sets of
            names.
        subsystems_touched (dict): A dict of booleans, mapping subsystem names
            to boolean.
        inRC (bool): Boolean for whether the commit is part of an RC phase or
            not.
        author_subsy_similarity (float): Measure of how focused the author is on
            the subsystems touched by the commit, normalized to [0.0 ... 1.0]
        author_taggers_similarity (float): Similarities between author and
            taggers, also normalized.
        taggers_subsys_similarity (float): Focus of taggers on the subsystems,
            also normalized.
    """
    # TODO Replace java-style getters with python-style properties
    # TODO `diff_info` appears to be dead code
    # http://2ndscale.com/rtomayko/2005/getters-setters-fuxors
    # https://google.github.io/styleguide/pyguide.html#Properties

    CORRECTIVE_KEYWORDS = ['bug', 'fix', 'error', 'fail']
    """Keywords to identify corrective commits"""
    # Ref: A. Mockus and L. G. Votta:
    # Identifying Reasons for Software Changes Using Historic Databases

    def __init__(self):
        """Initialise a commit object with blank values."""
        self.id = None
        self.cdate = None
        self.adate = None
        self.adate_tz = None
        self.author = None
        self.author_pi = None
        self.committer = None
        self.committer_pi = None
        self.is_corrective = False
        self.description = None
        self.diff_info = []
        self.commit_msg_info = (None, None)
        self.tag_pi_list = {}
        self.tag_names_list = {}
        self.subsystems_touched = {}
        self.inRC = False
        self.author_subsys_similarity = None
        self.author_taggers_similarity = None
        self.taggers_subsys_similarity = None

    def getCdate(self):
        return self.cdate

    def setCdate(self, cdate):
        self.cdate = cdate

    def getAddedLines(self, difftype):
        return self.diff_info[difftype][1]

    def getDeletedLines(self, difftype):
        return self.diff_info[difftype][2]

    def getChangedFiles(self, difftype):
        return self.diff_info[difftype][0]

    def getTagPIs(self):
        return self.tag_pi_list

    def setTagPIs(self, tag_pi_list):
        self.tag_pi_list = tag_pi_list

    def getTagNames(self):
        return self.tag_names_list

    def getCommitMessageLines(self):
        return self.commit_msg_info[0]

    def getCommitMessageSize(self):
        return self.commit_msg_info[1]

    def getAuthorName(self):
        return self.author

    def getAuthorPI(self):
        return self.author_pi

    def setAuthorPI(self, author_pi):
        self.author_pi = author_pi

    def getCommitterName(self):
        return self.committer

    def getCommitterPI(self):
        return self.committer_pi

    def setCommitterPI(self, committer_pi):
        self.committer_pi = committer_pi

    def getInRC(self):
        return self.inRC

    def setInRC(self, inRC):
        self.inRC = inRC

    def getSubsystemsTouched(self):
        return self.subsystems_touched

    def setSubsystemsTouched(self, subsystems_touched):
        self.subsystems_touched = subsystems_touched

    def setAuthorSubsysSimilarity(self, sim):
        self.author_subsys_similarity = sim

    def getAuthorSubsysSimilarity(self):
        return self.author_subsys_similarity

    def setAuthorTaggersSimilarity(self, sim):
        self.author_taggers_similarity = sim

    def getAuthorTaggersSimilarity(self):
        return self.author_taggers_similarity

    def setTaggersSubsysSimilarity(self, sim):
        self.taggers_subsys_similarity = sim

    def getTaggersSubsysSimilarity(self):
        return self.taggers_subsys_similarity

    def setDescription(self, desc):
        self.description = ' '.join(desc)

    def checkIfCorrective(self, desc):
        """Check if commit description contains keywords that indicate a
        corrective commit.

        Args:
            desc (list): Sequence of description lines.
        """
        # TODO Shoulnd't this method use self.description instead of parameter?
        is_corrective = False
        for line in desc:
            contains_keyword = [keyword in line.lower()
                                for keyword in Commit.CORRECTIVE_KEYWORDS]
            is_corrective = any(contains_keyword)

            if is_corrective:
                break

        self.is_corrective = is_corrective
