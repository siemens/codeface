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

"""Contains class commit with all required members"""

class Commit(object):
    """Describes a single commit.

    Attributes:
        id: Unique ID (hash value expected)
        cdate: Timestamp of the commit
        adate:
        adate_tz:
        author: Author name
        author_pi: PersonInfo instance for author
        committer: Committer name
        committer_pi: PersonInfo instance for Committer
        is_corrective: Boolean for whether commit is corrective
        description:
        diff_info: Contains tuple (added, changed, deleted) for each diff type
        commit_msg_info: First entry is number of lines, second number of chars
        tag_pi_list: A hash with tag type as key. The datum is an array with
            all PersonInfo instances for the tag type
        tag_names_list: A hash with tag type as key. The datum is an array with
            all names (as string) for the tag type
        subsystems_touched: Subsystems the commit touches. Keys are the
            subsystem names, values are 1 for touched and 0 for not touched.
        inRC: Boolean for whether the commit is part of an RC phase or not
        author_subsy_similarity: Measure of how focused the author is on the
            subsystems touched by the commit (0 is minimal, 1 is maximal focus)
        author_taggers_similarity: Similarities between author and taggers
        taggers_subsys_similarity: Focus of taggers on the subsystems
    Methods;
    """
    #TODO Replace java-style getters with python-style properties
    # http://2ndscale.com/rtomayko/2005/getters-setters-fuxors
    # https://google.github.io/styleguide/pyguide.html#Properties

    CORRECTIVE_KEYWORDS = ['bug', 'fix', 'error', 'fail']
    """Keywords to identify corrective commits"""
    # Ref: A. Mockus and L. G. Votta:
    # Identifying Reasons for Software Changes Using Historic Databases


    def __init__(self):
        """Initialise a commit object with blank values"""
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

    # The following methods replace hard-coded constants
    # with reasonable names
    def getCdate(self):
        """Returns date of commit"""
        return self.cdate

    def setCdate(self, cdate):
        """Sets date of commit"""
        self.cdate = cdate

    def getAddedLines(self, difftype):
        """Returns added lines"""
        return self.diff_info[difftype][1]

    def getDeletedLines(self, difftype):
        """Returns deleted lines"""
        return self.diff_info[difftype][2]

    def getChangedFiles(self, difftype):
        """Returns names of changed files"""
        return self.diff_info[difftype][0]

    def getTagPIs(self):
        """Returns PersonInfo IDs for tag"""
        return self.tag_pi_list

    def setTagPIs(self, tag_pi_list):
        """Sets PersonInfo IDs for tags"""
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

    def setDescription(self, descr):
        self.description = ' '.join(descr)

    def checkIfCorrective(self, descr):
        """Check if commit description contains keywords that indicate a
        corrective commit

        Args:
            descr:
        """
        for line in descr:
            contains_keyword = [keyword in line.lower()
                                for keyword in Commit.CORRECTIVE_KEYWORDS]
            is_corrective = any(contains_keyword)

            if is_corrective:
                break
        # End for line

        self.is_corrective = is_corrective
