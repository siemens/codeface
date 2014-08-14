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
# Copyright 2013, Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
# All Rights Reserved.

'''This class is a container to represent a commits relationship to the
other commits present in a particular file at the time of the commit.
The analysis is performed on a file by file basis. A commit can touch multiple
files however this class considers a given commit only in the context of a
single file.'''

import commit
import bisect


class FileDict:
    """
    A dictionary saving per-line information. We assume that we have information on any line,
    and that we only have to save changing lines.
    """
    def __init__(self, line_list, line_dict):
        """
        :rtype : FileDict
        """
        self.line_list = line_list
        self.line_dict = line_dict
        self.lastItem = line_list[-1]

    def __init__(self):
        """
        :rtype : FileDict
        """
        self.line_list = []
        self.line_dict = {}
        self.lastItem = -1

    def __iter__(self):
        return self.line_dict.__iter__()

    def get_line_info_raw(self, line_nr):
        """
        Returns the info for the given line (if the line was never set, the info for the last set line is returned)
        :param line_nr:
        :return:
        """
        i = bisect.bisect_right(self.line_list, line_nr)
        info_line = self.line_list[i-1]
        return self.line_dict[info_line]

    def get_line_info(self, line_nr):
        return set(self.get_line_info_raw(line_nr))

    def add_line(self, line_nr, info):
        """
        Add the given information to the current dictionary.
        Note: while filling the dictionary your line_nr has to be incremented!
        :param line_nr:
        :param info:
        :return:
        """
        if line_nr < self.lastItem:
            raise ValueError("can only incrementally add items")
        self.line_list.append(line_nr)
        self.line_dict[line_nr] = info


class FileCommit:
    def __init__(self):

        #filename under investigation
        self.filename = None

        #dictionary of dictionaries key is commit, value is a
        #dictionary with keys=lineNumbers value=commitHash, stores
        #the line number and corresponding commit hash for every
        #line of the file,
        self.fileSnapShots = {}

        #stores the commit hash of all contributions to the file for a
        #particular revision
        self.revCmts = []

        # dictionary with key = line number, value = function name
        self.functionIds = {0: "FILE_LEVEL"}

        # list of function line numbers in sorted order, this is for
        # optimizing the process of finding a function Id given a line number
        self.functionLineNums = [0]

        # dictionary with key = line number, value = feature list
        self.feature_info = FileDict()

    #Getter/Setters
    def getFileSnapShots(self):
        return self.fileSnapShots

    def getFileSnapShot(self):
        return self.fileSnapShots.values()[0]

    def setCommitList(self, cmtList):
        self.revCmts = cmtList

    def getrevCmts(self):
        return self.revCmts

    def setFunctionLines(self, functionIds):
        self.functionIds.update(functionIds)
        self.functionLineNums.extend(sorted(self.functionIds.iterkeys()))

    def setFeatureInfos(self, feature_line_infos):
        self.feature_info = feature_line_infos

    #Methods
    def addFileSnapShot(self, key, dict):
        self.fileSnapShots[key] = dict

    def findFuncId(self, lineNum):
        # returns the identifier of a function given a line number
        i = bisect.bisect_right(self.functionLineNums, lineNum)
        funcLine = self.functionLineNums[i-1]
        return self.functionIds[funcLine]

    def findFeatureList(self, lineNum):
        return self.feature_info.get_line_info(lineNum)
