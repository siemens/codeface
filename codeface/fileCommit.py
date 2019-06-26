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
    A generic dictionary for saving per-line information.
    We assume that this information is available on any line,
    and that the information only changes on some lines.
    So we only save the information on lines that change that info
    and use bisect to retrieve that information (for any line).
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
        Returns the info for the given line
        (if the line was never set, the info for the last set line
        is returned)
        :param line_nr: the line to retrieve the information for.
        :return: the information for the given line.
        """
        i = bisect.bisect_right(self.line_list, line_nr)
        info_line = self.line_list[i-1]
        return self.line_dict[info_line]

    def get_line_info(self, line_nr):
        return set(self.get_line_info_raw(line_nr))

    def add_line(self, line_nr, info):
        """
        Add the given information to the current dictionary.
        Note: while filling the dictionary the line_nr argument has to
        be incremented (this is only to make sure the caller
        gets the intended behavior)!
        :param line_nr: the line number of the information
        :param info: the information for the current line
        """
        if line_nr < self.lastItem:
            raise ValueError("can only incrementally add items")
        self.line_list.append(line_nr)

        # to ensure reliability for the 'bisect_right' call in the function
        # 'get_line_info_raw', make sure the lines in the line_list are sorted
        self.line_list.sort()

        self.line_dict[line_nr] = info

    def values(self):
        return self.line_dict.values()


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
        file_level = -1
        self.functionIds = {file_level:'File_Level'}

        # list of function line numbers in sorted order, this is for
        # optimizing the process of finding a function Id given a line number
        self.functionLineNums = [file_level]

        # Function Implementation
        self.functionImpl = {}

        # True if start/end boundaries of artefacts are available (otherwise,
        # only the start of an artefact is known
        self.artefact_line_range = False

        # source code element list
        # stores all source code elements of interest and
        # meta data
        # NOTE: This does never ever seem to be used. Discuss with
        # Mitchell what this was all about
        self._src_elem_list = []

        # dictionaries with key = line number, value = feature list|feature expression
        self.feature_info = FileDict()
        self.feature_expression_info = FileDict()

    #Getter/Setters
    def getFileSnapShots(self):
        return self.fileSnapShots

    def getFileSnapShot(self):
        return self.fileSnapShots.values()[0]

    def getFilename(self):
        return self.filename

    def setCommitList(self, cmtList):
        self.revCmts = cmtList

    def getrevCmts(self):
        return self.revCmts

    def getFuncImpl(self,id):
        if id in self.functionImpl:
            return self.functionImpl[id]
        else:
            return []

    def setFunctionLines(self, functionIds):
        self.functionIds.update(functionIds)
        for id in self.functionIds.values():
            self.functionImpl.update({id:[]})
        self.functionLineNums.extend(sorted(self.functionIds.iterkeys()))

    def setSrcElems(self, src_elem_list):
        self._src_elem_list.extend(src_elem_list)

    def set_feature_infos(self, feature_line_infos):
        self.feature_info = feature_line_infos[0]
        self.feature_expression_info = feature_line_infos[1]

    #Methods
    def addFileSnapShot(self, key, dict):
        self.fileSnapShots[key] = dict

    def findFuncId(self, line_num):
        # returns the identifier of a function given a line number
        func_id = 'File_Level'
        line_num = int(line_num)
        if self.artefact_line_range == True:
            if line_num in self.functionIds:
                func_id = self.functionIds[line_num]
        else:
            i = bisect.bisect_right(self.functionLineNums, line_num)
            func_line = self.functionLineNums[i-1]
            func_id = self.functionIds[func_line]
        return func_id

    def getLineCmtId(self, line_num):
        ## Retrieve the first file snap
        line_num = str(line_num)
        file_snapshot = self.getFileSnapShot()
        return file_snapshot[line_num]

    def getLength(self):
        return len(self.getFileSnapShot())

    def getIndx(self):
        return self.getFileSnapShot().keys()

    def addFuncImplLine(self, lineNum, srcLine):
        id = self.findFuncId(lineNum)
        self.functionImpl[id].append(srcLine)

    def findFeatureList(self, line_index):
        return self.feature_info.get_line_info(int(line_index) + 1)

    def findFeatureExpression(self, line_index):
        return self.feature_expression_info.get_line_info(int(line_index) + 1)
