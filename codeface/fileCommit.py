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
        self.functionIds = {-1:'File_Level'}

        # list of function line numbers in sorted order, this is for
        # optimizing the process of finding a function Id given a line number
        self.functionLineNums = [0]

        # Function Implementation
        self.functionImpl = {}

        # doxygen flag
        self.doxygen_analysis = False

        # source code element list
        # stores all source code elements of interest and
        # meta data
        self._src_elem_list = []

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
        return self.functionImpl[id]

    def setFunctionLines(self, functionIds):
        self.functionIds.update(functionIds)
        [self.functionImpl.update({id:[]}) for id in self.functionIds.values()]
        self.functionLineNums.extend(sorted(self.functionIds.iterkeys()))

    def setSrcElems(self, src_elem_list):
        self._src_elem_list.extend(src_elem_list)

    #Methods
    def addFileSnapShot(self, key, dict):
        self.fileSnapShots[key] = dict

    def findFuncId(self, line_num):
        # returns the identifier of a function given a line number
        func_id = 'File_Level'
        line_num = int(line_num)
        if self.doxygen_analysis == True:
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
