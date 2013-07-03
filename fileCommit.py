# This file is part of prosoda.  prosoda is free software: you can
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
        self.functionIds = {}
        
        # list of function line numbers in sorted order, this is for
        # optimizing the process of finding a function Id given a line number
        self.functionLineNums = []
        
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
        self.functionIds = functionIds
        # check if a function exists from line 1, if not make a dummy function
        # that pre first function lines will be assigned
        if 1 not in functionIds:
           self.functionIds[1] = "preFunction" 
        self.functionLineNums = sorted(self.functionIds.iterkeys())
        
    #Methods
    def addFileSnapShot(self, key, dict):
        self.fileSnapShots[key] = dict
        
    def findFuncId(self, lineNum):
        # returns the identifier of a function given a line number
        i = bisect.bisect_right(self.functionLineNums, lineNum)
        funcLine = self.functionLineNums[i-1]
        return self.functionIds[funcLine]    
    
