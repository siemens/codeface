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

class FileCommit:
    def __init__(self):

        #filename under investigation
        self.fileName = None
        
        #list of all commits to a file
        commitList = []
        
        #dictionary of dictionaries key is commit, value is a
        #dictionary with keys=lineNumbers value=commitHash, stores 
        #the line number and corresponding commit hash for every 
        #line of the file, 
        self.fileSnapShots = {}
        
        #stores the line numbers that correspond to mainCommitID
        #we restrict our analysis of the surrounding commits
        #to some small region around the main commits contributions
        self.lineNumsMainCmt = []
        
    #Getter/Setters    
    
    def getFileSnapShots(self):
        return self.fileSnapShots
        
    def setCommitList(self, cmtList):
        self.commitList = cmtList
        
    def getCommitList(self):
        return self.commitList
   
   
    #Methods
    
    def addFileSnapShot(self, key, dict):
        self.fileSnapShots[key] = dict
    
