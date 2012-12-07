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
        self.commitRelationship = {}
        
        #stores the line numbers that correspond to mainCommitID
        #we restrict our analysis of the surrounding commits
        #to some small region around the main commits contributions
        self.lineNumsMainCmt = []
        
        
        
    def setCommitList(self, cmtList):
        self.commitList = cmtList
        
    def getCommitList(self):
        return self.commitList
    
    def addCommitRelationship(self, key, dict):
        self.commitRelationship[key] = dict
    
