class codeLine:
    
    #===========================
    # Constructors
    #===========================   
    def __init__(self, lineNum=None, cmtHash=None, authorId=None, committerId=None):
        
        self.lineNum     = lineNum
        self.cmtHash     = cmtHash
        self.authorId    = authorId
        self.committerId = committerId
        
        
    #===========================
    # Accessors and Modifiers
    #===========================
    
    def get_lineNum(self):
        return self.lineNum
    
    def set_lineNum(self,lineNum):
        self.lineNum = lineNum
        
    def get_cmtHash(self):
        return self.cmtHash
    
    def set_cmtHash(self, cmtHash):
        self.cmtHash = cmtHash
        
    def get_authorId(self):
        return self.authorId
    
    def set_authorId(self, authorId):
        self.authorId = authorId
    
    def get_committerId(self):
        return self.committerId
    
    def set_commiterId(self, committerId):
        self.committerId = committerId
    
    