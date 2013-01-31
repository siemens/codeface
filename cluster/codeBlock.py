import codeLine

class codeBlock:
    '''
    A code block is a contiguous set of lines of code from a single 
    commit
    '''
    
    
    #===========================
    # Constructors
    #===========================
    
    def __init__(self, start=None, end=None, authorId=None, committerId=None, cmtHash=None):
        
        self.start       = start #start of the code block
        self.end         = end   #end of the code block
        self.authorId    = authorId 
        self.committerId = committerId 
        self.cmtHash     = cmtHash
    #===========================
    # Accessors and Modifiers
    #===========================
    
    def get_codeLines(self):
        return self.codeLines
    
    
    def add_codeLine(self, lineNum, cmtHash, authorId, committerId):
        self.codeLines.append( codeLine.codeLine(lineNum, cmtHash, authorId, committerId) )