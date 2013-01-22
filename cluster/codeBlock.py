import codeLine

class codeBlock:
    
    #===========================
    # Constructors
    #===========================
    
    def __init__(self, start=None, end=None, id=None, codeLines=None):
        
        self.start = start #start of the code block
        self.end   = end   #end of the code block
        self.id    = id    #reference to contributor 
        self.codeLines = codeLines
        
    #===========================
    # Accessors and Modifiers
    #===========================
    
    def get_codeLines(self):
        return self.codeLines
    
    
    def add_codeLine(self, lineNum, cmtHash, authorId, committerId):
        self.codeLines.append( codeLine.codeLine(lineNum, cmtHash, authorId, committerId) )