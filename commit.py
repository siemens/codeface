# Class to represent a commit (which may be composed of multiple diffs). 
# Since this class is serialised, it should be changed as little
# as possible.

class Commit:
    def __init__(self):
        # Base characteristics: uniqiue id (typically a hash value) and 
        # time stamp (commiter time) of the commit
        self.id = None
        self.cdate = None

        # Contains a tuple (added, deleted, changed)
        # for each diff type.
        self.diff_info = [] 

        # First entry is number of lines, second number of characters
        self.commit_msg_info = (None, None)

        # A hash with tag type as key. The datum is an array
        # with all persons associated with the tag type
        self.signed_offs = None 

    # The following methods replace hard-coded constants
    # with reasonable names
    def getAddedLines(self, difftype):
        return self.diff_info[difftype][1]

    def getDeletedLines(self, difftype):
        return self.diff_info[difftype][2]

    def getChangedFiles(self, difftype):
        return self.diff_info[difftype][0]

    def getSignedOffs(self):
        return self.signed_offs

    def getCommitMessageLines(self):
        return self.commit_msg_info[0]

    def getCommitMessageSize(self):
        return self.commit_msg_info[1]

