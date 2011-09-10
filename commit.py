# Class to represent a commit (which may be composed of multiple diffs). 
# Since this class is serialised, it should be changed as little
# as possible.

class Commit:
    def __init__(self):
        # Base characteristics: uniqiue id (typically a hash value) and 
        # time stamp (commiter time) of the commit
        self.id = None
        self.cdate = None
        self.author = None      # Author name
        self.author_pi  = None  # PersonInfo instance

        # Contains a tuple (added, deleted, changed)
        # for each diff type.
        self.diff_info = [] 

        # First entry is number of lines, second number of characters
        self.commit_msg_info = (None, None)

        # A hash with tag type as key. The datum is an array
        # with all PersonInfo instances for the tag type
        self.tag_pi_list = {}

        # A hash with tag type as key. The datum is an array
        # with all names (as string) for the tag type
        self.tag_names_list = {}

        # Subsystems the commit touches. Keys are the subsystem names
        # values are 1 for touched and 0 for not touched.
        self.subsystems_touched = {}

        # Does the commit fall into a RC phase?
        self.inRC = False

        # Measure of how focused the author is on the subsystems
        # touched by the commit (0 is minimal, 1 is maximal focus)
        self.author_subsys_similarity = None

        # Same for the similarity between author and taggers
        self.author_taggers_similarity = None

        # ... and for taggers and subsystems
        self.taggers_subsys_similarity = None
        

    # The following methods replace hard-coded constants
    # with reasonable names
    def getAddedLines(self, difftype):
        return self.diff_info[difftype][1]

    def getDeletedLines(self, difftype):
        return self.diff_info[difftype][2]

    def getChangedFiles(self, difftype):
        return self.diff_info[difftype][0]

    def getTagPIs(self):
        return self.tag_pi_list

    def setTagPIs(self, tag_pi_list):
        self.tag_pi_list = tag_pi_list

    def getTagNames(self):
        return self.tag_names_list

    def getCommitMessageLines(self):
        return self.commit_msg_info[0]

    def getCommitMessageSize(self): # "Size" as in "number of characters@
        return self.commit_msg_info[1]

    def getAuthorName(self):
        return self.author

    def getAuthorPI(self):
        return self.author_pi

    def setAuthorPI(self, author_pi):
        self.author_pi = author_pi

    def getInRC(self):
        return self.inRC

    def setInRC(self, inRC):
        self.inRC = inRC

    def getSubsystemsTouched(self):
        return self.subsystems_touched

    def setSubsystemsTouched(self, subsystems_touched):
        self.subsystems_touched = subsystems_touched

    def setAuthorSubsysSimilarity(self, sim):
        self.author_subsys_similarity = sim

    def getAuthorSubsysSimilarity(self):
        return self.author_subsys_similarity

    def setAuthorTaggersSimilarity(self, sim):
        self.author_taggers_similarity = sim

    def getAuthorTaggersSimilarity(self):
        return self.author_taggers_similarity

    def setTaggersSubsysSimilarity(self, sim):
        self.taggers_subsys_similarity = sim

    def getTaggersSubsysSimilarity(self):
        return self.taggers_subsys_similarity
