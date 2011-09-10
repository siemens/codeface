from commit_analysis import tag_types

class PersonInfo:
    """ Information about a commiter, and his relation to other commiters"""
    
    def __init__(self, subsys_names = [], ID=None, name=None):
        self.ID = ID
        self.name = name

        # Store from which persons a commit received a tag
        self.associations = {}
        for tag in tag_types:
            self.associations[tag] = {}

        # See addPerformTagRelation on the meaning of the following
        self.inv_associations = {}
        self.tagged_commits = {}
        for tag in tag_types + ["author"]:
            self.inv_associations[tag] = {}
            self.tagged_commits[tag] = []

        # See coputeTagStats
        self.tag_fraction = {}
        self.subsys_fraction = {}

        # Which subsystems were touched in which role. The entry
        # for each tag role is a hash with all subsystems as key, and the
        # count how often the subsystem was touched in that role as value
        self.subsys_touched = {}

        # "author" is a special-purpose role that is not included in
        # the generic tag role list.
        for tag in tag_types + ["author"]:
            self.subsys_touched[tag] = {}
            
            # General is used if the commit does not touch any well-defined
            # subsystem(s), for instance when a generic header is modified.
            for subsys in subsys_names + ["general"]:
                self.subsys_touched[tag][subsys] = 0
                self.subsys_touched[tag]["general"] = 0

        # Count how often the person has tagged a commit (i.e., given a
        # signed-off, an acked-by etc. to some commit)
        self.tagsPerformed = 0

    def setID(self, ID):
        self.ID = ID
    def getID(self):
        return self.ID

    def setName(self, name):
        self.name = name
    def getName(self):
        return self.name

    def addTagRelation(self, tag, ID, assoc):
        """State that the person has received or given a tag from/to ID.

        The distinction between taking and giving is made in other
        functions."""

        if (ID in assoc[tag]):
            assoc[tag][ID] += 1
        else:
            assoc[tag][ID] = 1
        
    def addReceiveTagRelation(self, tag, ID):
        """State that the person has received a tag from ID.

        For instance, the person could have a commit that was Acked-By
        ID 123, so it receives an Acked-by from 123.
        We count how often this happens per person."""
        
        self.addTagRelation(tag, ID, self.associations)

    def getReceiveTagRelations(self, tag):
        return self.associations[tag]


    def addPerformTagRelation(self, tag, ID, cmt):
        """State that the person has given a tag to ID. Also record the
           commit hash.

        Essentially the inverse of addReceiveTagRelation, with the extension
        that the commit hash is also recorded."""

        self.addTagRelation(tag, ID, self.inv_associations)
#        print("Extending {0} by {1}. Length before: {2}".format(tag, cmt.id, len(self.tagged_commits[tag])))

        cmt_subsys = cmt.getSubsystemsTouched()
        for subsys in cmt_subsys:
            self.subsys_touched[tag][subsys] += cmt_subsys[subsys]

        self.tagged_commits[tag].append(cmt.id)
        self.tagsPerformed += 1
        
    def getPerformTagRelations(self, tag):
        return self.inv_associations[tag]

    def computeTagStats(self):
        """Compute statistics inferred from the tag information.

        While this can be called anytime, it makes sense to call the function
        only once all tag data have been collected."""

        if self.tagsPerformed == 0:
            return

        # Distribution of tags (e..g, 30% Signed-Off, 60% CC, 10% Acked-By)
        for tag in tag_types + ["author"]:
            self.tag_fraction[tag] = \
                len(self.tagged_commits[tag])/float(self.tagsPerformed)

        # Distribution of subsystem tagging percentages (i.e., 30% net,
        # 20% drivers, 50% core).
        total_tags = 0
        
        # Summarise over all different tag variants
        for subsys in subsys_names + ["general"]:
            self.subsys_fraction[subsys] = 0
            for tag in tag_types + ["author"]:
                self.subsys_fraction[subsys] += \
                    self.subsys_touched[tag][subsys]
            total_tags += self.subsys_fraction[subsys]

        # ... and normalise accordingly
        if (total_tags != 0):
            for subsys in subsys_names + ["general"]:
                self.subsys_fraction[subsys] /= total_tags

        # TODO: Add any other calculations that are of interest here
            

    def getTagStats(self):
        return self.tag_fraction

    def getSubsysStats(self):
        return self.subsys_touched

############################ Test cases #########################
if __name__ == "__main__":
    personInfo = PersonInfo("sepp")

# TODO: Implement a couple of test cases
