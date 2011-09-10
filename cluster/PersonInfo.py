from commit_analysis import tag_types

class PersonInfo:
    """ Information about a commiter, and his relation to other commiters"""
    
    def __init__(self, subsys_names = [], ID=None, name="", email=""):
        self.ID = ID
        self.name = name
        self.email = email
        self.subsys_names = subsys_names

        # Store from which developers the person received a tag
        self.associations = {}
        for tag in tag_types:
            self.associations[tag] = {}

        # See addPerformTagRelation on the meaning of the following
        self.inv_associations = {}
        self.tagged_commits = {}
        for tag in tag_types + ["author"]:
            self.inv_associations[tag] = {}
            self.tagged_commits[tag] = []

        # See computeTagStats()
        self.tag_fraction = {}
        self.subsys_fraction = {}

        # List of all commits authored by this person (commit instances)
        # The commit_stats is a hash with summary statistics, see
        # computeCommitStats() for details
        self.commit_list = []
        self.commit_stats = None

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

        # Count how many tags (independent of tag category) have been
        # received form a specific ID.
        self.tags_received_by_id = {}

    def setID(self, ID):
        self.ID = ID
    def getID(self):
        return self.ID

    def setName(self, name):
        self.name = name
    def getName(self):
        if self.name == "":
            return self.email
        return self.name

    def setEmail(self, email):
        self.email = email
    def getEmail(self):
        return self.email

    def getCommitList(self):
        return self.commit_list

    def addCommit(self, cmt):
        self.commit_list.append(cmt)

    def getTagsReceivedByID(self, ID):
        if ID in self.tags_received_by_id.keys():
            return self.tags_received_by_id[ID]
        else:
            return 0

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

    def getSubsysFraction(self):
        return self.subsys_fraction

    def computeTagStats(self):
        """Compute statistics inferred from the tag information.

        While this can be called anytime, it makes sense to call the function
        only once all tag data have been collected."""

        if self.tagsPerformed == 0:
            print("Warning: {0} did not perform any tags?!".
                  format(self.getName()))
            return

#        print("{0} performed {1} tags".format(self.getName(),
#                                               self.tagsPerformed))

        # Per-author distribution of tags (e..g, 30% Signed-Off, 60%
        # CC, 10% Acked-By)
        for tag in tag_types + ["author"]:
            self.tag_fraction[tag] = \
                len(self.tagged_commits[tag])/float(self.tagsPerformed)

        # Per-author distribution of subsystem tagging percentages
        # (i.e., 30% net, 20% drivers, 50% core).
        total_tags = 0
        
        # Summarise over all different tag variants (for tags
        # given _by_ the developer)
        for subsys in self.subsys_names + ["general"]:
            self.subsys_fraction[subsys] = 0

            for tag in tag_types + ["author"]:
                self.subsys_fraction[subsys] += \
                    self.subsys_touched[tag][subsys]

            total_tags += self.subsys_fraction[subsys]

        # ... and normalise accordingly
        if (total_tags != 0):
            for subsys in self.subsys_names + ["general"]:
                self.subsys_fraction[subsys] /= float(total_tags)
        else:
            print("{0} did not tag on any subsystem?!".format(self.getName()))

        # Summarise the tags given _to_ (i.e, received by) the developer
        # from a specific ID
        for tag in self.associations:
            for ID in self.associations[tag]:
                if ID in self.tags_received_by_id.keys():
                    self.tags_received_by_id[ID] += self.associations[tag][ID]
                else:
                    self.tags_received_by_id[ID] = self.associations[tag][ID]

        # TODO: Add any other calculations that are of interest here
            
    def getTagStats(self):
        return self.tag_fraction

    def getSubsysStats(self):
        return self.subsys_touched

    def getSubsysDistribution(self):
        return self.subsys_fraction

    def computeCommitStats(self):
        """Infer statistical information from per-author commits.

        While this function can be called at any time, it naturally
        makes sense to only use it once all commits have been associated
        with the author"""

        self.commit_stats = {}
        self.commit_stats["added"] = 0
        self.commit_stats["deleted"] = 0
        self.commit_stats["numcommits"] = len(self.commit_list)
        
        # NOTE: We use only a single difftype although the information
        # from multiple is available
        for cmt in self.commit_list:
            self.commit_stats["added"]   += cmt.getAddedLines(0)
            self.commit_stats["deleted"] += cmt.getDeletedLines(0)

        # TODO: There are many other summary statistics that we could
        # compute, but which ones make sense?

    def getCommitStats(self):
        return self.commit_stats
            

############################ Test cases #########################
if __name__ == "__main__":
    personInfo = PersonInfo("sepp")

# TODO: Implement a couple of test cases
