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
# Copyright 2010, 2011, 2012 by Wolfgang Mauerer <wm@linux-kernel.net>
# All Rights Reserved.

from commit_analysis import tag_types, active_tag_types

class PersonInfo:
    """ Information about a commiter, and his relation to other commiters"""
    
    def __init__(self, subsys_names = [], ID=None, name="", email=""):
        self.ID = ID
        self.name = name
        self.email = email
        self.subsys_names = subsys_names
        
        #Edges FROM other developers (could be committers or authors) 
        self.inEdges = {}
        
        #Edges TO other developers (could be committers or authors)
        self.outEdges = {}
        
        #average of collaboration metric for a single Id
        #key = person ID, value = average edge weight
        self.inEdgesAvg = {}
        self.outEdgesAvg = {}
        
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
        self.all_tags_received_by_id = {}

        # Count how many active tags (without "passive" categories like CC)
        # have beenreceived form a specific ID.
        self.active_tags_received_by_id = {}


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

    def _getTagsReceivedByID(self, tag_hash, ID):
        if ID in tag_hash.keys():
            return tag_hash[ID]
        else:
            return 0
        
    def addInEdge(self, Id, value): 
        '''
        a Edge from some else towards this person
        '''
        self.addEdge(Id, self.inEdges, value)
        
        
    def addOutEdge(self, Id, value):
        '''
        a Edge from this person to someone else        
        '''
        self.addEdge(Id, self.outEdges, value)
    
        
    def edgeProcessing(self):
        '''
        This function should be call after all collaboration metrics have 
        been calculated. Performs basics statistics calculations that require 
        the entire collaboration network data present. 
        '''
        
        #average over Edges 
        for Id in self.inEdges.keys():
            self.inEdgesAvg[Id]  = sum( self.inEdges[Id] )   / len( self.inEdges[Id] )  * 1.0
            
        for Id in self.outEdges.keys():
            self.outEdgesAvg[Id] = sum( self.outEdges[Id] )  / len( self.outEdges[Id] )  * 1.0
        
        
    def addEdge(self, ID, edges, value):
        '''
        the direction of the Edge is made by calling this 
        with addInEdge or addOutEdge. the Value parameter 
        indicates the weight of the relationship.
        '''
        
        if(ID in edges):
            edges[ID].append(value)
        else:
            edges[ID] = [value] 
        
    def getActiveTagsReceivedByID(self, ID):
        return self._getTagsReceivedByID(self.active_tags_received_by_id, ID)

    def getSumInEdge(self, ID):
        
        if ID in self.inEdges:
            #sum over all calculated edge weights for this person
            return sum(self.inEdges[ID])
        else:
            return 0

    def getSumOutEdge(self, ID):
        
        if ID in self.outEdges:
            #sum over all calculated edge weights for this person
            return sum(self.outEdges[ID])
        else:
            return 0
        
    def getAvgInEdge(self, ID):
        
        if ID in self.inEdgesAvg:
            
            return self.inEdgesAvg[ID]
        
        else:
            return 0
       
    def getAvgOutEdge(self, ID):
        
        if ID in self.outEdgesAvg:
            
            return self.outEdgesAvg[ID]
        
        else:
            return 0

    def getAllTagsReceivedByID(self, ID):
        return self._getTagsReceivedByID(self.all_tags_received_by_id, ID)

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

    # Helper for computeTagStats, see below
    def _sum_associations(self, tag, rcv_by_id_hash):
        for ID in self.associations[tag]:
            if ID in rcv_by_id_hash:
                rcv_by_id_hash[ID] += self.associations[tag][ID]
            else:
                rcv_by_id_hash[ID] = self.associations[tag][ID]

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
            # An author is supposed to sign-off at least his own commits
            print("{0} did not tag on any subsystem?!".format(self.getName()))

        # Summarise the tags given _to_ (i.e, received by) the developer
        # from a specific ID
        for tag in self.associations.keys():
            self._sum_associations(tag, self.all_tags_received_by_id)

        # Active tags do not include things like CC, which can
        # be issued without the second party's consent
        for tag in active_tag_types:
            self._sum_associations(tag, self.active_tags_received_by_id)

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
