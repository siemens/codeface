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
# Copyright 2010, 2011 by Wolfgang Mauerer <wm@linux-kernel.net>
# Copyright 2012, 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.

from logging import getLogger; log = getLogger(__name__)
from prosoda.commit_analysis import tag_types, active_tag_types, proximity_relation \
, committer2author_relation, all_link_types

class PersonInfo:
    """ Information about a commiter, and his relation to other commiters"""

    def __init__(self, subsys_names = [], ID=None, name="", email=""):
        self.ID = ID
        self.name = name
        self.email = email
        self.subsys_names = subsys_names

        # Store from which developers the person received a tag
        self.associations = {}
        for link_type in all_link_types:
            self.associations[link_type] = {}

        # See addSendRelation on the meaning of the following
        self.inv_associations = {}
        self.tagged_commits = {}
        for link_type in all_link_types + ["author"]:
            self.inv_associations[link_type] = {}

        # See computeTagStats()
        for tag in tag_types + ["author"]:
            self.tagged_commits[tag] = []
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
        for link_type in all_link_types + ["author"]:
            self.subsys_touched[link_type] = {}

            # General is used if the commit does not touch any well-defined
            # subsystem(s), for instance when a generic header is modified.
            for subsys in subsys_names + ["general"]:
                self.subsys_touched[link_type][subsys] = 0
                self.subsys_touched[link_type]["general"] = 0

        # Count how often the person has made a link to someone else (i.e., given a
        # signed-off, made a commit in close proximity, committed someone code)
        self.linksPerformed = 0

        # Count how many tags (independent of tag category) have been
        # received form a specific ID.
        self.all_tags_received_by_id = {}

        # Count how many active tags (without "passive" categories like CC)
        # have beenreceived form a specific ID.
        self.active_tags_received_by_id = {}

        #count how many links based on the proximity metric were received by
        #a given ID
        self.proximity_links_recieved_by_id = {}

        #count how many links based on committer -> author were received by
        #a given ID
        self.committer_links_recieved_by_id = {}

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

    def _getLinksReceivedByID(self, link_hash, ID):
        if ID in link_hash.keys():
            return link_hash[ID]
        else:
            return 0

    def getActiveTagsReceivedByID(self, ID):
        return self._getLinksReceivedByID(self.active_tags_received_by_id, ID)

    def getLinksReceivedByID(self, ID, link_type):
        if link_type == proximity_relation:
            return self._getLinksReceivedByID(self.proximity_links_recieved_by_id, ID)
        elif link_type == committer2author_relation:
            return self._getLinksReceivedByID(self.committer_links_recieved_by_id, ID)

    def getAllTagsReceivedByID(self, ID):
        return self._getTagsReceivedByID(self.all_tags_received_by_id, ID)

    def addRelation(self, relation_type, ID, assoc, weight=1):
        """State that the person has received or given a tag from/to ID.

        The distinction between taking and giving is made in other
        functions."""

        if (ID in assoc[relation_type]):
            assoc[relation_type][ID] += weight
        else:
            assoc[relation_type][ID] = weight

    def addReceiveRelation(self, relation_type, ID, weight=1):
        '''
        add a one directional relation from the person identified by
        ID and this person instance (ie. self)
        eg. ID ----> self
        the weight parameter specified the edge strength
        '''

        self.addRelation(relation_type, ID, self.associations, weight)

    def addSendRelation(self, relation_type, ID, cmt, weight=1):
        '''
        add a one directional relation from the person instance
        (ie. self) and the person identified by ID
        eg. self ----> ID
        the weight parameter specified the edge strength
        '''

        self.addRelation(relation_type, ID, self.inv_associations)

        if relation_type in tag_types:
            self.tagged_commits[relation_type].append(cmt.id)

        self.linksPerformed +=1
        self.addCmt2Subsys(cmt, relation_type)

    def addCmt2Subsys(self, cmt, relation_type):
        '''record which subsystem the commit was made to and what type of
        link was performed (proximity, tag, committed)'''

        cmt_subsys = cmt.getSubsystemsTouched()
        for subsys in cmt_subsys:
            self.subsys_touched[relation_type][subsys] += cmt_subsys[subsys]

    def getPerformTagRelations(self, relation_type):
        return self.inv_associations[relation_type]

    def getSubsysFraction(self):
        return self.subsys_fraction

    # Helper for computeTagStats, see below
    def _sum_relations(self, relation_type, rcv_by_id_hash):
        for ID in self.associations[relation_type]:
            if ID in rcv_by_id_hash:
                rcv_by_id_hash[ID] += self.associations[relation_type][ID]
            else:
                rcv_by_id_hash[ID] = self.associations[relation_type][ID]

    def computeStats(self, link_type):

        #computer tag specific stats
        if link_type == "Tag":
            self.computeTagStats()

        #determine fraction of relation types
        #for each subsystem
        self.computeSubsysFraction()

        #sum over the relation types
        self.computeRelationSums()

    def computeTagStats(self):
        """Compute statistics inferred from the tag information.

        While this can be called anytime, it makes sense to call the function
        only once all link data have been collected."""

        if self.linksPerformed == 0:
            log.warning("{0} did not perform any links?!".
                  format(self.getName()))
            return

#        print("{0} performed {1} tags".format(self.getName(),
#                                               self.tagsPerformed))

        # Per-author distribution of tags (e..g, 30% Signed-Off, 60%
        # CC, 10% Acked-By)
        for tag in tag_types + ["author"]:
            self.tag_fraction[tag] = \
                len(self.tagged_commits[tag])/float(self.linksPerformed)


    def computeSubsysFraction(self):

        total_links = 0
        # Summarise over all different link variants
        for subsys in self.subsys_names + ["general"]:
            self.subsys_fraction[subsys] = 0

            for link_type in all_link_types + ["author"]:
                self.subsys_fraction[subsys] += \
                    self.subsys_touched[link_type][subsys]

            total_links += self.subsys_fraction[subsys]

        # ... and normalise accordingly
        if (total_links != 0):
            for subsys in self.subsys_names + ["general"]:
                self.subsys_fraction[subsys] /= float(total_links)

    def computeRelationSums(self):
        # Summarise the links given _to_ (i.e, received by) the developer
        # from a specific ID
        for tag in tag_types:
            self._sum_relations(tag, self.all_tags_received_by_id)

        # Active tags do not include things like CC, which can
        # be issued without the second party's consent
        for tag in active_tag_types:
            self._sum_relations(tag, self.active_tags_received_by_id)

        #sum other possible link types
        self._sum_relations(proximity_relation,        self.proximity_links_recieved_by_id)
        self._sum_relations(committer2author_relation, self.committer_links_recieved_by_id)


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
