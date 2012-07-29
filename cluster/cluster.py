#! /usr/bin/env python
##! /usr/local/bin/pypy  ### NOTE: pypy fails indeterministically sometimes
# Prepare base data for the commit cluster analysis
# (statistical operations will be carried out by R)
# WM 5. Aug 2011

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

import os
import csv
import shelve
import pickle
import os.path
import kerninfo
import argparse
from progressbar import *
from VCS import gitVCS
from PersonInfo import PersonInfo
from commit_analysis import *
from idManager import idManager

def _abort(msg):
    print(msg + "\n")
    sys.exit(-1)

def createDB(filename, git_repo, revrange, subsys_descr, rcranges=None):
    git = gitVCS();
    git.setRepository(git_repo)
    git.setRevisionRange(revrange[0], revrange[1])
    git.setSubsysDescription(subsys_descr)
    if rcranges != None:
        git.setRCRanges(rcranges)
    git.extractCommitData()

    print("Shelfing the VCS object")
    output = open(filename, 'wb')
    pickle.dump(git, output, -1)
    output.close()
    print("Finished with pickle")

#    d = shelve.open(filename)
#    d["git"] = git
#    d.close()

def readDB(filename):
#    k = shelve.open(filename)
#    git = k["git"]
#    k.close()
    pkl_file = open(filename, 'rb')
    git = pickle.load(pkl_file)
    pkl_file.close()

    return(git)

def computeSubsysAuthorSimilarity(cmt_subsys, author):
    """ Compute a similarity measure between commit and commit author
    in terms of touched subsystems (ranges from 0 to 1)."""
    
    # NOTE: This is naturally subjective because there are many
    # other choices for defining a similarity measure.
    asf = author.getSubsysFraction()

    sim = 0
    for (subsys_name, subsys_touched) in cmt_subsys.iteritems():
        sim = max(sim, asf[subsys_name]*subsys_touched)

    if  sim > 1:
        _abort("Internal error: Author/Subsystem similarity exceeds one.")

    if sim == 0:
        print("Zero similarity for {0}".format(author.getName()))
        for (subsys_name, subsys_touched) in cmt_subsys.iteritems():
            print("    {0}: {1}, {2}".format(subsys_name, subsys_touched, asf[subsys_name]))

    return sim

def computeAuthorAuthorSimilarity(auth1, auth2):
    """Compute a similarity measure (between 0 and 1) of two authors.

    The measure is derived from the subsystem activitiy of the authors.
    """
    # NOTE: Again, the definition of the similarity measure is subjectiv

    frac1 = auth1.getSubsysFraction()
    frac2 = auth2.getSubsysFraction()
    count = 0
    sim = 0

    for (subsys_name, fraction) in frac1.iteritems():
        if fraction != 0 and frac2[subsys_name] != 0:
            count += 1
            sim += fraction + frac2[subsys_name] # UB: 2

    if count > 0:
        sim /= float(2*count)
        
    if  sim > 1:
        _abort("Internal error: Author/Author similarity exceeds one.")
    
    return sim

def createStatisticalData(cmtlist, id_mgr):
    """Generate a person connection data structure from a list of commits

    cmtlist is the list of commits.
    id_mgr is an instance of idManager to handle person IDs and PersonInfo instances
    """
    
    # To obtain the full collaboration information, we need to have
    # the complete list of commits. This is why we don't compute the
    # information during the first parsing stage, but parse the
    # information in a second pass

    # With every person, we can associate statistical information into
    # which subsystems he/she typically commits, with whom he collaborates,
    # and so on. From this, we can infer further information for each
    # commit, for instance how many people working on different subststems
    # have signed off the commit, of how important the people who sign off
    # the commit are.

    # NOTE: These operations would be really apt for parallelisation
    # once every person has a unique ID (or if a thread safe ID assignment 
    # mechanism is used). However, the first pass takes much much
    # longer, so optimisations are better spent there.
    widgets = ['Pass 2/2: ', Percentage(), ' ', Bar(), ' ', ETA()]
    pbar = ProgressBar(widgets=widgets, maxval=len(cmtlist)).start()
    
    for i in range(0, len(cmtlist)):
        cmt = cmtlist[i]

        if i % 10 == 0:
            pbar.update(i)
            
        # Second, infer in which way people are involved in the commit
        ID = id_mgr.getPersonID(cmt.getAuthorName())
        pi = id_mgr.getPI(ID)
        cmt.setAuthorPI(pi)

        pi.addCommit(cmt)

        # Remember which subsystems the person touched in the role as an author
        pi.addPerformTagRelation("author", ID, cmt)
        tag_pi_list = {}
        
        for tag in tag_types:
            tag_pi_list[tag] = []
            for name in getInvolvedPersons(cmt, [tag]):
                relID = id_mgr.getPersonID(name)
                tag_pi_list[tag].append(id_mgr.getPI(relID))
                
                # Authors typically sign-off their patches,
                # so don't count this as a relation.
                if (relID != ID):
                    # Author received a sign-off etc. by relID
                    pi.addReceiveTagRelation(tag, relID)

                    # relID did a sign-off etc. to author
                    id_mgr.getPI(relID).addPerformTagRelation(tag, ID, cmt)

        cmt.setTagPIs(tag_pi_list)

    # Now that all information on tags is available, compute the normalised
    # statistics. While at it, also compute the per-author commit summaries.
    for (key, person) in id_mgr.getPersons().iteritems():
        person.computeTagStats()
        person.computeCommitStats()

    # Do another iteration pass over the commits, and compute data
    # that require the first pass results as input.
    # TODO: Think about what exactly we want to achieve here, because
    # this is the basis for data analysis with R.
    for cmt in cmtlist:
        # Compute similarity between the subsystems touched by the
        # commit, and the subsystems the author typically deals with
        author_pi = cmt.getAuthorPI()
        sim = computeSubsysAuthorSimilarity(cmt.getSubsystemsTouched(),
                                            author_pi)
        cmt.setAuthorSubsysSimilarity(sim)
        
        # Compute the similarity between author and taggers, and between
        # commit and taggers
        count = 0
        atsim = 0 # Author-tagger similarity
        tssim = 0 # Tagger-subsys similarity
        
        for (key, pi_list) in cmt.getTagPIs().iteritems():
            for pi in pi_list:
                count += 1
                atsim += computeAuthorAuthorSimilarity(author_pi, pi)
                tssim += \
                    computeSubsysAuthorSimilarity(cmt.getSubsystemsTouched(),
                                                  pi)

        if count > 0:
            atsim /= float(count)
            tssim /= float(count)

        cmt.setAuthorTaggersSimilarity(atsim)
        cmt.setTaggersSubsysSimilarity(tssim)

    return None

def emitStatisticalData(cmtlist, id_mgr, outdir):
    """Save the available information for further statistical processing.

    Several files are created in outdir:
    - Information about the commits proper (commits.txt)
    - Names/ID associations (ids.txt). This file also contains
      the per-author total of added/deleted/modified lines etc.
    - Per-Author information on relative per-subsys work distribution (id_subsys.txt)
    - Connection between the developers derived from commit tags (tags.txt)"""

    # Save information about the commits
    # NOTE: We could care about different diff types, but currently,
    # we don't. There are strong indications that it does not matter
    # at all anyway which diff algorithm we use
    out = open(os.path.join(outdir, "commits.txt"), 'wb')

    # Construct the header...
    header = "ID	ChangedFiles	AddedLines	DeletedLines	DiffSize	CmtMsgLines	CmtMsgBytes	NumSignedOffs	NumTags	"
    header += "\t".join(id_mgr.getSubsysNames() + ["general"])
    header += "\tTotalSubsys"
    header += "\tSubsys"
    header += "\tinRC"
    header += "\tAuthorSubsysSimilarity"
    header += "\tAuthorTaggersSimilarity"
    header += "\tTaggersSubsysSimilarity"
    print >>out, header

    # ... and write the values proper
    fmtstr = "\t".join(["{" + str(x) + "}" for x in range(0,9)])
    for cmt in cmtlist:
        outstr = fmtstr.format(cmt.id,
                               cmt.getChangedFiles(0),
                               cmt.getAddedLines(0),
                               cmt.getDeletedLines(0),
                               cmt.getAddedLines(0) + cmt.getDeletedLines(0),
                               cmt.getCommitMessageLines(),
                               cmt.getCommitMessageSize(),
                               getSignoffCount(cmt),
                               getSignoffEtcCount(cmt))

        subsys_touched = cmt.getSubsystemsTouched()
        subsys_count = 0
        for subsys in id_mgr.getSubsysNames() + ["general"]:
            outstr += "\t{0}".format(subsys_touched[subsys])
            subsys_count += subsys_touched[subsys]
            if subsys_touched[subsys] == 1:
                # If the commit touches more than one subsys, this
                # is obviously not unique.
                subsys_name = subsys

        outstr += "\t{0}".format(subsys_count)
        outstr += "\t{0}".format(subsys_name)
        
        if cmt.getInRC():
            outstr += "\t1"
        else:
            outstr += "\t0"

        outstr += "\t{0}".format(cmt.getAuthorSubsysSimilarity())
        outstr += "\t{0}".format(cmt.getAuthorTaggersSimilarity())
        outstr += "\t{0}".format(cmt.getTaggersSubsysSimilarity())

        out.write(outstr + "\n")
        # TODO: Continue writing here. Include at least
        # signoff-info (subsys info of signers)
        # similarity_between_author_and_signers
        # predominantly add, remove, or modify code (3-level factor)
    out.close()
    ##############

    # Export per-author subsystem information (could be included in ids.txt, but since
    # the information is basically orthogonal, we use two files.)
    out = open(os.path.join(outdir, "id_subsys.txt"), 'wb')

    header = "ID\t"
    header += "\t".join(id_mgr.getSubsysNames() + ["general"])
    print >>out, header

    for id in sorted(id_mgr.getPersons().keys()):
        outstr = "{0}\t".format(id)
        pi = id_mgr.getPI(id)
        subsys_fraction = pi.getSubsysFraction()

        for subsys in id_mgr.getSubsysNames() + ["general"]:
            outstr += "\t{0}".format(subsys_fraction[subsys])

        print >>out, outstr

    out.close()

    ##############
    # Save id/name associations together with the per-author summary statistics
    id_writer = csv.writer(open(os.path.join(outdir, "ids.txt"), 'wb'),
                           delimiter='\t',
                           quotechar='\\', quoting=csv.QUOTE_MINIMAL)
    # Header
    id_writer.writerow(["ID", "Name", "eMail", "added", "deleted", "total", "numcommits"])

    # Content
    for id in sorted(id_mgr.getPersons().keys()):
        pi = id_mgr.getPI(id)
        cmt_stat = pi.getCommitStats()
        added = cmt_stat["added"]
        deleted = cmt_stat["deleted"]
        numcommits = cmt_stat["numcommits"]
        id_writer.writerow([id, pi.getName(), pi.getEmail(), added, deleted, added + deleted,
                             numcommits])
    
    ##############
    # Store the adjaceny matrix for developer tagging, i.e., create
    # a NxN matrix in which the entry a_{i,j} denotes how often developer
    # j was tagged by developer i
    # NOTE: This produces a sparse matrix, but since the number
    # of developers is only a few thousand, it will likely not pay
    # off to utilise this fact for more efficient storage.

    out = open(os.path.join(outdir, "tags.txt"), 'wb')
    idlist = sorted(id_mgr.getPersons().keys())
    # Header
    out.write("# " +
              "\t".join([str(id_mgr.getPI(elem).getName()) for elem in idlist]) +
              "\n")
    
    # Matrix. The sum of all elements in row N describes how many
    # tags id N has received. The sum of column N states how many
    # tags were given by id N to other developers.
    for id_receiver in idlist:
        out.write("\t".join(
            [str(id_mgr.getPI(id_receiver).getActiveTagsReceivedByID(id_sender))
               for id_sender in idlist]) + "\n")

    out.close()

    return None

    

###########################################################################
# Main part
###########################################################################

def performAnalysis(dbfilename, git_repo, revrange, subsys_descr, create_db, outdir, rcranges=None):
    if create_db == True:
        print("Creating data base for {0}..{1}").format(revrange[0],
                                                        revrange[1])
        createDB(dbfilename, git_repo, revrange, subsys_descr, rcranges)
        
        
    print("Reading from data base...")
    git = readDB(dbfilename)
    cmtlist = git.extractCommitData("__main__")

    # Determine in which ways the authors are connected with each other,
    # plus some more statistical information
#    person_ids = {} # NOTE: May only be accessed via getPersonID
#    persons = {}
    id_mgr = idManager()
    id_mgr.setSubsysNames(kerninfo.subsysDescrLinux.keys())
    createStatisticalData(cmtlist, id_mgr)
                          
    # Save the results in text files that can be further processed with
    # statistical software, that is, GNU R
    emitStatisticalData(cmtlist, id_mgr, outdir)
    
##################################################################
def doKernelAnalysis(rev, outbase, git_repo, create_db):
    from_rev = "v2.6.{0}".format(rev)
    to_rev = "v2.6.{0}".format(rev+1)
    rc_start = "{0}-rc1".format(to_rev)

    outdir = os.path.join(outbase, str(rev))

    if not os.path.exists(outdir):
        try:
            os.mkdir(outdir)
        except os.error as e:
            print("Could not create output dir {0}: {1}".format(outdir, e.strerror))
            exit(-1)

    filename = os.path.join(outbase, "linux-{0}-{1}".format(rev,rev+1))
    performAnalysis(filename, git_repo, [from_rev, to_rev], kerninfo.subsysDescrLinux,
                    create_db, outdir, [[rc_start, to_rev]])

##################################################################################

parser = argparse.ArgumentParser()
parser.add_argument('repo')
parser.add_argument('outdir')
parser.add_argument('rev')
parser.add_argument('--create_db', action='store_true')
args = parser.parse_args()

git_repo = args.repo
outbase = args.outdir
try:
    rev = int(args.rev)
except ValueError:
    print "Cannot parse revision!"
    exit(-1)

doKernelAnalysis(rev, outbase, git_repo, args.create_db)
exit(0)

#git_repo = "/Users/wolfgang/git-repos/linux/.git"
#outbase = "/Users/wolfgang/papers/csd/cluster/res/"
#rev = 32
#doKernelAnalysis(rev, outbase, git_repo, False)
#exit(0)

#for rev in range(30,39):
#    doKernelAnalysis(rev, outbase, git_repo, True)
#exit(0)


########################### Some (outdated) examples ################
# Which roles did a person fulfill?
for person in persons.keys()[1:10]:
    tag_stats = persons[person].getTagStats()
    print("Name: {0}".format(persons[person].getName()))
    stats_str = ""
    for (tag, count) in tag_stats.iteritems():
        stats_str += "({0}, {1}) ".format(tag, count)
    print("   {0}".format(stats_str))


# Which subsystems was the person involved in as author?
for person in persons.keys()[1:10]:
    subsys_stats = persons[person].getSubsysStats()["author"]
    print("Name: {0}".format(persons[person].getName()))

    stats_str = ""
    for (subsys, count) in subsys_stats.items():
        stats_str += "({0}, {1}) ".format(subsys, count)

    print("   {0}".format(stats_str))
            
# TODO: Compute the grand total for all subsystems except "author"
# to see in which ones the developer was involved as a non-author

# With which others did the person cooperate?
print("ID\tcdate\tAddedLines\tSignoffs\tCmtMsgSize\tChangedFiles\t")
for ID, pi  in persons.items()[1:10]:
    print("ID: {0}, name: {1}".format(pi.getID(), pi.getName()))
    print("  Signed-off-by:")
    for relID, count in pi.getPerformTagRelations("Signed-off-by").iteritems():
        print("    person: {0}, count: {1}".format(persons[relID].getName(),
                                                    count))
    
    

