#! /usr/bin/env python
# Prepare base data for the commit cluster analysis
# (statistical operations will be carried out by R)

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
import os
import csv
import shelve
import pickle
import os.path
import argparse
import codeBlock
import codeLine
import math
import random
from progressbar import ProgressBar, Percentage, Bar, ETA
from logging import getLogger; log = getLogger(__name__)

from prosoda import kerninfo
from prosoda.commit_analysis import (getSignoffCount, getSignoffEtcCount,
        getInvolvedPersons, tag_types)
from prosoda.VCS import gitVCS
from prosoda.dbmanager import DBManager, tstamp_to_sql
from .PersonInfo import PersonInfo
from .idManager import idManager

#Global Constants
SEED = 448

#enum-like class to distinguish between the various
#methods used to link individuals
class LinkType:
    tag              = "tag"
    proximity        = "proximity"
    committer2author = "committer2author"
    file             = "file"


def createDB(filename, git_repo, revrange, subsys_descr, link_type, rcranges=None):
    #------------------
    #configuration
    #------------------
    git = gitVCS();
    git.setRepository(git_repo)
    git.setRevisionRange(revrange[0], revrange[1])
    git.setSubsysDescription(subsys_descr)

    if rcranges != None:
        git.setRCRanges(rcranges)

    #------------------------
    #data extraction
    #------------------------
    git.extractCommitData(link_type=link_type)

    #------------------------
    #save data
    #------------------------
    log.devinfo("Shelving the VCS object")
    output = open(filename, 'wb')
    pickle.dump(git, output, -1)
    output.close()
    log.devinfo("Finished shelving the VCS object")


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
        log.critical("Author/Subsystem similarity exceeds one.")
        raise Exception("Author/Subsystem similarity exceeds one.")

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
        log.critical("Author/Subsystem similarity exceeds one.")
        raise Exception("Author/Subsystem similarity exceeds one.")

    return sim


def computeSnapshotCollaboration(file_commit, cmtList, id_mgr, link_type,
                                  startDate=None, random=False):
    '''Generates the collaboration data from a file snapshot at a particular
    point in time'''

    '''
    Detailed description: the fileSnapShot is a representation of how a file
    looked at the time of a particular commit. The fileSnapshot is a
    dictionary with key = a particular commit hash and the value is the how
    the file looked at the time of that commit.How the file looked is
    represented by a another dictionary with key = a code line number and the
    value is a commit hash referencing the commit that contributed that
    particular line. The commit hashes are then used to reference the people
    involved.
    '''

    #------------------------
    #variable declarations
    #------------------------
    maxDist     = 25
    author      = True
    fileState   = file_commit.getFileSnapShot()
    revCmtIds   = file_commit.getrevCmts()
    revCmts     = [cmtList[revCmtId] for revCmtId in revCmtIds]

    for cmt in revCmts:
        # the fileState will be modified but for each loop we should start with
        # the original fileState
        fileState_mod = fileState.copy()

        # check if commit is in the current revision of the file, if it is not
        # we no longer have a need to process further since the commit is now
        # irrelevant
        if not(cmt.id in fileState_mod.values()):
            continue

        #find code lines of interest, these are the lines that are localized
        #around the cmt.id hash, modify the fileState to include only the
        #lines of interest
        if(not(random)):
            fileState_mod = linesOfInterest(fileState_mod, cmt.id, maxDist,
                                            cmtList, file_commit)

        #remove commits that occur prior to the specified startDate
        if startDate != None:
            fileState_mod = removePriorCommits(fileState_mod, cmtList, startDate)

        #collaboration is meaningless without more than one line
        #of code
        if len(fileState_mod) > 1:

            # identify code line clustering using function location information
            clusters = groupFuncLines(file_commit, fileState_mod, cmtList)

            #calculate the collaboration coefficient for each code block
            [computeCommitCollaboration(cluster, cmt, id_mgr, link_type,
                                        maxDist, author) for cluster in clusters if cluster]


def groupFuncLines(file_commit, file_state, cmtList):
    '''
    cluster code lines that fall under the same function
    '''
    func_indx = {}
    indx      = 0
    func_blks = []
    lines     = sorted( map( int, file_state.keys() ) )
    blk_start = lines[0]
    blk_end   = blk_start

    for func_id in file_commit.functionIds.values():
        func_indx[func_id] = indx
        func_blks.append([])
        indx += 1

    for i in range(0,len(file_state) - 1):
        curr_line  = lines[i]
        next_line  = lines[i+1]
        curr_cmt_id = file_state[str(curr_line)]
        next_cmt_id = file_state[str(next_line)]
        curr_func_id = file_commit.findFuncId(curr_line)
        next_func_id = file_commit.findFuncId(next_line)
        curr_func_indx = func_indx[curr_func_id]
        next_func_indx = func_indx[next_func_id]
        if (curr_cmt_id == next_cmt_id) and (curr_func_id == next_func_id) \
        and (curr_line + 1 == next_line):
            blk_end = blk_end + 1
        else:
            func_blks[curr_func_indx]. \
            append(codeBlock.codeBlock(blk_start, blk_end,
                   cmtList[str(curr_cmt_id)].getAuthorPI().getID(),
                   cmtList[str(curr_cmt_id)].getCommitterPI().getID(),
                   curr_cmt_id))
            blk_start = next_line
            blk_end   = blk_start

    # boundary case
    func_blks[next_func_indx].append(codeBlock.codeBlock(blk_start, blk_end,
                            cmtList[str(next_cmt_id)].getAuthorPI().getID(),
                            cmtList[str(next_cmt_id)].getCommitterPI().getID(),
                            next_cmt_id))

    return func_blks

def randomizeCommitCollaboration(codeBlks, fileState):
    '''
    randomizes the location in the file where commits were made
    '''

    '''
    Commits made to a file and the line number for the commits are
    captured prior to using this function. This function will randomize
    the location (line number) where the commits were made. The idea behind
    this is to see if people really do make preferential attachment to
    people who are working on related code or the interactions seen from
    commit activity is random. If we see the no difference in the significance
    of the community structure between randomized and non randomized tests then
    we can assume interactions appear to be random. In other words the commiters
    are not collaboration with people near their code anymore than someone making
    commits far from their code.

    - Input -
    codeBlks: a set of codeBlock objects
    fileState: the original file that the code blocks were found from,
                this is a dictionary that maps code line numbers to commit hashes
   - Output -
   randCodeBlks: the randomized codeBlock objects
    '''
    random.seed(SEED)

    #get number of lines of code in the file
    fileLen = len(fileState)
    codeLineNum = range(1, fileLen + 1)

    #randomly sample the code blocks
    codeBlksRand = random.sample(codeBlks, len(codeBlks))

    #assign code line ranges to the randomized code blocks
    #we map consecutive line numbers to the blocks
    #effectively we have randomized the code block organization
    #in the file
    for codeBlk in codeBlksRand:

        #get the range that is spanned by the code block
        blkSpan = codeBlk.end - codeBlk.start + 1

        #extract new code line range
        newCodeLineRange = codeLineNum[0:blkSpan]

        codeBlk.start = newCodeLineRange[0]
        codeBlk.end   = newCodeLineRange[-1]

        #remove selected range from random sampled vector
        for i in newCodeLineRange:
            codeLineNum.remove(i)


        #end for i
    #end for codeBlk

    return codeBlksRand


def computeCommitCollaboration(codeBlks, cmt, id_mgr, link_type, maxDist,
                               author=False):
    '''
    Computes a value that represents the collaboration strength
    between a commit of interest and every other commit that
    contributed code in close proximity the commit of interests
    contributions. The method computes all possible combinations
    of code block relationships then averages. This is very similar
    to the function "computerPersonsCollaboration" except we consider
    the commit hash to identify the contribution instead of the person
    then later map the commit to a person. The advantages is we can
    differentiate between when an author made a contribution. This way
    we can identify when an author/committer is collaborating with
    oneself. A strong collaboration with oneself would possibly
    suggest a special case where other developers don't want to
    or cannot contribute. This information may be significant and
    we should try and capture it. This information is not possible
    to capture with "computePersonsCollaboration" because when we
    first map commit hashes to a person we lose the ability to resolve
    different (in time) contributions by a person.
    - Input -
    codeBlks - a collection of codeBlock objects
    cmt      - the commit object of the revision we are interested in
                measuring the collaboration for
    id_mgr   - manager for people and information relevant to them
                the collaboration metric is stored in this object
    maxDist  - maximum separation of code for consideration, beyond
               this distance the code block is ignored in the
               calculation
    author   - if true, then commits authors are considered for collaboration
                if false then commit committers are considered for collaboration

    '''
     #variable declarations
    relStrength  = {} # key = unique person id, value = Edge strength to personId
    personIdBlks = [] # blocks that were contributed by personId
    contribIdSet = [] # a set of all ids that contributed to codeBlks


    #get all blocks contributed by the revision commit we are looking at
    revCmtBlks = [blk for blk in codeBlks if blk.cmtHash == cmt.id]

    #get the person responsible for this revision
    if author:
        revPerson = id_mgr.getPI( revCmtBlks[0].authorId )
    else:
        revPerson = id_mgr.getPI( revCmtBlks[0].committerId )

    #find all other commit ids for older revisions
    oldCmtIdSet = set( [blk.cmtHash for blk in codeBlks
                        if blk.cmtHash != cmt.id] )

    #calculate relationship between personId and all other contributors
    for oldCmtId in oldCmtIdSet:

        #get all blocks for the oldCmtId
        oldRevBlks = [blk for blk in codeBlks if blk.cmtHash == oldCmtId]

        # collaboration strength is seen as the sum of the newly contributed
        # lines of code and previously committed code by the other person
        collaboration_strength = computeBlksSize(revCmtBlks, oldRevBlks)

        #store result
        if author:
            personId = oldRevBlks[0].authorId
        else:
            personId = oldRevBlks[0].committerId

        inEdgePerson = id_mgr.getPI(personId)
        revPerson.addSendRelation      (link_type, personId, cmt,
                                        collaboration_strength)
        inEdgePerson.addReceiveRelation(link_type, revPerson.getID(),
                                        collaboration_strength)


def computePersonsCollaboration(codeBlks, personId, id_mgr, maxDist):
    '''
    Computes a value that represents the collaboration strength
    between a person of interest and every other person that
    contributed code in close proximity the person of interests
    contributions. The method computes all possible combinations
    of code block relationships then averages.
    - Input -
    codeBlks - a collection of codeBlock objects
    personId - a unique identifier of an individual who contributed
               to at least one code block in codeBlks
    id_mgr   - manager for people and information relevant to them
                the collaboration metric is stored in this object
    maxDist  - maximum separation of code for consideration, beyond
               this distance the code block is ignored in the
               calculation
    '''
    #variable declarations
    relStrength  = {} # key = unique person id, value = Edge strength to personId
    personIdBlks = [] # blocks that were contributed by personId
    contribIdSet = [] # a set of all ids that contributed to codeBlks
    person       = id_mgr.getPI(personId) #all Edges are outwards from this person

    #get all blocks contributed by personId
    personIdBlks = [blk for blk in codeBlks if blk.id == personId]

    #find all contributors id
    contribIdSet = set( [blk.id for blk in codeBlks] )

    #calculate relationship between personId and all other contributors
    for Id in contribIdSet:

        #do not compute collaboration with oneself
        if Id == personId:
            continue


        #get all blocks by contributor Id
        IdBlocks = [blk for blk in codeBlks if blk.id == Id]

        #compute relationship strength for ALL combinations of blocks
        allCombStrengths  = [computeEdgeStrength(blk1, blk2, maxDist)
                             for blk1 in IdBlocks for blk2 in personIdBlks]

        #average the strengths
        avgStrength = sum(allCombStrengths) / len(allCombStrengths) * 1.0

        #store result
        inEdgePerson = id_mgr.getPI(Id)
        person.addOutEdge(   Id   , avgStrength)
        inEdgePerson.addInEdge(personId, avgStrength)


def computeBlksSize(blks1, blks2):
    # compute the total size of two sets of codeBlock objects
    blks_total = blks1 + blks2
    size_total = 0
    for blk in blks_total:
        size_total += blk.end - blk.start + 1
    return size_total


def computeEdgeStrength(blk1, blk2, maxDist):
    '''
    Calculates a value that indicates how strongly the two
    code blocks are related based on proximity
    - Input -
    blk1, blk2: codeBlock objects
    maxDist: the maximum distance two blocks
             may be separated
    - Output -
    EdgeStrength: a floating point number representing how related
                  the two code blocks are
    '''

    #calculate the degree of separation between the code blocks
    #in terms of lines of code
    dist = blockDist(blk1, blk2)

    if dist < maxDist:
        edgeStrength = 0.5 + (math.cos( (math.pi * dist) / maxDist ) / 2.0)

    else:
        edgeStrength = 0


    return edgeStrength


def simpleCluster(codeBlks, snapShotCmt, maxDist, author=False):
    '''
    Group the code blocks into clusters, this an
    ad hoc simple method. The goal is to group code
    into clusters around the commit of interest and we
    are trying to identify related code based on proximity
    to the commit of interest.

    -- Input --
    codeBlks: an array of codeBlock objects for a particular file
    snapShotCmt: the commit object which marks the point in time when
                 the file contents (contained in codeBlks) were
                 acquired
   -- Output --
   blkClusters - a collection of clusters, each cluster contains a subset of the
                 codeBlks input array
   '''
    #=========================================================
    #get the id of the person of interest, that is the one
    #which made the contribution of interest
    #=========================================================
    #personId = None
    #if author:
    #    personId = snapShotCmt.getAuthorPI().getID()

    #else:
    #    personId = snapShotCmt.getCommitterPI().getID()
    cmtId = snapShotCmt.id


    #find code blocks that correspond to personId
    indx = 0
    blksOfInterest = []
    otherBlks = []

    for blk in codeBlks:

        if blk.cmtHash == cmtId:
            blksOfInterest.append(indx)

        else:
            otherBlks.append(indx)

        indx += 1

    #=========================================================
    #determine what code blocks of interest should be clustered
    #together, penalize not forming a new cluster based on
    #distance between farthest apart blocks
    #take advantage of the fact that the blks are sorted, we don't
    #have to compare all blocks to each other (n choose 2)
    #=========================================================
    #initialize variables
    blkClusters = [] #a collection of clusters
    cluster = [] #a collection of blocks that constitute a single cluster
    clusterStartBlk = codeBlks[ blksOfInterest[  0  ] ] #beginning of a cluster
    cluster.append(clusterStartBlk) #add the first block to the first cluster

    for i in range(len(blksOfInterest) - 1):
        #find the next block of interests
        #recall blksOfInterest provides the index to the
        #blocks of interest in the codeBlocks array
        nextBlk = codeBlks[ blksOfInterest[ i+1 ] ]

        #find the distance between current and next blocks
        dist = blockDist(clusterStartBlk, nextBlk)

        #check if the block is close enough to be considered
        #part of the same cluster
        if (dist <= maxDist):
            cluster.append(nextBlk)

        else: #block is too far away, save current cluster and start new cluster
            blkClusters.append(cluster) #add current cluster to collection
            cluster = [] #start new cluster
            clusterStartBlk = nextBlk #set next cluster to start at nextBlk
            cluster.append(clusterStartBlk) #add blk to next cluster

    #add final cluster
    blkClusters.append(cluster)


    #=========================================================
    #for the remaining blocks assign them to the appropriate
    #on the basis of nearest cluster to the block
    #we always want the distance measurement to be w.r.t. the
    #commit of interest blocks, makes for a messy initialization
    #=========================================================
    #TODO: rewrite this, use the fact that no distance measure
    #is necessary when blk index falls between a single clusters
    #blk indices
    finalClusterIdx = len(blkClusters) - 1

    currClusterIdx      = 0
    currCluster         = blkClusters[currClusterIdx]
    currClusterStartBlk = currCluster[ 0] #first block in cluster
    currClusterEndBlk   = currCluster[-1] #last block in cluster

    nextClusterIdx = currClusterIdx + 1

    if(nextClusterIdx <= finalClusterIdx):

        nextCluster         = blkClusters[nextClusterIdx]
        nextClusterStartBlk = nextCluster[ 0] #first block in cluster
        nextClusterEndBlk   = nextCluster[-1] #last block in cluster


    for blkIdx in otherBlks:


        if(nextClusterIdx > finalClusterIdx):

            #assign the rest of the blks to the current cluster
            blkClusters[currClusterIdx].append( codeBlks[blkIdx] )

        else:
            nextCluster         = blkClusters[nextClusterIdx]
            nextClusterStartBlk = nextCluster[ 0] #first block in cluster
            nextClusterEndBlk   = nextCluster[-1] #last block in cluster

            blk = codeBlks[blkIdx]

            #calculate distance from cluster
            currClusterDist = min( blockDist(currClusterStartBlk, blk),
                                   blockDist(currClusterEndBlk, blk) )
            nextClusterDist = min( blockDist(nextClusterStartBlk, blk),
                                   blockDist(nextClusterEndBlk, blk) )

            if( currClusterDist <= nextClusterDist ): #block falls within this cluster
                blkClusters[currClusterIdx].append( codeBlks[blkIdx] )

            else: #move onto next cluster
                #reinitialize current cluster settings
                currClusterIdx      = nextClusterIdx
                nextClusterIdx      = currClusterIdx + 1

                currCluster         = blkClusters[currClusterIdx]
                currClusterStartBlk = currCluster[ 0] #first block in cluster
                currClusterEndBlk   = currCluster[-1] #last block in cluster

                blkClusters[currClusterIdx].append( codeBlks[blkIdx] )




    return blkClusters


def removePriorCommits(fileState, clist, startDate):
    '''
    removes commits that occured prior to a startDate

    - Input -
    fileState: dictionary, key = code line number and value =
                commit hash for that line
    clist:     list of all commit objects, referenced by commit hash
    startDate: all commits older than this date are removed

    - Output -
    modFileState: a modified fileState with all lines commited prior to
                  the startDate removed
    '''

    #variable declarations
    modFileState = {}

    for (lineNum, cmtId) in fileState.items():

        if cmtId in clist:
            #get commit object containing commit date
            cmtObj = clist[cmtId]

            if( cmtObj.getCdate() >= startDate ):
                modFileState[lineNum] = cmtId
        #else:
            # if the commit is not found in clist then we know it is a commit
            # made before the startDate and we can ignore it


    return modFileState


def linesOfInterest(fileState, snapShotCommit, maxDist, cmtlist, file_commit):
    '''
    Finds the regions of interest for analyzing the file.
    We want to look at localized regions around the commit of
    interest (snapShotCommit) and ignore code lines that are
    located some far distance away.

    - Input -
    fileState:      code line numbers together with commit hashes
    snapShotCommit: the commit hash that marks when the fileState was acquired
    maxDist:        indicates how large the area of interest should be
    file_commit: a fileCommit instance
    - Output -
    modFileState: the file state after line not of interest are removed
    '''
    #variable declarations
    fileMaxLine = int(max(fileState.keys(), key=int))
    fileMinLine = int(min(fileState.keys(), key=int))
    snapShotCmtDate = cmtlist[snapShotCommit].getCdate()
    linesSet    = set()
    modFileState = {}
    snapshot_func_set = set()

    #take a pass over the fileState to identify where the snapShotCommit
    #made contributions to the fileState
    snapShotCmtLines = []
    for lineNum in fileState.keys():

        cmtId = fileState[lineNum]

        if cmtId == snapShotCommit:
            snapShotCmtLines.append(lineNum)
            # retrieve the function id that each line falls into
            snapshot_func_set.add(file_commit.findFuncId(int(lineNum)))
    #end for line

    # remove lines that are from commits that occur after the snapShotCmt
    for lineNum, cmtId in fileState.items():
        if cmtId in cmtlist:
            cmtDate = cmtlist[cmtId].getCdate()
        else:
            #must be a old commit that occurred in a prior release
            continue

        # check to keep lines committed in the past with respect to the current
        # snapshot commit
        if cmtDate <= snapShotCmtDate:
            # check if the line will fall under one of the functions that the
            # snapshot commit lines fall under (ie. we only want to keep lines
            # that are in the same functions as the snapshot commit
            if file_commit.findFuncId(int(lineNum)) in snapshot_func_set:
                modFileState[lineNum] = fileState[lineNum]

            # else: ignore line since it belongs to some function outside of
            # the set of functions we are interested in

        #else: forget line because it was in a future commit

    return modFileState


def blockDist(blk1, blk2):
    '''
    Finds the euclidean distance between two code blocks.
    This is the positive distance from the start of one block
    to the end of the second block.
    '''
    #TODO: throw exception for overlapping blocks and identical blocks
    #currently if this is called with identical blocks distance = -1
    dist = 0

    if(blk1.start > blk2.end):

        dist = blk1.start - blk2.end

    else:
        dist = blk2.start - blk1.end


    return (dist - 1) #subtract 1 so that adjacent blocks have a distance of zero


def findCodeBlocks(fileState, cmtList, author=False):
    '''
    Finds code blocks for a given file state, a code block is defined by the
    start and end line numbers for contiguous lines with a single author or
    committer.  If author is set to true then code blocks are found based on
    author of commit otherwise committer identifier is used.
    '''
    #---------------------
    #variable definitions
    #---------------------
    codeLines = {} #key = line number, value = codeLine object


    #-----------------------------------------------------
    #find out who is responsible (unique ID) for each line
    #of code for the file snapshot
    #------------------------------------------------------
    for (key, cmtId) in fileState.items():

       lineNum = int(key)

       #get author and committer unique identifier
       authorId    = cmtList[str(cmtId)].getAuthorPI().getID()
       committerId = cmtList[str(cmtId)].getCommitterPI().getID()

       #assign the personID to the line number
       codeLines[ lineNum ] = codeLine.codeLine(lineNum, cmtId, authorId,
                                                committerId)

    #------------------------
    #find contiguous lines
    #------------------------
    #TODO: check if sorting is actually necessary (in most cases I presume not)
    lineNums = sorted( map( int, fileState.keys() ) )

    blkStart   = lineNums[0]
    blkEnd     = lineNums[0]
    codeBlocks = []
    for i in range(len(lineNums) - 1):

       #get the next and current line information
       nextLineNum = lineNums[i+1]
       currLineNum = lineNums[i]

       currCodeLine = codeLines[ currLineNum ]
       nextCodeLine = codeLines[ nextLineNum ]

       currCmtId = currCodeLine.get_cmtHash()
       nextCmtId = nextCodeLine.get_cmtHash()

       #we have to check for contiguous lines
       if( nextLineNum != (currLineNum + 1) ): #not contiguous line

           #save code block span for prior contributor
           codeBlocks.append( codeBlock.codeBlock(blkStart, blkEnd,
                                                  currCodeLine.authorId,
                                                  currCodeLine.committerId,
                                                  currCodeLine.cmtHash) )

           #reinitialize start and end for next
           #contributors block
           currCmtId = nextCmtId
           blkStart  = nextLineNum
           blkEnd    = blkStart


       else: #lines are contiguous

           #check if next line has same commit hash we assume that commits
           #contain code that related, so even if the author is the same but a
           #different commit then the code is not consider to be accomplishing
           #one related task necessarily
           if currCmtId == nextCmtId:
               #increment the line count
               blkEnd += 1


           else: #different contributor

               #save code block span for prior contributor
               codeBlocks.append( codeBlock.codeBlock(blkStart, blkEnd,
                                                      currCodeLine.authorId,
                                                      currCodeLine.committerId,
                                                      currCodeLine.cmtHash) )


               #reinitialize start and end for next
               #contributors block
               currCmtId = nextCmtId
               blkStart  = nextLineNum
               blkEnd    = blkStart


    #take care of boundary case
    #save final block span for prior contribution
    codeBlocks.append( codeBlock.codeBlock(blkStart, blkEnd,
                                           nextCodeLine.authorId,
                                           nextCodeLine.committerId,
                                           nextCodeLine.cmtHash) )

    return codeBlocks


def createStatisticalData(cmtlist, id_mgr, link_type):
    """Generate a person connection data structure from a list of commits

    cmtlist is the list of commits.
    id_mgr is an instance of idManager to handle person IDs and PersonInfo instances
    """

    # Now that all information on tags is available, compute the normalised
    # statistics. While at it, also compute the per-author commit summaries.
    for (key, person) in id_mgr.getPersons().iteritems():
            person.computeCommitStats()
            person.computeStats(link_type)

    computeSimilarity(cmtlist)

    return None


def writeCommitData2File(cmtlist, id_mgr, outdir, releaseRangeID, dbm, conf):
    '''
    commit information is written to the outdir location
    '''

    # Save information about the commits
    # NOTE: We could care about different diff types, but currently,
    # we don't. There are strong indications that it does not matter
    # at all anyway which diff algorithm we use
    projectID = dbm.getProjectID(conf["project"], conf["tagging"])

    # Clear the commit information before writing new commints
    dbm.doExec("DELETE FROM commit WHERE projectId=%s AND releaseRangeId=%s",
               (projectID, int(releaseRangeID)))

    for cmt in cmtlist:
        subsys_touched = cmt.getSubsystemsTouched()
        subsys_count = 0
        for subsys in id_mgr.getSubsysNames() + ["general"]:
            subsys_count += subsys_touched[subsys]
            if subsys_touched[subsys] == 1:
                # If the commit touches more than one subsys, this
                # is obviously not unique.
                subsys_name = subsys

        if cmt.getInRC():
            inRC=1
        else:
            inRC=0

        dbm.doExec("INSERT INTO commit " +
                   "(commitHash, commitDate, author, authorDate,"
                   " authorTimezone, projectId, ChangedFiles, " +
                   #
                   "AddedLines, DeletedLines, DiffSize, CmtMsgLines, " +
                   "CmtMsgBytes, " +
                   #
                   "NumSignedOffs, NumTags, TotalSubsys, " +
                   "Subsys, inRC, " +
                   #
                   "AuthorSubsysSimilarity, AuthorTaggersSimilarity, " +
                   "TaggersSubsysSimilarity, releaseRangeId) " +
                   "VALUES " +
                   # TODO: For some reason, using %d for integers does not work
                   # (and likewise for %f)
                   "(%s, %s, %s, %s, %s, " +
                   " %s, %s, %s, %s, %s, " +
                   " %s, %s, %s, %s, %s, " +
                   " %s, %s, %s, %s)",
                   (cmt.id, tstamp_to_sql(int(cmt.getCdate())),
                    cmt.getAuthorPI().getID(), tstamp_to_sql(int(cmt.adate)),
                    cmt.adate_tz, projectID, cmt.getChangedFiles(0),
                    #
                    int(cmt.getAddedLines(0)), int(cmt.getDeletedLines(0)),
                    int(cmt.getAddedLines(0) + cmt.getDeletedLines(0)),
                    int(cmt.getCommitMessageLines()),
                    int(cmt.getCommitMessageSize()),
                    #
                    int(getSignoffCount(cmt)), int(getSignoffEtcCount(cmt)),
                    int(subsys_count), subsys_name, int(inRC),
                    #
                    float(cmt.getAuthorSubsysSimilarity()),
                    float(cmt.getAuthorTaggersSimilarity()),
                    float(cmt.getTaggersSubsysSimilarity()),
                    int(releaseRangeID)))

        # TODO: Continue writing here. Include at least
        # signoff-info (subsys info of signers)
        # similarity_between_author_and_signers
        # predominantly add, remove, or modify code (3-level factor)
    dbm.doCommit()

def writeSubsysPerAuthorData2File(id_mgr, outdir):
    '''
    per-author subsystem information is written to the outdir location
    '''
    # Export per-author subsystem information (could be included in ids.txt,
    # but since the information is basically orthogonal, we use two files.)
    header = "ID\t"
    header += "\t".join(id_mgr.getSubsysNames() + ["general"])
    lines = [header]
    for id in sorted(id_mgr.getPersons().keys()):
        outstr = "{0}\t".format(id)
        pi = id_mgr.getPI(id)
        subsys_fraction = pi.getSubsysFraction()
        for subsys in id_mgr.getSubsysNames() + ["general"]:
            outstr += "\t{0}".format(subsys_fraction[subsys])
        lines.append(outstr)
    out = open(os.path.join(outdir, "id_subsys.txt"), 'wb')
    out.writelines(lines)
    out.close()

def writeIDwithCmtStats2File(id_mgr, outdir, releaseRangeID, dbm, conf):
    '''
    ID information together with commit stats for each ID are written
    to the database.

    NOTE: The information written here can not completely faithfully
    be recovered from table commit: When tags are used to capture collaboration
    relations, persons without commits can be referenced, for instance when
    they are CCed in a patch, but did not contribute any code during the
    release cycle.
    '''

    projectID = dbm.getProjectID(conf["project"], conf["tagging"])

    # Clear the information before writing new commints
    dbm.doExec("DELETE FROM author_commit_stats WHERE releaseRangeId=%s",
               (int(releaseRangeID)))

    for id in sorted(id_mgr.getPersons().keys()):
        pi = id_mgr.getPI(id)
        cmt_stat = pi.getCommitStats()
        added = cmt_stat["added"]
        deleted = cmt_stat["deleted"]
        numcommits = cmt_stat["numcommits"]
        dbm.doExec("INSERT INTO author_commit_stats " +
                   "(authorId, releaseRangeId, added, deleted, total, numcommits) "
                   + "VALUES (%s, %s, %s, %s, %s, %s)",
                   (id, releaseRangeID, cmt_stat["added"], cmt_stat["deleted"],
                    cmt_stat["added"] + cmt_stat["deleted"],
                    cmt_stat["numcommits"]))

    dbm.doCommit()


def writeAdjMatrix2File(id_mgr, outdir, conf):
    '''
    Connections between the developers are written to the outdir location
    in adjacency matrix format
    '''

    # Store the adjacency matrix for developer network, i.e., create
    # a NxN matrix in which the entry a_{i,j} denotes how strongly
    # developer j was associated with developer i
    # NOTE: This produces a sparse matrix, but since the number
    # of developers is only a few thousand, it will likely not pay
    # off to utilise this fact for more efficient storage.

    link_type = conf["tagging"]
    out = open(os.path.join(outdir, "adjacencyMatrix.txt"), 'wb')
    idlist = sorted(id_mgr.getPersons().keys())
    # Header
    out.write("# " +
              "\t".join([str(id_mgr.getPI(elem).getName()) for elem in idlist]) +
              "\n")

    # Matrix. The sum of all elements in row N describes how many
    # tags id N has received. The sum of column N states how many
    # tags were given by id N to other developers.
    if link_type == LinkType.tag:
        for id_receiver in idlist:
            out.write("\t".join(
                [str(id_mgr.getPI(id_receiver).getActiveTagsReceivedByID(id_sender))
                   for id_sender in idlist]) + "\n")

    else:
        for id_receiver in idlist:
            out.write("\t".join(
                [str(id_mgr.getPI(id_receiver).getLinksReceivedByID(id_sender, link_type))
                   for id_sender in idlist]) + "\n")



    out.close()


def emitStatisticalData(cmtlist, id_mgr, outdir, releaseRangeID, dbm, conf):
    """Save the available information for a release interval for further statistical processing.

    Several files are created in outdir respectively the database:
    - Information about the commits proper (formerly commits.txt)
    - Names/ID associations (formerly ids.txt). This file also contains
      the per-author total of added/deleted/modified lines etc.
    - Per-Author information on relative per-subsys work distribution (id_subsys.txt)
    - Connection between the developers derived from commit tags (adjacencyMatrix.txt)"""

    writeCommitData2File(cmtlist, id_mgr, outdir, releaseRangeID, dbm, conf)

    # NOTE: Subsystem information is currently not written into the
    # proper database because it is not configured for almost all projects
    writeSubsysPerAuthorData2File(id_mgr, outdir)

    writeIDwithCmtStats2File(id_mgr, outdir, releaseRangeID, dbm, conf)

    writeAdjMatrix2File(id_mgr, outdir, conf)

    return None


def populatePersonDB(cmtlist, id_mgr, link_type=None):
    for cmt in cmtlist:
        #create person for author
        ID = id_mgr.getPersonID(cmt.getAuthorName())
        pi = id_mgr.getPI(ID)
        cmt.setAuthorPI(pi)
        pi.addCommit(cmt)

        if link_type == LinkType.proximity or \
           link_type == LinkType.committer2author or \
           link_type == LinkType.file:
            #create person for committer
            ID = id_mgr.getPersonID(cmt.getCommitterName())
            pi = id_mgr.getPI(ID)
            cmt.setCommitterPI(pi)
            pi.addCommit(cmt)

    return None


def computeProximityLinks(fileCommitList, cmtList, id_mgr, link_type, \
                          startDate=None, speedUp=True):
    '''
    Constructs network based on commit proximity information
    '''

    '''
    Two contributors are linked when they make a commit that is in
    close proximity to each other (ie. same file AND nearby line numbers).
    Collaboration is quantified by a single metric indicating the
    strength of collaboration between two individuals.
    '''
    for file_commit in fileCommitList.values():

        if speedUp:
            computeSnapshotCollaboration(file_commit, cmtList, id_mgr, link_type,
                                         startDate)
        else:
            [computeSnapshotCollaboration(fileSnapShot[1], [fileSnapShot[0]],
                                    cmtList, id_mgr, link_type, startDate)
                                    for fileSnapShot
                                    in fileCommit.getFileSnapShots().items()]


def computeCommitterAuthorLinks(cmtlist, id_mgr):
    '''
    Constructs network based on the author and commiter of a commit
    '''

    '''
    For each commit in the cmtList a one directional link is created
    from the commiter to the author. The commiter is aware of the contents
    and intents of the authors work and is therefore an indication of
    collaboration.
    - Input -
    cmtlist: commit objects
    id_mgr: idManager object storing all individuals information
    '''

    #----------------------------------
    #Process Bar Setup
    #----------------------------------
    widgets = ['Pass 2/2: ', Percentage(), ' ', Bar(), ' ', ETA()]
    pbar = ProgressBar(widgets=widgets, maxval=len(cmtlist)).start()

    for i in range(0, len(cmtlist)):

        if i % 10 == 0:
            pbar.update(i)

        #get commiter object
        cmt = cmtlist[i]

        #find author and committer unique identifiers
        ID_author    = id_mgr.getPersonID(cmt.getAuthorName())
        ID_committer = id_mgr.getPersonID(cmt.getCommitterName())
        pi_author    = id_mgr.getPI(ID_author)
        pi_committer = id_mgr.getPI(ID_committer)
        edge_weight  = 1

        #add link from committer -> author
        pi_committer.addSendRelation   (LinkType.committer2author, pi_author.getID(), cmt, edge_weight)
        pi_author   .addReceiveRelation(LinkType.committer2author, pi_committer.getID()  , edge_weight)

    pbar.finish()
    #end for i


def computeTagLinks(cmtlist, id_mgr):
    '''
    Constructs network based on tagging information
    '''

    '''
    Two individual are linked by a one directional relationship
    if a tag is placed on an individuals commit or individuals
    who already added a tag to a commit.
    - Input -
    cmtlist: commit objects
    id_mgr: idManager object storing all individuals information
    '''
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

    #----------------------------------
    #Process Bar Setup
    #----------------------------------
    widgets = ['Pass 2/2: ', Percentage(), ' ', Bar(), ' ', ETA()]
    pbar = ProgressBar(widgets=widgets, maxval=len(cmtlist)).start()

    #---------------------------------
    # Identify links for all commits
    #---------------------------------
    for i in range(0, len(cmtlist)):
        cmt = cmtlist[i]

        if i % 10 == 0:
            pbar.update(i)

        ID = id_mgr.getPersonID(cmt.getAuthorName())
        pi = id_mgr.getPI(ID)

        # Remember which subsystems the person touched in the role as an author
        pi.addSendRelation("author", ID, cmt)
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
                        pi.addReceiveRelation(tag, relID)

                        # relID did a sign-off etc. to author
                        id_mgr.getPI(relID).addSendRelation(tag, ID, cmt)

        cmt.setTagPIs(tag_pi_list)
    pbar.finish()
    #end for i


def computeSimilarity(cmtlist):
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

###########################################################################
# Main part
###########################################################################
def performAnalysis(conf, dbm, dbfilename, git_repo, revrange, subsys_descr,
                    create_db, outdir, rcranges=None,
                    limit_history=False):
    link_type = conf["tagging"]

    if create_db == True:
        log.devinfo("Creating data base for {0}..{1}".format(revrange[0],
                                                        revrange[1]))
        createDB(dbfilename, git_repo, revrange, subsys_descr, \
                 link_type, rcranges)

    projectID = dbm.getProjectID(conf["project"], conf["tagging"])
    revisionIDs = (dbm.getRevisionID(projectID, revrange[0]),
                   dbm.getRevisionID(projectID, revrange[1]))
    releaseRangeID = dbm.getReleaseRangeID(projectID, revisionIDs)

    log.devinfo("Reading from data base {0}...".format(dbfilename))
    git = readDB(dbfilename)
    cmtlist = git.extractCommitData("__main__")
    cmtdict = git.getCommitDict()

    #---------------------------------
    #Fill person Database
    #---------------------------------
    id_mgr = idManager(dbm, conf)
    populatePersonDB(cmtdict.values(), id_mgr, link_type)

    if subsys_descr != None:
        id_mgr.setSubsysNames(subsys_descr.keys())

    #---------------------------------
    #compute network connections
    #---------------------------------
    if link_type == LinkType.tag:
        computeTagLinks(cmtlist, id_mgr)

    elif link_type == LinkType.committer2author:
        computeCommitterAuthorLinks(cmtlist, id_mgr)

    elif link_type in (LinkType.proximity, LinkType.file):
        if limit_history:
            startDate = git.getRevStartDate()
        else:
            startDate = None

        fileCommitList = git.getFileCommitDict()
        computeProximityLinks(fileCommitList, cmtdict, id_mgr, link_type,
                              startDate)
    #---------------------------------
    #compute statistical information
    #---------------------------------
    createStatisticalData(cmtlist, id_mgr, link_type)

    #---------------------------------
    #Save the results in text files that can be further processed with
    #statistical software, that is, GNU R
    #---------------------------------
    emitStatisticalData(cmtlist, id_mgr, outdir, releaseRangeID, dbm, conf)


##################################################################
def doProjectAnalysis(conf, from_rev, to_rev, rc_start, outdir,
                      git_repo, create_db, limit_history=False):
    #--------------
    #folder setup
    #--------------
    if not os.path.exists(outdir):
        try:
            os.makedirs(outdir)
        except os.error as e:
            log.exception("Could not create output dir {0}: {1}".
                    format(outdir, e.strerror))
            raise

    if rc_start != None:
        rc_range = [[rc_start, to_rev]]
    else:
        rc_range = None

    #----------------------------
    #Perform appropriate analysis
    #----------------------------
    filename = os.path.join(outdir, "vcs_analysis.db")
    dbm = DBManager(conf)
    performAnalysis(conf, dbm, filename, git_repo, [from_rev, to_rev],
#                        kerninfo.subsysDescrLinux,
                        None,
                        create_db, outdir, rc_range, limit_history)

##################################
#         TESTING CODE
##################################
def testFileCommit():

    dbfilename = "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/linux-test"

    repoDir = "/Users/Mitchell/git/linux-2.6/.git"
    #fileNames = ["drivers/net/loopback.c"]

    outDir = "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_NonTag/30"

    revRange = ["v2.6.30", "v2.6.31"]


    #createFileCmtDB(outDir, repoDir, fileNames)
    performNonTagAnalysis(dbfilename, repoDir, False, outDir, revRange, True)


def testAnalysis(link_type):

    dbfilename = "/home/au/workspace/prosoda/cluster/res/linux/tag/v2.6.30-v2.6.31/vcs_analysis.db"

    repoDir = "/Users/Mitchell/git/linux-2.6/.git"

    revRange = ["v2.6.30", "v2.6.31"]

    outDir = "/home/au/workspace/prosoda/cluster/res/linux/tag/v2.6.30-v2.6.31"

    performAnalysis(dbfilename, repoDir, revRange, None, False,
                    outDir, rcranges=None)

##################################
#         Main
##################################
if __name__ == "__main__":

    #testFileCommit()

    parser = argparse.ArgumentParser()
    parser.add_argument('repo', help="Path to the git repository")
    parser.add_argument('conf', help="Project specific configuration file")
    parser.add_argument('outdir', help="Directory to create result files in")
    parser.add_argument('from_rev', help="Start revision")
    parser.add_argument('to_rev', help="End revision")
    parser.add_argument("--rc_start",
                        help="ID/tag that marks release candidate starting point")
    parser.add_argument('--create_db', action='store_true',
                        help="(Re-)create database")

    args = parser.parse_args()

    limit_history = True

    # TODO: Take project and link_type from the configuration
    conf = load_config(args.conf)
    global_conf = load_global_config("prosoda.conf")

    dbm = dbManager(global_conf)
    doProjectAnalysis(conf, dbm, args.from_rev, args.to_rev, args.rc_start,
                      args.outdir, args.repo, args.create_db, limit_history)

#git_repo = "/Users/wolfgang/git-repos/linux/.git"
#outbase = "/Users/wolfgang/papers/csd/cluster/res/"
#rev = 32
#doKernelAnalysis(rev, outbase, git_repo, False)
#exit(0)

#for rev in range(30,39):
#    doKernelAnalysis(rev, outbase, git_repo, True)
#exit(0)


########################### Some (outdated) examples ################
'''
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
'''
