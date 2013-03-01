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
import sys
sys.path.append('./')
sys.path.append('../')
sys.path.append('cluster/')

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
import codeBlock
import codeLine
import math

def _abort(msg):
    print(msg + "\n")
    sys.exit(-1)

def createFileCmtDB(filename, git_repo, revrange):
    
    git = gitVCS()
    git.setRepository(git_repo)
    #git.config4LinuxKernelAnalysis("kernel")
    git.config4LinuxKernelAnalysis()
    git.setRevisionRange(revrange[0], revrange[1])
    
    git.extractCommitData()
    git.extractFileCommitData()

    print("Shelfing the VCS object")
    output = open(filename, 'wb')
    pickle.dump(git, output, -1)
    output.close()
    print("Finished with pickle")


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
            print("    {0}: {1}, {2}".format(subsys_name, subsys_touched,
                                             asf[subsys_name]))

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

def computeSnapshotCollaboration(fileSnapShot, cmtList, id_mgr, startDate=None):
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
    snapShotCmt = cmtList[ fileSnapShot[0] ] #commit object marking the point in time when the file snapshot was taken
    fileState   = fileSnapShot[1] #the state of the file when the SnapShotCmt was committed
    maxDist     = 25
    author      = True
    #find code lines of interest, these are the lines that are localized 
    #around the snapShotCmt, modify the fileState to include only the 
    #lines of interest 
    modFileState = linesOfInterest(fileState, snapShotCmt.id, maxDist)
    
    #remove commits that occur prior to the specified startDate
    if startDate != None:
        modFileState = removePriorCommits(modFileState, cmtList, startDate)
    
    #remove commits made by the person of interest which do not correspond to 
    #commit of interest
    #modFileState = remove
    
    #collaboration is meaningless without more than one line 
    #of code
    if len(modFileState) > 1:
        
        #now find the code blocks, a block is a section of code by one author
        #use the commit hash to identify the committer or author info as needed 
        codeBlks = findCodeBlocks(modFileState, cmtList, author)
        
        if codeBlks:
            #next cluster the blocks, using the distance measure to figure out
            #what blocks belong together in one group or cluster
            clusters = simpleCluster(codeBlks, snapShotCmt, maxDist, author)
        
            #calculate the collaboration coefficient for each code block
            #[computePersonsCollaboration(cluster, snapShotCmt.getAuthorPI().getID(), id_mgr, maxDist) for cluster in clusters]
            
            
            [computeCommitCollaboration(cluster, snapShotCmt.id, id_mgr,
                                        maxDist, author) for cluster in clusters]
    
def computeCommitCollaboration(codeBlks, revCmtId, id_mgr, maxDist,
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
    revcmtId - the commit id of the revision we are interested in 
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
    revCmtBlks = [blk for blk in codeBlks if blk.cmtHash == revCmtId]
    
    #get the person responsible for this revision
    if author:
        revPerson = id_mgr.getPI( revCmtBlks[0].authorId ) 
    else:
        revPerson = id_mgr.getPI( revCmtBlks[0].committerId )
    
    #find all other commit ids for older revisions
    oldCmtIdSet = set( [blk.cmtHash for blk in codeBlks
                        if blk.cmtHash != revCmtId] )
    
    #calculate relationship between personId and all other contributors 
    for oldCmtId in oldCmtIdSet:
        
        #get all blocks for the oldCmtId
        oldRevBlks = [blk for blk in codeBlks if blk.cmtHash == oldCmtId]
        
        #compute relationship strength for ALL combinations of blocks  
        allCombStrengths  = [computeEdgeStrength(blk1, blk2, maxDist)
                             for blk1 in oldRevBlks for blk2 in revCmtBlks]
        
        #TODO: check if summing is the appropriate operation
        #sum the strengths 
        sumStrength = sum(allCombStrengths) #/ len(allCombStrengths) * 1.0
        
        #store result
        if author: 
            personId = oldRevBlks[0].authorId
        else:
            personId = oldRevBlks[0].committerId
        
        if personId == revPerson.getID():
            pass
        
        inEdgePerson = id_mgr.getPI(personId)
        revPerson.addOutEdge  ( personId         , sumStrength)
        inEdgePerson.addInEdge( revPerson.getID(), sumStrength)
    
   
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
        
        #get commit object containing commit date
        cmtObj = clist[cmtId]
        
        if( cmtObj.getCdate() >= startDate ):
            modFileState[lineNum] = cmtId
        
        #else forget about commit
        
    return modFileState
        
def linesOfInterest(fileState, snapShotCommit, maxDist):
    '''
    Finds the regions of interest for analyzing the file. 
    We want to look at localized regions around the commit of 
    interest (snapShotCommit) and ignore code lines that are 
    located some far distance away. 
    
    - Input -
    fileState:      code line numbers together with commit hashes
    snapShotCommit: the commit hash that marks when the fileState was acquired
    maxDist:        indicates how large the area of interest should be
    
    - Output - 
    modFileState: the file state after line not of interest are removed 
    '''
    #variable declarations 
    fileMaxLine = len(fileState)
    modFileState = {} 
    
    #take a pass over the fileState to identify where the snapShotCommit 
    #made contributions to the fileState
    snapShotCmtLines = [] 
    for key in fileState.keys(): 
        
        cmtId = fileState[key]
        
        if cmtId == snapShotCommit:
            snapShotCmtLines.append(key)
    
    #build modFileState by selecting only lines of interest
    for line in snapShotCmtLines:
        #calculate region of interest
        #TODO: little inefficient since there will possibly be some overlap between ranges
        upperBound = int(line) + maxDist
        lowerBound = int(line) - maxDist  
        
        #limit upper and lower bounds to file size
        if(upperBound > fileMaxLine):
            upperBound = fileMaxLine
        if(lowerBound < 1):
            lowerBound = 1
        
        #save lines of interest    
        for i in range(lowerBound, upperBound + 1):
            key = str(i)
            
            if key in fileState:
                modFileState[key] = fileState[key] 
    
    #end for line
    
    
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

    # Export per-author subsystem information (could be included in ids.txt,
    # but since the information is basically orthogonal, we use two files.)
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
    id_writer.writerow(["ID", "Name", "eMail", "added", "deleted", "total",
                        "numcommits"])

    # Content
    for id in sorted(id_mgr.getPersons().keys()):
        pi = id_mgr.getPI(id)
        cmt_stat = pi.getCommitStats()
        added = cmt_stat["added"]
        deleted = cmt_stat["deleted"]
        numcommits = cmt_stat["numcommits"]
        id_writer.writerow([id, pi.getName(), pi.getEmail(), added, deleted,
                            added + deleted, numcommits])
    
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

    
def createPersonDB(cmtList):
    
    id_mgr = idManager()
    
    for cmt in cmtList.values():
        
        #create person for author
        ID = id_mgr.getPersonID(cmt.getAuthorName())
        pi = id_mgr.getPI(ID)
        cmt.setAuthorPI(pi)
        pi.addCommit(cmt)
        
        #create person for committer 
        ID = id_mgr.getPersonID(cmt.getCommitterName())
        pi = id_mgr.getPI(ID)
        cmt.setCommitterPI(pi)
        pi.addCommit(cmt)

    return id_mgr

def buildCollaborationStructure(fileCommitList, cmtList, id_mgr, startDate=None):
    '''
    Constructs the collaboration connections between all contributors of 
    a system. Collaboration is quantified by a single metric indicating the 
    strength of collaboration between two individuals. A higher value 
    indicates a stronger connection. 
    '''
    
    for fileCommit in fileCommitList.values():
        
        [computeSnapshotCollaboration(fileSnapShot, cmtList, id_mgr, startDate)
         for fileSnapShot in fileCommit.getFileSnapShots().items()]
            
        
def writeData(cmtList, id_mgr,  outdir):
    # TODO: Large parts of this function are 99% identical with
    # emitStatisticalData. Refactor the commonalities into one joint function
    '''
    Write data to file to be further processed by statistics software
    
    Several files are created in outdir:
    - Names/ID Edges (ids.txt)
    - Connection between the developers derived from commits (not tags)
    '''
   
    ##############
    # Save id/name associations together with the per-author summary statistics
    id_writer = csv.writer(open(os.path.join(outdir, "ids.txt"), 'wb'),
                           delimiter='\t',
                           quotechar='\\', quoting=csv.QUOTE_MINIMAL)
    # Header
    id_writer.writerow(["ID", "Name", "eMail", "added", "deleted", "total",
                        "numcommits"])

    # Content
    for id in sorted(id_mgr.getPersons().keys()):
        pi = id_mgr.getPI(id)
        cmt_stat = pi.getCommitStats()
        added = cmt_stat["added"]
        deleted = cmt_stat["deleted"]
        numcommits = cmt_stat["numcommits"]
        id_writer.writerow([id, pi.getName(), pi.getEmail(), added, deleted,
                            added + deleted, numcommits])
    
    ##############
    # Store the adjacency matrix for developers, i.e., create
    # a NxN matrix in which the entry a_{i,j} denotes how closely developer
    # j was contributing to developer i
    # NOTE: This produces a sparse matrix, but since the number
    # of developers is only a few thousand, it will likely not pay
    # off to utilise this fact for more efficient storage.

    out = open(os.path.join(outdir, "adjacencyMatrix.txt"), 'wb')
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
            [str(id_mgr.getPI(id_receiver).getSumInEdge(id_sender))
               for id_sender in idlist]) + "\n")

    out.close()

    return None
    
def processPersonData(cmtlist, id_mgr):
    '''
    processing of data in the id_mgr that requires all data 
    to be present.
    '''
    #compute basic statistic information on the Edges between 
    #contributors
    #TODO: maybe this can be moved into id manager 
    #[id_mgr.getPI(Id).EdgeProcessing() for Id in id_mgr.getPersons().keys()] 

    # TODO: This is just c&p from createStatisticalData. The functions
    # need to be merged once the non-tag analysis is stable enough
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
    
    #compute the per-author commit summaries.
    for (key, person) in id_mgr.getPersons().iteritems():
        person.computeCommitStats()
        person.edgeProcessing()
    
###########################################################################
# Main part
###########################################################################
def performNonTagAnalysis(dbfilename, git_repo, create_db, outDir, revRange,
                          limitHistory=False):
    
    if create_db == True:
        createFileCmtDB(dbfilename, git_repo, revRange)
        
    print("Reading from data base {0}...".format(dbfilename))
    git = readDB(dbfilename)
    
    #store relevant VCS object attributes
    cmtList = git.getCommitDict()
    if limitHistory:
        startDate = git.getRevStartDate()
    else:
        startDate = None
    
    
    #get personal info from commit data
    id_mgr = createPersonDB(cmtList)
    
    #----------------------------------------
    #build connections between contributors
    #----------------------------------------
    fileCommitList = git.getFileCommitDict()
   
    #calculate the collaboration metrics for all contributors
    buildCollaborationStructure(fileCommitList, cmtList, id_mgr, startDate)
        
    #-------------------------------------------------------------
    #perform processing on collaboration data, all collaboration 
    #data is required to be present for this to work correctly 
    #-------------------------------------------------------------
    # TODO: cmtlist is an actual list, cmtList (capital L)is (
    # why of WHY) a _HASH_, not a list.
    cmtlist = git.extractCommitData("__main__")
    processPersonData(cmtlist, id_mgr)
    
    #-------------------------------------------------------------------
    # Save the results in text files that can be further processed with
    # statistical software, that is, GNU R
    #-------------------------------------------------------------------
    writeData(cmtList, id_mgr, outDir)
    
    
def performAnalysis(dbfilename, git_repo, revrange, subsys_descr, create_db,
                    outdir, rcranges=None):
    if create_db == True:
        print("Creating data base for {0}..{1}").format(revrange[0],
                                                        revrange[1])
        createDB(dbfilename, git_repo, revrange, subsys_descr, rcranges)
        
        
    print("Reading from data base {0}...".format(dbfilename))
    git = readDB(dbfilename)
    cmtlist = git.extractCommitData("__main__")

    # Determine in which ways the authors are connected with each other,
    # plus some more statistical information
    id_mgr = idManager()
    if subsys_descr != None:
        id_mgr.setSubsysNames(subsys_descr.keys())
    createStatisticalData(cmtlist, id_mgr)
                          
    # Save the results in text files that can be further processed with
    # statistical software, that is, GNU R
    emitStatisticalData(cmtlist, id_mgr, outdir)
    
##################################################################
def doProjectAnalysis(project, from_rev, to_rev, rc_start, outdir, git_repo,
                      create_db, nonTag, limitHistory=False):
    #--------------
    #folder setup 
    #--------------

    if not os.path.exists(outdir):
        try:
            os.makedirs(outdir)
        except os.error as e:
            print("Could not create output dir {0}: {1}".format(outdir,
                                                                e.strerror))
            exit(-1)

    if rc_start != None:
        rc_range = [[rc_start, to_rev]]
    else:
        rc_range = None

    #----------------------------
    #Perform appropriate analysis
    #----------------------------
    filename = os.path.join(outdir, "vcs_analysis.db")

    if nonTag:
        print("Performing non-tag based analysis")
        performNonTagAnalysis(filename, git_repo, create_db, outdir,
                              [from_rev, to_rev], limitHistory)
    
    else:
        print("Performing tag based analysis")
        performAnalysis(filename, git_repo, [from_rev, to_rev],
#                        kerninfo.subsysDescrLinux,
                        None,
                        create_db, outdir, rc_range)


##################################
#         TESTING CODE
##################################
def testFileCommit():
    
    dbfilename = "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_NonTag/linux-30-31-nonTag"
    
    repoDir = "/Users/Mitchell/git/linux-2.6/.git"
    #fileNames = ["drivers/net/loopback.c"]
    
    outDir = "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_NonTag/30"
    
    revRange = ["v2.6.30", "v2.6.31"]
    
    
    #createFileCmtDB(outDir, repoDir, fileNames)
    performNonTagAnalysis(dbfilename, repoDir, False, outDir, revRange, True)
    
    
def loadVCSObjectTest():
    
    dbfilename = "/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/res_Tag/linux-30-31-Tag"
    VCS_object = readDB(dbfilename)
    

##################################
#         Main
##################################

if __name__ == "__main__":
    
    #testFileCommit()
    
    parser = argparse.ArgumentParser()
    parser.add_argument('repo', help="Path to the git repository")
    parser.add_argument('project', help="Project name")
    parser.add_argument('outdir', help="Directory to create result files in")
    parser.add_argument('from_rev', help="Start revision")
    parser.add_argument('to_rev', help="End revision")
    parser.add_argument("--rc_start",
                        help="ID/tag that marks release candidate starting point")
    parser.add_argument('--create_db', action='store_true',
                        help="(Re-)create database")
    # tagged analysis is the default, the argument is only available
    # for systematic consistency
    parser.add_argument('--non_tag', action='store_true',
                        help="Perform a source based cluster analysis")
    parser.add_argument('--tag', action='store_true',
                        help="Perform a tag based cluster analysis (default)")
    args = parser.parse_args()
    
    limitHistory = True

    doProjectAnalysis(args.project, args.from_rev, args.to_rev, args.rc_start,
                      args.outdir, args.repo, args.create_db, args.non_tag,
                      limitHistory)
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
    

