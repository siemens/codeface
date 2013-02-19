#! /usr/bin/env python

# Define classes to interact with source code management
# systems (currently, only git is supported, but adding
# support for Mercurial and Subversion should be easy to do)
# This class is designed to be serialisable after the data have
# been read in, so the design should remain unaltered whenever possible.
# Processing and experimentation steps that require frequent
# changes must be implemented in different classes.

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

# WM 15-Feb-2010 (Polished and generalised version based on 
# various older Perl scripts)

# TODO: When the git implementation is finished, the general parts
# should be factored out and the VCS specific parts be implemented
# for other VCSes. See the comments of the functions on what must be
# VCS-specific.
# TODO: Unify range handling. Either a range is always a list, or always
# represented by two parameters.

from subprocess import *
from progressbar import *
import commit
import fileCommit
import re
import sys

class Error(Exception):
    """Base class for exceptions in this module."""
    pass

class ParseError(Error):
    """Exception raised for parsing errors.

    Attributes:
        line -- string that could not be parsed
        id -- id of the commit in question
    """

    def __init__(self, line, id):
        self.line = line
        self.id = id

def _abort(msg):
    print(msg + "\n")
    sys.exit(-1)


class VCS:
    """
    Encapsulate methods to analyse VCS repositories

    This base class does nothing in particular, but serves
    as an 'interface' (or what you would regard in the Python world
    as this) for the tool specific implementations (e.g., for git
    and mercurial).
    
    Note: Typical use of the subclasses is to assign a repository,
    and to call extractCommitData to perform the time-consuming
    processing. Afterwards, the object can be serialised for later
    (and faster) use by various post-processing and analysis methods.
    """

    def __init__(self):
        # "None" represents HEAD for end and the inital
        # commit for start
        self.rev_startDate = None;
        self.rev_start     = None;
        self.rev_end       = None;
        self.repo          = None

        # For each subsystem, contains a time-ordered list of all commits
        # (i.e., _commit_list_dict["block"] is a list[] of ids)
        self._commit_list_dict = None

        # The raw commit hashes (no commit.Commit objects) per subsystem,
        # structure as above.
        self._commit_id_list_dict = None

        # When _rc_ranges is set to something like [[A,B], [C,D]],
        # then the commits A..B and C..D are taken to be in release ranges.
        # All the low-level commit hashes are then stored in _release_id_list
        self._rc_id_list = None
        self._rc_ranges = None
        
        # Contains all commits indexed by commit id
        self._commit_dict = None
        
        #Dictionary with key = filename and value = list of all commits
        #for that file 
        self._fileCommit_dict = None
        
        #file names to include in analysis(non-taged based)
        self._fileNames = None
        
        self.subsys_description = {}
    
    def getCommitDict(self):
        return self._commit_dict
    
    def getRevStartDate(self):
        return self.rev_startDate
    
    def getFileCommitDict(self):
        return self._fileCommit_dict
        
    def setFileNames(self, fileNames):
        self._fileNames = fileNames
        
    def getFileNames(self):
        return self._fileNames
            
    def setRepository(self, repo):
        self.repo = repo

    def setRevisionRange(self, rev_start, rev_end):
        self.rev_start = rev_start
        self.rev_end = rev_end

    def setSubsysDescription(self, subsys_description):
        # See kerninfo.py for some examples of the format
        if subsys_description != None:
            self.subsys_description = subsys_description

    def setRCRanges(self, rc_ranges):
        self._rc_ranges = rc_ranges
        self._rc_id_list = []

    def extractCommitData(self, subsys="__main__"):
        """Analyse the repository and cache the results.
        
        Takes the list of commits, computes the associated 
        diffs in various ways, and stores (i.e., caches) the results 
        in the object instance so that they can be serialised."""
        return []

    def extractCommitDataRange(self, revrange, subsys="__main__"):
        """Analyse a certain range of the repository.

        Restrict the data obtained from extractCommitData() to
        a specific time slice."""
        return []

    def getDiffVariations(self):
        return 1

    def _subsysIsValid(self, subsys):
        """Check if subsystem subsys is valid."""
        return subsys=="__main__" or subsys in self.subsys_description.keys()


class gitVCS (VCS):
    def __init__(self):
        VCS.__init__(self) # Python OOP braindamage
        # Some analysis patterns that are required to analyse the
        # output of git
        self.cmtHashPattern = re.compile(r'^\w{40}$') 
        self.logPattern = re.compile(r'^(.*?) (.*)$')
        self.authorPattern = re.compile(r'^Author: (.*)$')
        self.committerPattern = re.compile(r'^Commit: (.*)$')
        self.signedOffPattern = re.compile(r'^(.*?): (.*)$')
        self.diffStatFilesPattern = re.compile(r'(\d*?) file(|s) changed')
        self.diffStatInsertPattern = re.compile(r' (\d*?) insertion')
        self.diffStatDeletePattern = re.compile(r' (\d*?) deletion')

    def getDiffVariations(self):
        # We support diffs formed of 2x2 combinations: 
        # with and without whitespace sensitivity,
        # and regular/patience diff.
        return 4

    def _prepareCommitLists(self):
        """Gets the hash values (or whatever is used to identify
        a commit) and log messages (plus some other metadata, but
        excluding the actual diffs) for the desired revision range.
        The frontend function is extractCommitData()

        Returns a dictionary with subsystem names as keys and 
        commit lists as values. The global list (i.e., for the complete
        project including all subsystems) is stored under "__main__".
        """
        if self.repo == None:
            _abort("Internal error: Can't do anything without repo")

        # Start with the global list for the whole project
        self._prepareGlobalCommitList()

        # .. and proceed with the subsystems. We "recycle" the already
        # created commit instances by placing them on subsystem specific
        # lists. 
        for subsys in self.subsys_description.keys():
            clist = self._getCommitIDsLL(self.subsys_description[subsys])

            # Based on this information, prepare a list of commit.Commit
            # objects
            self._commit_list_dict[subsys] = \
                [self._commit_dict[self._Logstring2ID(logstring)]
                 for logstring in reversed(clist)]
            
            # For the subsystems, we also prepare lists of "raw" IDs
            # that are required for the subsystem identification
            # pass (NOTE: There are some optimisation opportunities here)
            self._commit_id_list_dict[subsys] = \
                [self._Logstring2ID(logstring)
                 for logstring in reversed(clist)]

        # Finally, we also create commit ID lists for ranges of
        # interest (currently, only to decide wether a commit is in
        # a feature freeze phase or not)
        if self._rc_ranges != None:
            for range in self._rc_ranges:
                clist = self._getCommitIDsLL("", range[0], range[1])
                self._rc_id_list.extend([self._Logstring2ID(logstring)
                                          for logstring in clist])
            
    def _getCommitIDsLL(self, dir_list, rev_start=None, rev_end=None):
        """Low-level routine to extract the commit list from the VCS.

        Must be implemented specifically for every VCS, and must
        return a list of strings that can be parsed with 
        _Logstring2ID() for a specific revision range (rev_start..rev_end)
        in the subsystem described by the directory list dir_list."""

        if rev_start == None and rev_end != None:
            _abort("Internal error: Bogous range!")

        revrange = ""
        if rev_start:
            revrange += "{0}..".format(rev_start)
        else:
            if self.rev_start:
                revrange += "{0}..".format(self.rev_start)

        if rev_end:
            revrange += rev_end
        else:
            if self.rev_end:
                revrange += self.rev_end

        # TODO: Check the effect that -M and -C (to detect copies and
        # renames) have on the output. Is there anything we need
        # to take into account?
        # First, get the output for the complete revision
        # range (TODO: cache this)
        # NOTE: %H prints the hash value of the commit, %ct denotes
        # the comitter date (it's important to use comitter and not
        # author date; this guarantees monotonically increasing time
        # sequences)
        # Passing a simple formatted string and using getoutput() to
        # obtain the result is way nicer in python3.
        cmd = 'git --git-dir={0} log --no-merges -M -C'.format(self.repo).split()
        cmd.append('--pretty=format:%ct %H')
        cmd.append('--date=local') # Essentially irrelevant
        cmd.append(revrange)
        if (len(dir_list) > 0):
            cmd.append("--")
            for dir in dir_list:
                cmd.append(dir)

        print("About to call {0}".format(" " .join(cmd)))
        try:
            p2 = Popen(cmd, stdout=PIPE)
            clist = p2.communicate()[0].splitlines()
        except OSError:
            _abort("Internal error: Could not spawn git")

        # Remember the comment about monotonically increasing time sequences
        # above? True in principle, but unfortunately, a very small number
        # of commits can violate this for various reasons. Since these
        # outliers screw up the cumulative graph, we have to add an
        # extra sorting pass.
        clist.sort(reverse=True)

        # Then, obtain the first and last commit in the desired range
        # and extract the desired subrange
        return clist


    def _getSingleCommitInfo(self, cmtHash):
        #produces the git log output for a single commit hash
        
        #build git command
        cmd = 'git --git-dir={0} log'.format(self.repo).split()
        cmd.append(cmtHash)
        cmd.append("-1")
        cmd.append('--pretty=format:%ct %H')
        
        #submit query to git 
        logMsg = self._gitQuery(cmd)
        
        return logMsg

    def _getFileCommitInfo(self, fname=None, rev_start=None, rev_end=None):
        '''extracts a list of commits on a specific file for the specified 
        revision range'''
       
        revrange = ""        
        
        if rev_start == None and rev_end != None:
            revrange += reg_end
        else:
            if rev_start:
                revrange += "{0}..".format(rev_start)
    
            if rev_end:
                revrange += rev_end
        
        
        #build git command
        #we must take merge commits, otherwise when we cross-reference with 
        #git blame some commits will be missing
        cmd = 'git --git-dir={0} log -M -C'.format(self.repo).split()
        cmd.append('--pretty=format:%ct %H')
        cmd.append('--date=local') # Essentially irrelevant
        if rev_start and rev_end:
            cmd.append(revrange)
        if fname:
            cmd.append("--")
            cmd.append(fname)
        
                
        #submit query command        
        clist = self._gitQuery(cmd)
        
        # Remember the comment about monotonically increasing time sequences
        # above? True in principle, but unfortunately, a very small number
        # of commits can violate this for various reasons. Since these
        # outliers screw up the cumulative graph, we have to add an
        # extra sorting pass.
        clist.sort(reverse=True)

        # Then, obtain the first and last commit in the desired range
        # and extract the desired subrange
        return clist
            
            
    def _gitQuery(self, cmd):
        '''low level routine to submit a git command and return the 
        output'''
        
#        print("About to call {0}".format(" " .join(cmd)))
        try:
            p2 = Popen(cmd, stdout=PIPE)
            output = p2.communicate()[0].splitlines()
        except OSError:
            _abort("Internal error: Could not spawn git")
            
        return output
        

    def _prepareGlobalCommitList(self):
        """Prepare the list of all commits for the complete project.

        The results are stored in a sequential list of all commits
        in _commit_list_dict["__main__"] as well as in a dictionary
        (_commit_dict) that is indexed by commit id.
        """
        # If we have already computed the list, we need not
        # do it again
        if self._commit_list_dict != None:
            return

        self._commit_list_dict = {}
        self._commit_id_list_dict = {}
        self._commit_dict = {}
        clist = self._getCommitIDsLL("")

        # We need to process the array in inverse order to obtain a
        # time-wise increasing sequence. The result is a list of
        # commit objects
        self._commit_list_dict["__main__"] = [self._Logstring2Commit(logstring) 
                                              for logstring in reversed(clist)]
        for cmt in self._commit_list_dict["__main__"]:
            self._commit_dict[cmt.id] = cmt


    def _Logstring2ID(self, str):
        """Extract the commit ID from a log string."""
        match = self.logPattern.search(str)
        if not(match):
            # TODO: Throw an exception
            print("Internal error: _Logstring2ID could not parse log string!")
            sys.exit(-1)

        return match.group(2)

    def _Logstring2Commit(self, str):
        """Create an instance of commit.Commit from a given log string.

        Must be implemented by every VCS. Turns a string from the list
        returned by getCommitIDsLL into a commit.Commit instance"""

        match = self.logPattern.search(str)
        if not(match):
            # TODO: Throw an exception
            print("Internal error: _Logstring2Commit could not parse log string!")
            sys.exit(-1)

        cmt = commit.Commit()
        cmt.cdate = match.group(1)
        cmt.id = match.group(2)

        return cmt

    def _analyseDiffStat(self, msg, cmt):
        """Analyse the results of diff show with respect to the diffstat."""
        msg = msg.splitlines()
        files = 0
        insertions = 0
        deletions = 0
        matched = False

        try: 
            match = self.diffStatFilesPattern.search(msg[-1])
            if (match):
                files = match.group(1)
                matched = True

            match = self.diffStatInsertPattern.search(msg[-1])
            if (match):
                insertions = match.group(1)
                matched = True

            match = self.diffStatDeletePattern.search(msg[-1])
            if (match):
                deletions = match.group(1)
                matched = True

        except IndexError: 
            print("Blubb?!")
            print(msg)
            raise ParseError("Empty commit?", cmt.id)

        if not(matched):
            raise ParseError(msg[-1], cmt.id)

        cmt.diff_info.append((int(files), int(insertions), int(deletions)))

    def _parseCommit(self, cmt):
        # First, determine which subsystems are touched by the commit
        cmt_subsystems = cmt.getSubsystemsTouched()
        touched_subsys = False

        for subsys in self.subsys_description.keys():
            if cmt.id in self._commit_id_list_dict[subsys]:
                cmt_subsystems[subsys] = 1
                touched_subsys = True
            else:
                cmt_subsystems[subsys] = 0

        # Commit is not associated with a specific subsystem, so
        # file it under "general"
        if touched_subsys == False:
            cmt_subsystems["general"] = 1
        else:
            cmt_subsystems["general"] = 0

            cmt.setSubsystemsTouched(cmt_subsystems)

        # Second, check if the commit is within the release cycle
        if self._rc_id_list != None:
            if cmt.id in self._rc_id_list:
                cmt.setInRC(True)
            else:
                cmt.setInRC(False)

        # Third, analyse the diff content
        # TODO: Using a list of entries in diff_info is suboptimal.
        # This should be replaced with a hash indexed by parameter
        # combination
        for difftype in ("", "--patience"):
            for whitespace in ("", "--ignore-space-change"):
                cmd = ("git --git-dir={0} show --format=full --shortstat "
                       "--numstat {1} {2} {3}".format(self.repo, difftype,
                                                      whitespace, cmt.id)).split()
                try:
                    # print("About to call " + " ".join(cmd))
                    p2 = Popen(cmd, stdout=PIPE)
                    msg = p2.communicate()[0]
                    self._analyseDiffStat(msg, cmt)
                except UnicodeDecodeError:
                    # Since we work in utf8 (which git returns and
                    # Python is supposed to work with), this exception
                    # seems to stem from a faulty encoding. Just
                    # ignore the commit
                    cmt.diff_info.append((0,0,0))
                except ParseError as pe:
                    # Since the diff format is very easy to parse,
                    # this most likely stems from a malformed diff
                    # that can be ignored. Nevertheless, report the
                    # line and the commit id
                    print("Could not parse diffstat for {0}!".
                          format(pe.id))
                    print("{0}".format(pe.line))
                    cmt.diff_info.append((0,0,0))
                except OSError:
                    _abort("Internal error: Could not spawn git")

            # The commit message is independent of the diff type, so we
            # can re-use the information in msg
            self._analyseCommitMsg(msg, cmt)



    def _analyseCommitMsg(self, msg, cmt):
        """Analyse the commit message."""
        # The format we are analysing is the following:
        ######
        # commit db1f05bb85d7966b9176e293f3ceead1cb8b5d79
        # Author: Author Name <author.name@email.tld>
        # Date:   Wed Feb 10 12:15:53 2010 +0100 
        #
        #     Headline of the commit
        #
        #     Body of the commit description. Note that the headline is
        #     available only by convention, but not enforced, so we cannot
        #     rely on it. The user-supplied message part always starts with
        #     four spaces.
        # 
        #     CC: J. Kernel Hacker <kernel.hacker@redhat.com>
        #     Signed-off-by: Hans Huber <hans@hubercorp.com>
        #     Signed-off-by: Friedrich Genscher <genscher@brd.gov>  
        #
        # 8	1	path/to/file.c
        # 2	0	path/to/another.c
        #  2 files changed, 10 insertions(+), 1 deletions(-)
        ######
        # We get a list representation of the commit that was split
        # by line breaks, so restore the original state first and then
        # do a decomposition into parts 
        
        parts = msg.split("\n\n")

        # Find the chunk that contains the commit id (for 99.9% of all
        # commits, this is chunk 0, but things are different for tags.
        for i in range(0, len(parts)):
            if parts[i].startswith("commit "):
                break
            elif i == len(parts)-1:
                # TODO: This should be made an exception, and we
                # should exit gracefully. On some occasions, this
                # error is triggered although the commit in question
                # (e.g., 9d32c30542f9ec) can be parsed
                # fine when viewed within a different range...?!
                # Perhaps this is also a pypy issue. For the commit
                # mentioned before, the problem was only detected with
                # pypy, not with cpython. 
                _abort("Cannot find metadata start in commit message!")
        
        commit_index = i
        descr_index = i+1

        # Determine author and committer
        for line in parts[commit_index].split("\n"):
            match = self.authorPattern.search(line)
            if (match):
                cmt.author = match.group(1)

            match = self.committerPattern.search(line)
            if (match):
                cmt.committer = match.group(1)

        signed_off_part = parts[descr_index].split("\n    \n    ")[-1]
        # Ensure that there are actually signed-offs in the signed-off
        # part 
        found = False
        for line in signed_off_part.split("\n"):
            line = re.sub("^    ", "", line)
            if (line.startswith("CC:") 
                or line.startswith("Signed-off-by:") 
                or line.startswith("Acked-by:") 
                or line.startswith("Reviewed-by:") 
                or line.startswith("Reported-by:") 
                or line.startswith("Tested-by:") 
                or line.startswith("LKML-Reference:")):
                found = True
                break

        if found:
            descr_message = "\n".join(parts[descr_index].
                                      split("\n    \n    ")[0:-1])
            self._analyseSignedOffs(signed_off_part, cmt)
        else:
            descr_message = parts[descr_index]

        # Normalise the commit message
        final_message = ""
        for line in descr_message.split("\n"):
            line = re.sub("^    ", "", line)
            final_message += line + "\n"

        cmt.commit_msg_info = (len(final_message.split("\n")), 
                               len(final_message))

    def _analyseSignedOffs(self, msg, cmt):
        """Analyse the Signed-off-part of a commit message."""
        
        tag_names_list = cmt.getTagNames()
        for entry in msg.split("\n"):
            entry = re.sub("^    ", "", entry)
            match = self.signedOffPattern.search(entry)
            if (match):
                key = match.group(1).replace(" ", "").replace(":", "")
                value = match.group(2)

                if key in tag_names_list.keys():
                    tag_names_list[key].append(value)
                else:
                    tag_names_list[key] = [value]
            else:
                print("Warning: Could not parse Signed-off like line:")
                print('{0}'.format(entry))

    def extractCommitDataRange(self, revrange, subsys="__main__"):
        """
        Same as extractCommitData, but for a specific temporal range.
        
        Instead of using the whole repo, we extract a temporal
        subset. Since the data for the complete repo are cached, we
        use them as basis: The appropriate commit objects are cherry-picked
        from the global list and rearranged into a new, subsys and
        range-specific list.
        """
        
        if (len(revrange) != 2):
            _abort("Internal error: Bogous range")
            
        globData = self.extractCommitData(subsys)
            
        # NOTE: __main__ is a pseudo-subsystem that is not contained
        # in the subsystem description
        if subsys=="__main__":
            clist = self._getCommitIDsLL("", revrange[0], revrange[1])    
        else:
            clist = self._getCommitIDsLL(self.subsys_description[subsys],
                                         revrange[0], revrange[1])

        return [self._commit_dict[self._Logstring2ID(logstring)]
                for logstring in reversed(clist)]


    def extractCommitData(self, subsys="__main__"):
        if not(self._subsysIsValid(subsys)):
            _abort("Subsys specification invalid: {0}\n".format(subsys))

        # If we've already computed the result, make use of it
        # (shelved objects can therefore provide a significant 
        # performance advantage)
        if self._commit_list_dict:
            print("Using cached data")
            return self._commit_list_dict[subsys]

        self._prepareCommitLists()

        # _commit_list_dict as computed by _prepareCommitLists() already
        # provides a decomposition of the commit list into subsystems:
        # It suffices to analyse the commits in the global commit list
        # __main__, the subsystem information follows automatically 
        # from this.
        count = 0
        widgets = ['Pass 1/2: ', Percentage(), ' ', Bar(), ' ', ETA()]
        pbar = ProgressBar(widgets=widgets,
                           maxval=len(self._commit_list_dict["__main__"])).start()

        for cmt in self._commit_list_dict["__main__"]:
            count += 1
            if count % 20 == 0:
                pbar.update(count)

#            print("Processing commit {0}/{1} ({2})".
#                  format(count, len(self._commit_list_dict["__main__"]), 
#                         cmt.id))

            self._parseCommit(cmt)

        # For the subsystems, we need not re-analyse the commits again,
        # but can just pick the results from the global analysis. 
        # Which was already done by _prepareCommitLists() ;-)

        return self._commit_list_dict[subsys]


    def _getBlameMsg(self, fileName, commitHash):
        '''provided with a file name and commit hash the function returns 
        the blame message'''
        
        #build command string 
        cmd = 'git --git-dir={0} blame'.format(self.repo).split()
        cmd.append("-p") #format for machine consumption
        cmd.append("-w") #ignore whitespace changes
        cmd.append("--")
        cmd.append(fileName)
        cmd.append(commitHash)
    
        
        #query git repository 
        blameMsg = self._gitQuery(cmd)
        
        return blameMsg
        

    def _parseBlameMsg(self, msg):
        '''input a blame msg and the commitID under examination the 
        output contains code line numbers and corresponding commitID'''
        
        #variable declarations
        commitLineDict = {} #dictionary, key is line number, value is commit ID 
        
        
        while msg:
            
            line = msg.pop(0).split(" ")
            
            #the lines we want to match start with a commit hash
            if(self.cmtHashPattern.match( line[0] ) ):
               
               lineNum    = line[2] 
               commitHash = line[0]
               
               commitLineDict[lineNum] = commitHash
               
               #here we could get the line of code if needed, 
               #for now it would be a waste to do anything 
               #with it so just toss it away
               msg.pop(0) 
             
            
        return commitLineDict
   
    def extractFileCommitData(self):
       '''high level function to extract all commits for a given file 
       and builds a fileCommit object to store relavent data'''
       
       if self._fileCommit_dict:
           print("using cached data...")
           return
       
       #query the git repository for all commits to a particular 
       #set of files, FileCmtList is a list of commit hashes
       self._prepareFileCommitList(self._fileNames) 
       
       #get diff and remaining commit data (author,committer etc)
       #and store in respective commit objects
       map(self._parseCommit, self._commit_dict.values())   
       
    def _prepareFileCommitList(self, fnameList):
        # TODO: This function mixes preparing the commit list with parsing
        # the commits. This should be separated, as in the generic case
        #variable initialization 
        self._fileCommit_dict = {}
        blameMsgCmtIds = set() #stores all commit Ids seen from blame messages
        
        if self._commit_dict is None:
            self._commit_dict = {}    
        
        
        count = 0
        widgets = ['Pass 1/2: ', Percentage(), ' ', Bar(), ' ', ETA()]
        pbar = ProgressBar(widgets=widgets,
                           maxval=len(fnameList)).start()

        for fname in fnameList:
            count += 1
            if count % 20 == 0:
                pbar.update(count)

            #create fileCommit object, one per filename to be 
            #stored in _fileCommit_dict
            fileCmts = fileCommit.FileCommit()
            
            #get commit objects for the given file and revision range
            cmtList  = self.getFileCommits(fname, self.rev_start, self.rev_end)
            
            #many file may not have any commits made to then during the 
            #revision of interest, in that case don't store the data
            if len(cmtList) != 0:
                #store commit hash in fileCommit object, store only the hash 
                #and then reference the commit db, this prevents the duplication 
                #of information since a commit can touch many files
                fileCmts.setCommitList([cmt.id for cmt in cmtList])
                
                #store the commit object to the committerDB, the justification 
                #for splitting this way is to avoid redundant commit info 
                #since a commit can touch many files, we use the commit 
                #hash to reference the commit data (author, date etc)
                for cmt in cmtList:
                    if cmt not in self._commit_dict:
                        self._commit_dict[cmt.id] = cmt
                      
                #get git blame information for each commit in each file 
                for cmt in cmtList:
                
                    #query git reppository for blame message
                    blameMsg = self._getBlameMsg(fname, cmt.id)
           
                    #parse the blame message, this extracts the line number 
                    #and corresponding commit hash, returns a dictionary 
                    #Key = line number, value = commit hash
                    #basically a snapshot of what the file looked like 
                    #at the time of the commit
                    fileLayout_dict = self._parseBlameMsg(blameMsg)
                     
                    
                    #store the dictionary to the fileCommit Object
                    fileCmts.addFileSnapShot(cmt.id, fileLayout_dict)
                    
                    #save cmtIDs from blame message for the step below
                    # explained in "capture the remaining commits"
                    blameMsgCmtIds.update( fileLayout_dict.values() )
            
                #end for cmtList
                
                #store fileCommit object to dictionary
                self._fileCommit_dict[fname] = fileCmts
            
            #else:
                #do nothing
            #end if cmtList
                
           
        #end for fnameList 
        
        #find the date of earliest commit made for this revision
        #Check if time zones influence this or its already normalized
        self.rev_startDate = min( [cmt.getCdate() for cmt in self._commit_dict.values()] )
        
        
        #-------------------------------
        #capture the remaining commits 
        #-------------------------------
        '''
         if the analysis of the git blame messages is focused 
         only on a temporal region (ie. not entire history) it 
         can be the case that commits not found during the git 
         log query (due to revision range) are present in the 
         git blame messages. We must query git again to get the 
         remaining commits otherwise we cannot reference them during 
         the blame message anaysis.
         '''
         
        #find all commits that are missing from the commit dictionary
        #recall that fileCommit_dict stores all the commit ids for a 
        #file to reference commit objects in the commit_dict
        # TODO: The number of commits found this way seems fairly large
        # Check if the method is really correct.
        missingCmtIds = blameMsgCmtIds - set(self._commit_dict)
        
        #retrieve missing commit information and add it to the commit_dict
        missingCmts = [ self.cmtHash2CmtObj(cmtId)
                        for cmtId in missingCmtIds ]
        if missingCmts:
            count = 0
            widgets = ['Pass 1.5/2: ', Percentage(), ' ', Bar(), ' ', ETA()]
            pbar = ProgressBar(widgets=widgets,
                               maxval=len(missingCmts)).start()
    
            for cmt in missingCmts:
                count += 1
                if count % 20 == 0:
                    pbar.update(count)
    
                self._commit_dict[cmt.id] = cmt

        #TODO: figure out a way to get the missing commits without 
        #      having to parse all commit messages
        #get entire commit history on file
        #self._commit_dict.update( {cmt.id: cmt for cmt in self.getFileCommits() if not(self._commit_dict.has_key(cmt.id))} )
    
    def cmtHash2CmtObj(self, cmtHash):
        '''
        input: cmtHash
        output: a commit object with additional commit information added
        such as author, committer and date
        '''
        
        #query git for log information on this particular cmtHash 
        logMsg = self._getSingleCommitInfo(cmtHash)
        
        #create commit object from the log message 
        cmtObj = self._LogString2Commit(logMsg[0])
        
        
        return cmtObj
                
    def getFileCommits(self, fname=None, rev_start=None, rev_end=None):
        '''
        returns a list of commit objects from the commits on a given 
        file and revision range. If no revision range is provided 
        the whole history is captured
        '''
         
        #query git to get all committers to a particular file
        #includes commit hash, author and committer data and time
        logMsg = self._getFileCommitInfo(fname, rev_start, rev_end)
            
        #store the commit hash to the fileCommitList
        cmtList = map(self._Logstring2Commit, logMsg)    
            
        return cmtList    
            
    def config4LinuxKernelAnalysis(self, directories=None):
        '''
        use this to configue what files should be included in the 
        file based analysis (ie. non-tag based method). This will 
        query git for the file names and build the list automatically. 
        -- Input --
        directories - a list of paths to limit the search for filenames  
        '''
        

        #build git query 
        cmd = 'git --git-dir={0} ls-tree --name-only --full-tree -r'.format(self.repo).split()
        cmd.append('HEAD')
        if directories:
            cmd.append(directories)
        
        #query git 
        output = self._gitQuery(cmd)
        
        #filter results to only get implementation files (ie *.c) 
        fileNames = [fileName for fileName in output if fileName.endswith(".c")]
        
        self.setFileNames(fileNames)
        
       
        
################### Testing Functions ###########################

    def _testNonTagSNA(self):
        
        git.setRepository("/Users/Mitchell/git/Test_repo/.git")
        self._fileNames = ["Test_py/src/main.py"]
        #fist get all the commits for the range you want
        #build commit objects 
        #clist = self._getCommitIDsFile("Test_py/src/main.py")
        
        self.extractFileCommitData()
        
        #create a list of commit objects for each commit in the file
        fcmtobj = fileCommit.FileCommit()
        
        FileCmtList = map(self._Logstring2Commit, self._getFileCommitInfo(fname))
        fcmtobj.setCommitList(FileCmtList)
        #call git blame for each commit object (this unique possibley for every commit) 
        a = fcmtobj.getCommitList()
        for cmt in fcmtobj.getCommitList():
            blameMsg = git._getBlameMsg(fname, cmt.id)
            
            
            commitLineDict = self._parseBlameMsg(blameMsg, cmt.id)
            
            fcmtobj.addCommitRelationship(cmt.id, commitLineDict)
    
        
        #computer correltion 
        commitLineDict = self._parseBlameMsg(blameMsg)
        


############################ Test cases #########################
if __name__ == "__main__":
    
    #test nontagSNA
    git = gitVCS()
    git.setRepository("/Users/Mitchell/git/Test_repo/.git")
    git._testNonTagSNA()
    
    
    git.setRepository("/Users/Mitchell/git/linux-2.6/.git")
    git.setRevisionRange("22242681cff52bfb7cb~1", "22242681cff52bfb7cb")
    clist = git.extractCommitData("__main__")

    print("Shelfing the git object")
    import shelve
    d = shelve.open("/tmp/git-shelf")
    d["git"] = git
    d.close()

    print("Obtained {0} commits".format(len(clist)))
    for cmt in clist[0:10]:
        print("Commit {0}: {1}, {2}".format(cmt.id, cmt.cdate, cmt.diff_info))
    quit()

    print("Same in blue after unshelfing:")
    k = shelve.open("/tmp/git-shelf")
    git2 = k["git"]
    k.close()

    clist2 = git2.extractCommitData()
    print("Obtained {0} commits".format(len(clist2)))
    for cmt in clist2[0:10]:
        print("Commit {0}: {1}, {2}".format(cmt.id, cmt.cdate, cmt.diff_info))
