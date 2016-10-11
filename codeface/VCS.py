#! /usr/bin/env python

# Define classes to interact with source code management
# systems (currently, only git is supported, but adding
# support for Mercurial and Subversion should be easy to do)
# This class is designed to be serialisable after the data have
# been read in, so the design should remain unaltered whenever possible.
# Processing and experimentation steps that require frequent
# changes must be implemented in different classes.

# This file is part of Codeface. Codeface is free software: you can
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
# Copyright 2010, 2011, by Wolfgang Mauerer <wm@linux-kernel.net>
# Copyright 2012, 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
# All Rights Reserved.

# WM 15-Feb-2010 (Polished and generalised version based on
# various older Perl scripts)

# TODO: When the git implementation is finished, the general parts
# should be factored out and the VCS specific parts be implemented
# for other VCSes. See the comments of the functions on what must be
# VCS-specific.
# TODO: Unify range handling. Either a range is always a list, or always
# represented by two parameters.
import itertools
import readline

import commit
import fileCommit
import re
import os
import bisect
import ctags
import tempfile
import sourceAnalysis
import shutil
from fileCommit import FileDict
from progressbar import ProgressBar, Percentage, Bar, ETA
from ctags import CTags, TagEntry
from logging import getLogger
from codeface.linktype import LinkType

log = getLogger(__name__)
from .util import execute_command

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
        self.rev_start_date = None;
        self.rev_end_date = None;
        self.rev_start     = None;
        self.rev_end       = None;
        self.repo          = None

        # Get commit ranges using dates
        # True: get list of commits made between revision date range
        # False: get list of commits reachable in one revision and not the other
        self.range_by_date = False

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
        return self.rev_start_date

    def getRevEndDate(self):
        return self.rev_end_date

    def getCommitDate(self, rev):
        return self._getRevDate(rev)

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

    def setRangeByDate(self, range_by_date):
        self.range_by_date = range_by_date

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


def parse_sep_line(line):
    if not line.startswith("\"sep="):
        raise ParseError(
            ("expected that the csv file header starts with '\"sep=' "
             "but it started with '{}'")
            .format(line), 'CSVFile')
    stripped = line.rstrip()
    if not stripped.endswith("\""):
        raise ParseError(
            ("expected that the csv file header ends with '\"' "
             "but the line was '{}'")
            .format(line), 'CSVFile')
    return stripped[5:-1]


def parse_line(sep, line):
    """
    Parses a line from a csv file
    :param sep:
    :param line:
    :return:
    """
    def parse_line_enumerator(sep, line):
        # 'test,test' will be '"test,test"' in the csv file
        # 'test"this,"test' will be '"test""this,""test"' in the csv file
        in_quote = False
        ignore_next = False
        current_value = ''
        index = -1
        for c in line:
            index += 1
            assert (c == line[index])
            if ignore_next:
                ignore_next = False
                continue

            if c == '"':
                if in_quote:
                    if len(line) > index + 1 and line[index + 1] == '"':
                        current_value += c
                        ignore_next = True
                    else:
                        in_quote = False
                else:
                    in_quote = True
            elif c == sep and not in_quote:
                yield current_value
                current_value = ''
            else:
                current_value += c
        yield current_value
    return [l.strip() for l in parse_line_enumerator(sep, line)]


class LineType:
    IF = "#if"
    ELSE = "#else"
    ELIF = "#elif"


def parse_feature_line(sep, line):
    """
    parse the current line which is something like:
    FILENAME,LINE_START,LINE_END,TYPE,EXPRESSION,CONSTANTS

    Considering the #ifdef annotation "(!(defined(A)) && (!(defined(B)) && defined(C)",
    the feature expression is the whole string, while the list of feature constants used
    in the annotation (i.e., [A, B, C]).

    :param line: the line to parse
    :return: start_line, end_line, line_type, feature_list, feature_expression
    """
    parsed_line = parse_line(sep, line)
    # FILENAME,LINE_START,LINE_END,TYPE,EXPRESSION,CONSTANTS
    try:
        start_line = int(parsed_line[1])
        end_line = int(parsed_line[2])
        line_type_raw = parsed_line[3]
        if line_type_raw not in (LineType.IF, LineType.ELSE, LineType.ELIF):
            raise ParseError(
                ("could not parse feature line (because we could"
                 "not parse the line_type): \"{}\"")
                .format(line), 'CSVFile')
        line_type = line_type_raw
        if parsed_line[5]:
            feature_list = parsed_line[5].split(';')
        else:
            feature_list = []
        feature_expression = parsed_line[4]

        return start_line, end_line, line_type, feature_list, feature_expression
    except ValueError:
        raise ParseError(
            ("could not parse feature line (most likely because we "
             "could not parse the start- or end-line which should "
             "be on index 2 and 3): \"{}\"")
            .format(line), 'CSVFile')


def get_feature_lines(parsed_lines, filename):
    """
    calculates dictionaries representing the feature sets and the feature expressions
    for any line of the given file.

    - feature expression: e.g., "(!(defined(A)) && (!(defined(B)) && defined(C)"
    - feature set: e.g., [A, B, C] (all used constants in the feature expression)
    :param parsed_lines: a list of tuples with
    (start_line, end_line, line_type, feature_list, feature_expression) elements
    :param filename: the name or the analysed files
    (only used for descriptive error messages if the calculation fails)
    :return tuple of:
    feature_lines: a FileDict object to access the feature sets on any line;
    fexpr_lines: a FileDict object to access the feature expression on any line
    """

    # mapping line -> feature list|feature expression, we only add changing elements
    feature_lines = FileDict()
    feature_lines.add_line(0, [])

    fexpr_lines = FileDict()
    fexpr_lines.add_line(0, [])

    # we want a format like (is_start, features) for every line with an
    # #ifdef (ie. line that changes the feature set)
    annotated_lines = {}
    annotated_lines_fexpr = {}

    def check_line(line, lines_list):
        if line in lines_list:
            raise ParseError(
                ("every line index can be used at most once "
                 "(problematic line was {0} in file {1})")
                    .format(line, filename), filename)

    # We now transform the cppstats output in another output which will
    # help to implement the algorithm below in a simple and fast way.
    # The old format is a list of
    # (start_line, end_line, line_type, feature_list, feature_expression)
    # tuples for every #ifdef/#else.
    # The new format is a list of (is_start, feature_set) and of
    # (is_start, feature_expression, line_type) for every #ifdef(/#else)/#endif.
    # We try to ignore #else wherever possible or handle
    # the #else like a nested #if.
    for start_line, end_line, line_type, feature_list, feature_expression in parsed_lines:
        if not feature_list:
            # empty feature list is something like: '#if 0' or
            # '#if MACRO(0,2)', we ignore those.
            continue
        if start_line >= end_line:
            raise ParseError(
                ("start_line can't be greater or equal to end_line "
                 "(problematic line was {0} in file {1})")
                .format(start_line, filename), filename)

        if line_type == LineType.IF:
            # ifs start on their own line, however the end_line could
            # already be used by the start of an else/elif
            # (#else is the end of the previous #if
            # and the start of another '#if')
            check_line(start_line, annotated_lines)
            if end_line in annotated_lines:
                # in that case we just say the #else line belongs to the
                # virtual starting '#if'
                end_line -= 1
            # Now end_line should be unused
            check_line(end_line, annotated_lines)
            annotated_lines[start_line] = (True, feature_list)
            annotated_lines[end_line] = (False, feature_list)

            # for same lines, the feature expression applies
            check_line(start_line, annotated_lines_fexpr)
            check_line(end_line, annotated_lines_fexpr)
            annotated_lines_fexpr[start_line] = (True, feature_expression, line_type)
            annotated_lines_fexpr[end_line] = (False, feature_expression, line_type)
        else:
            # we try to mostly ignore else and elif if the feature_
            # list doesn't change
            is_start, old_feature_list = annotated_lines[start_line]
            if (not is_start) and old_feature_list == feature_list:
                # We are on an ELSE, however the feature list did not
                # change so we just delete the current line and move the
                # list to the new end
                del annotated_lines[start_line]
                annotated_lines[end_line] = is_start, old_feature_list
            elif is_start:
                raise ParseError(
                    ("line {0} appeared twice as start line "
                     "(problematic file was {1})")
                        .format(start_line, filename), filename)
            else:
                # So we have a elif with different features,
                # so we start more features now end add them to the ending
                # later
                # (so we handle this as if there was a new #ifdef started)
                del annotated_lines[start_line]
                annotated_lines[start_line] = (True, feature_list)
                annotated_lines[end_line] = \
                    (False, old_feature_list + feature_list)

            # a feature expression applies for all times as it is always different to #if branch
            annotated_lines_fexpr[start_line] = (True, feature_expression, line_type)
            annotated_lines_fexpr[end_line] = (False, feature_expression, line_type)


    # Now that we have calculated the annotated_lines we just calculate the
    # feature sets on those lines and save them in a FileDict instance.
    # We can always access the last feature_list with the FileDict
    # (because we sorted the lines)
    for line in sorted(annotated_lines):
        is_start, features = annotated_lines[line]
        # Get last info
        last_feature_list = feature_lines.get_line_info_raw(line)
        # Copy last list and create new list for current line
        new_feature_list = list(last_feature_list)
        if is_start:
            # if the current line starts a new list of features,
            # we just need to add those to
            # the new list (note that order matters in this case).
            for r in features:
                new_feature_list.insert(0, r)
        else:
            # if the current line ends a list of features,
            # we remove them from the list
            # (reverse order as adding).
            for r in reversed(features):
                item = new_feature_list.pop(0)
                assert(item == r)
            # Remove in next line
            # (because we want to count the current #endif line as well).
            line += 1

        feature_lines.add_line(line, new_feature_list)

    # Convert the calculated annotated_lines_fexpr to a FileDict.
    fexpr_stack = [[]] # construct stack of current feature expressions
    for line in sorted(annotated_lines_fexpr):
        is_start, feature_expression, line_type = annotated_lines_fexpr[line]

        if is_start:
            if line_type == LineType.IF:
                # start a new stack item
                fexpr_stack.append([feature_expression])
            else:
                # add to last stack
                fexpr_stack[-1].append(feature_expression)
        else:
            fexpr_stack.pop() # remove last stack item
            line += 1 # Remove in next line (because we want to count the current #endif line as well).

        if fexpr_stack[-1]:
            # if there is an expression in the list, add this one
            fexpr_lines.add_line(line, [fexpr_stack[-1][-1]])
        else:
            # otherwise, add empty list
            fexpr_lines.add_line(line, [])

    # Replace an empty list of features or an empty feature expression with
    # the 'Base_Feature' constant
    feature_lines_with_base = FileDict()
    for line_nr in feature_lines:
        features = feature_lines.get_line_info_raw(line_nr)
        feature_lines_with_base.add_line(line_nr, features if bool(features) else ['Base_Feature'])

    fexpr_lines_with_base = FileDict()
    for line_nr in fexpr_lines:
        fexpr = fexpr_lines.get_line_info_raw(line_nr)
        fexpr_lines_with_base.add_line(line_nr, fexpr if bool(fexpr) else ['Base_Feature'])

    # return feature lines and feature-expression lines
    return (feature_lines_with_base, fexpr_lines_with_base)


def get_feature_lines_from_file(file_layout_src, filename):
    """
    similar to _getFunctionLines but computes the line numbers of each
    feature in the file.
    """
    '''
    - Input -
    file_layout_src:
        dictionary with 'key=line number' and 'value=line of code'
    file_commit: fileCommit instance where the results will be stored

    - Description -
    The file_layout is used to construct a source code file that can be
    parsed by cppstats to generate a cppstats csv file.
    The cppstats csv file is then accessed to extract the feature sets
    and line numbers to be saved in the fileCommit object
    '''

    # grab the file extension to determine the language of the file
    fileExt = os.path.splitext(filename)[1]

    # temporary file where we write transient data needed for cppstats
    srcFile = tempfile.NamedTemporaryFile(suffix=fileExt, delete=False)
    featurefile = tempfile.NamedTemporaryFile(suffix=".csv")
    # generate a source code file from the file_layout_src dictionary
    # and save it to a temporary location
    for line in file_layout_src:
        srcFile.write(line)
    srcFile.flush()

    # run cppstats analysis on the file to get the feature locations
    cmd = "/usr/bin/env cppstats --kind featurelocations --file {0} {1}"\
        .format(srcFile.name, featurefile.name).split()
    try:
        # direct_io is a bit noisy, but there are "silent" errors in
        # cppstats which are only shown in its output
        # and not in the return code. Enable the following line if
        # you think you run into such an error (IE unit tests fail).
        #execute_command(cmd, direct_io=True)
        execute_command(cmd)

        results_file = open(featurefile.name, 'r')
        sep = parse_sep_line(next(results_file))
        headlines = parse_line(sep, next(results_file))
        (feature_lines, fexpr_lines) = \
            get_feature_lines(
                [parse_feature_line(sep, line) for line in results_file],
                filename)

        # clean up temporary files
        srcFile.close()
        featurefile.close()
        os.remove(srcFile.name)
    except (Exception, IOError):
        import sys
        error_type, error_value, traceback = sys.exc_info()
        log.warning("IGNORING cppstats failure ({0}, {1}), "
                    "project file \"{2}\" is left as temp file ({3}) "
                    "for inspection!"
                    .format(error_type, error_value,
                            filename, srcFile.name))
        log.debug("Trace of previous error: {0}".format(traceback))
        empty = FileDict()
        empty.add_line(0, [])
        feature_lines = empty
        fexpr_lines = empty

    # return resulting FileDict instances for feature sets and feature expressions
    return (feature_lines, fexpr_lines)

class gitVCS (VCS):
    def __init__(self):
        VCS.__init__(self) # Python OOP braindamage
        # Some analysis patterns that are required to analyse the
        # output of git
        self.cmtHashPattern = re.compile(r'^\w{40}$')
        # The Log pattern must match : <timestamp> <hash> <timestamp> <date> <time> <timezone>
        self.logPattern = re.compile(r'([0-9]+) ([0-9a-f]+) ([0-9]+) [0-9-]+ [0-9:]+ ([0-9\+\-]+)$')
        self.prettyFormat = "--pretty=format:%ct %H %at %ai"
        self.authorPattern = re.compile(r'^Author: (.*)$')
        self.committerPattern = re.compile(r'^Commit: (.*)$')
        self.signedOffPattern = re.compile(r'^(.*?): (.*)$')
        self.diffStatFilesPattern = re.compile(r'(\d*?) file(|s) changed')
        self.diffStatInsertPattern = re.compile(r' (\d*?) insertion')
        self.diffStatDeletePattern = re.compile(r' (\d*?) deletion')
        # Commit Sign-off patterns
        sign_off_prefix = ("CC:", "Signed-off-by:", "Acked-by:", "Reviewed-by:",
                           "Reported-by:", "Tested-by:", "LKML-Reference:", "Patch:")
        sign_off_prefix = ["("+s+")(\s*)(.*)$" for s in sign_off_prefix]
        self.signOffPatterns = [re.compile(prefix, re.I) for prefix in sign_off_prefix]

    def getDiffVariations(self):
        # We support diffs formed of 2x2 combinations:
        # with and without whitespace sensitivity,
        # and regular/patience diff.
        return 4

    def _getRevDate(self, rev):
         cmd_base = 'git --git-dir={0} log --no-merges --format=%ct -1'.format(self.repo).split()
         cmd = cmd_base + [rev]
         date = execute_command(cmd)
         return date.strip()

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
            log.critical("Repository unset in Git VCS")
            raise Error("Can't do anything without repo")

        ## Retrieve and store the commit timestamp for the revision range
        self.rev_start_date = self._getRevDate(self.rev_start)
        self.rev_end_date = self._getRevDate(self.rev_end)

        # Start with the global list for the whole project
        self._prepareGlobalCommitList()

        # .. and proceed with the subsystems. We "recycle" the already
        # created commit instances by placing them on subsystem specific
        # lists.
        for subsys in self.subsys_description.keys():
            clist = self._getCommitIDsLL(self.rev_start, self.rev_end,
                                         self.subsys_description[subsys])

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
        # interest (currently, only to decide whether a commit is in
        # a feature freeze phase or not)
        if self._rc_ranges != None:
            for range in self._rc_ranges:
                clist = self._getCommitIDsLL(range[0], range[1])
                self._rc_id_list.extend([self._Logstring2ID(logstring)
                                          for logstring in clist])

    def _getCommitIDsLL(self, rev_start, rev_end, dir_list=None):
        """Low-level routine to extract the commit list from the VCS.

        Input:
            rev_start - start of revision range
            rev_end - end of revision range
            dir_list - directory list of a specific subsystems
        Output:
            clist - list of strings representing commits that can be parsed with
                    _Logstring2ID()
        """
        if self.range_by_date:
            start_date = self._getRevDate(rev_start)
            end_date = self._getRevDate(rev_end)
            rev_range = ['--since=' + start_date,
                         '--before=' + end_date]

        else:
            rev_range = ['{0}..{1}'.format(rev_start, rev_end)]

        # TODO: Check the effect that -M and -C (to detect copies and
        # renames) have on the output. Is there anything we need
        # to take into account?
        # First, get the output for the complete revision
        # range (TODO: cache this)
        # NOTE: %H prints the hash value of the commit, %ct denotes
        # the comitter date (it's important to use comitter and not
        # author date; this guarantees monotonically increasing time
        # sequences). %at gives the author timestamp, and %ai gives the
        # time in a format where we can extract the time zone
        # Passing a simple formatted string and using getoutput() to
        # obtain the result is way nicer in python3.
        cmd = 'git --git-dir={0} log -M -C'.format(self.repo).split()
        cmd.append('--no-merges')
        cmd.append(self.prettyFormat)
        cmd.extend(rev_range)

        if dir_list is not None:
            cmd.append("--")
            cmd.extend(dir_list)

        clist = execute_command(cmd).splitlines()

        # Remember the comment about monotonically increasing time sequences
        # above? True in principle, but unfortunately, a very small number
        # of commits can violate this for various reasons. Since these
        # outliers screw up the cumulative graph, we have to add an
        # extra sorting pass.
        clist.sort(reverse=True)

        return clist

    def _getSingleCommitInfo(self, cmtHash):
        #produces the git log output for a single commit hash

        #build git command
        cmd = 'git --git-dir={0} log'.format(self.repo).split()
        cmd.append(cmtHash)
        cmd.append("-1")
        cmd.append(self.prettyFormat)

        #submit query to git
        logMsg = execute_command(cmd).splitlines()

        return logMsg

    def _getFileCommitInfo(self, fname=None, rev_start=None, rev_end=None):
        '''extracts a list of commits on a specific file for the specified
        revision range'''

        revrange = ""

        if rev_start == None and rev_end != None:
            revrange += rev_end
        else:
            if rev_start:
                revrange += "{0}..".format(rev_start)

            if rev_end:
                revrange += rev_end


        #build git command
        #we must take merge commits, otherwise when we cross-reference with
        #git blame some commits will be missing
        cmd = 'git --git-dir={0} log --no-merges -M -C'.format(self.repo).split()
        cmd.append(self.prettyFormat)
        if rev_start and rev_end:
            cmd.append(revrange)
        if fname:
            cmd.append("--")
            cmd.append(fname)


        #submit query command
        clist = execute_command(cmd).splitlines()

        # Remember the comment about monotonically increasing time sequences
        # above? True in principle, but unfortunately, a very small number
        # of commits can violate this for various reasons. Since these
        # outliers screw up the cumulative graph, we have to add an
        # extra sorting pass.
        clist.sort(reverse=True)

        # Then, obtain the first and last commit in the desired range
        # and extract the desired subrange
        return clist

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
        clist = self._getCommitIDsLL(self.rev_start, self.rev_end)

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
            log.critical("_Logstring2ID could not parse log string!")
            raise Error("_Logstring2ID could not parse log string!")

        return match.group(2)

    def _Logstring2Commit(self, str):
        """Create an instance of commit.Commit from a given log string.

        Must be implemented by every VCS. Turns a string from the list
        returned by getCommitIDsLL into a commit.Commit instance"""

        match = self.logPattern.search(str)
        if not(match):
            log.critical("_Logstring2Commit could not parse log string!")
            raise Error("_Logstring2Commit could not parse log string!")

        cmt = commit.Commit()
        cmt.cdate = match.group(1)
        cmt.id = match.group(2)
        cmt.adate = match.group(3)
        # This integer is not really a number, rather a representation of the
        # hour:minute offset to UTC. E.g. "+0200" becomes 200. To extract the
        # hourly offset, use integer division.
        cmt.adate_tz = int(match.group(4))

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

            # Check if this is a genuine empty commit
            # Since the commit message is indented by 4 spaces
            # we check if the last line of the commit still starts this way.
            if not matched and msg[-1].startswith("    "):
                log.devinfo("Empty commit. Commit <id {}> is: '{}'".
                            format(cmt.id, msg))
                matched = True

        except IndexError:
            log.error("Empty commit?! Commit <id {}> is: '{}'".
                    format(cmt.id, msg))
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
                    msg = execute_command(cmd)
                    self._analyseDiffStat(msg, cmt)
                except UnicodeDecodeError:
                    # Since we work in utf8 (which git returns and
                    # Python is supposed to work with), this exception
                    # seems to stem from a faulty encoding. Just
                    # ignore the commit
                    cmt.diff_info.append((0,0,0))
                    log.warning("Ignoring commit {} due to unicode error.".
                            format(pe.id))
                except ParseError as pe:
                    # Since the diff format is very easy to parse,
                    # this most likely stems from a malformed diff
                    # that can be ignored. Nevertheless, report the
                    # line and the commit id
                    log.error("Could not parse diffstat for {0}!".
                          format(pe.id))
                    log.error("{0}".format(pe.line))
                    cmt.diff_info.append((0,0,0))
                except OSError:
                    log.exception("Could not spawn git")
                    raise

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
                # On some occasions, this
                # error is triggered although the commit in question
                # (e.g., 9d32c30542f9ec) can be parsed
                # fine when viewed within a different range...?!
                # Perhaps this is also a pypy issue. For the commit
                # mentioned before, the problem was only detected with
                # pypy, not with cpython.
                log.critical("Cannot find metadata start in commit message!")
                raise Error("Cannot find metadata start in commit message!")

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

        descr = parts[descr_index].split("\n")

        # Check if commit is corrective using key word search of description
        cmt.checkIfCorrective(descr)

        # Add commit description to commit object
        cmt.setDescription(descr)

        # Ensure that there are actually sign off tags in the commit message
        found = False
        i = 0
        for line in descr:
            line = line.lstrip()
            found = any([prefix.match(line) for prefix in self.signOffPatterns])
            if found:
                break
            i+=1

        if found:
            descr_message = "\n".join(parts[descr_index].
                                      split("\n \n ")[0:i-1])
            self._analyseSignedOffs(descr[i:], cmt)
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
        for entry in msg:
            entry = entry.lstrip()
            matches = [tag.search(entry) for tag in self.signOffPatterns
                       if tag.search(entry)!=None]
            if (matches):
                match = matches[0]
                key = match.group(1).replace(" ", "").replace(":", "")
                value = match.group(3)
                if key in tag_names_list.keys():
                    tag_names_list[key].append(value)
                else:
                    tag_names_list[key] = [value]
            else:
                log.debug("Could not parse Signed-off like line:")
                log.debug('{0}'.format(entry))

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
            log.critical("Bogus range")
            raise Error("Bogus range")

        globData = self.extractCommitData(subsys)

        # NOTE: __main__ is a pseudo-subsystem that is not contained
        # in the subsystem description
        if subsys=="__main__":
            clist = self._getCommitIDsLL(revrange[0], revrange[1])
        else:
            clist = self._getCommitIDsLL(revrange[0], revrange[1],
                                         self.subsys_description[subsys])

        return [self._commit_dict[self._Logstring2ID(logstring)]
                for logstring in reversed(clist)]


    def extractCommitData(self, subsys="__main__", link_type=None):
        if not(self._subsysIsValid(subsys)):
            log.critical("Subsys specification invalid: {0}\n".format(subsys))
            raise Error("Invalid subsystem specification.")

        # If we've already computed the result, make use of it
        # (shelved objects can therefore provide a significant
        # performance advantage)
        if self._commit_list_dict:
            log.devinfo("Using cached data to extract commit information")
            return self._commit_list_dict[subsys]

        self._prepareCommitLists()

        if link_type in (LinkType.proximity, LinkType.file,
                         LinkType.feature, LinkType.feature_file):
            self.addFiles4Analysis(self._commit_dict.keys())
            self._prepareFileCommitList(self._fileNames, link_type=link_type)

        # _commit_list_dict as computed by _prepareCommitLists() already
        # provides a decomposition of the commit list into subsystems:
        # It suffices to analyse the commits in the global commit list
        # __main__, the subsystem information follows automatically
        # from this.
        count = 0
        widgets = ['Pass 1/2: ', Percentage(), ' ', Bar(), ' ', ETA()]
        pbar = ProgressBar(widgets=widgets,
                           maxval=len(self._commit_dict)).start()

        for cmt in self._commit_dict.values():
            count += 1
            if count % 20 == 0:
                pbar.update(count)

#            print("Processing commit {0}/{1} ({2})".
#                  format(count, len(self._commit_list_dict["__main__"]),
#                         cmt.id))

            self._parseCommit(cmt)

        pbar.finish()
        # For the subsystems, we need not re-analyse the commits again,
        # but can just pick the results from the global analysis.
        # Which was already done by _prepareCommitLists() ;-)

        return self._commit_list_dict[subsys]


    def _getBlameMsg(self, fileName, rev):
        '''provided with a filename and revision the function returns
        the blame message'''

        #build command string
        cmd = 'git --git-dir={0} blame'.format(self.repo).split()
        cmd.append("-p") #format for machine consumption
        cmd.append("-w") #ignore whitespace changes
        cmd.append("-C") #find copied code (attribute to original)
        cmd.append("-M") #find moved code (attribute to original)
        cmd.append(rev)
        cmd.append("--")
        cmd.append(fileName)

        #query git repository
        blameMsg = execute_command(cmd).splitlines()

        return blameMsg


    def _parseBlameMsg(self, msg):
        '''input a blame msg and the commitID under examination the
        output contains code line numbers and corresponding commitID'''

        #variable declarations
        commitLineDict = {} #dictionary, key is line number, value is commit ID
        codeLines      = [] # list of source code lines

        while msg:

            line = msg.pop(0).split(" ")

            #the lines we want to match start with a commit hash
            if(self.cmtHashPattern.match( line[0] ) ):

               lineNum    = str(int(line[2]) - 1)
               commitHash = line[0]

               commitLineDict[lineNum] = commitHash

            # check for line of code, signaled by a tab character
            elif line[0].startswith('\t'):
               line[0] = line[0][1:] # remove tab character
               codeLines.append(' '.join(line) + '\n')


        return (commitLineDict, codeLines)


    def _prepareFileCommitList(self, fnameList, link_type, singleBlame=True,
                               ignoreOldCmts=True):
        '''
        uses git blame to determine the file layout of a revision
        '''
        '''
        The file layout is a dictionary that indicates which commit hash is
        responsible for each line of code in a file. The blame data can be
        recorded for each commit or only for one revision.
        - Input -
        fnameList: a list of file names for which to capture the blame data
        singleBlame: when set true only only the latest revision blame is called
                     if set false blame data will be captured for every commit
                     made during the specificed revision range, caution: if set
                     false the computation become extremely intensive. The
                     disadvantage of setting true is if two commits
                     to the same line of code are made withing the revision
                     range only the most recent committed line will be captured.
        ignoreOldCmts: commits made to the file before the start of the revision
                        range will be ignored from the analysis. That is to say
                        lines of code that existed prior to the revision start
                        are ignored.
        '''


        #variable initialization
        if self._fileCommit_dict is None:
            self._fileCommit_dict = {}

        blameMsgCmtIds = set() #stores all commit Ids seen from blame messages

        if self._commit_dict is None:
            self._commit_dict = {}

        if len(fnameList) == 0:
            return

        count = 0
        widgets = ['Blame Analysis: ', Percentage(), ' ', Bar(), ' ', ETA()]
        pbar = ProgressBar(widgets=widgets,
                           maxval=len(fnameList)).start()

        for fname in fnameList:
            count += 1
            if count % 20 == 0:
                pbar.update(count)

            #create fileCommit object, one per filename to be
            #stored in _fileCommit_dict
            file_commit = fileCommit.FileCommit()
            file_commit.filename = fname

            #get commit objects for the given file within revision range
            cmtList  = []
            cmtList  = self.getFileCommits(fname, self.rev_start, self.rev_end)

            #store commit hash in fileCommit object, store only the hash
            #and then reference the commit db to get the object
            file_commit.setCommitList([cmt.id for cmt in cmtList])

            #store the commit object to the committerDB, the justification
            #for splitting this way is to avoid redundant commit info
            #since a commit can touch many files, we use the commit
            #hash to reference the commit object (author, date etc)
            self._commit_dict.update({cmt.id:cmt for cmt in cmtList
             if cmt.id not in self._commit_dict})

            #Determine the revision that git blame will be called on
            if self.range_by_date:
                #Find the revision where the last change was applied up until the
                #the end of the analysis time window specified by a date
                cmd = 'git --git-dir={0} log'.format(self.repo).split()
                cmd.append("--until={0}".format(self.rev_end_date))
                cmd.append("--format=%H")
                cmd.append("--follow")
                cmd.append("--diff-filter=ACMRTB")
                cmd.append("-1")
                cmd.append("--")
                cmd.append(file_commit.filename)
                rev = execute_command(cmd).strip()

                if rev=="":
                  rev = self.rev_end
            else:
                #Use revision that represents the final commit for the specified
                #revision range
                rev = self.rev_end

            # Check if file has been deleted
            cmd = "git --git-dir={0} ls-tree".format(self.repo).split()
            cmd.append("--name-only")
            cmd.append("--full-tree")
            cmd.append("-r")
            cmd.append(rev)
            existing_files = execute_command(cmd).split()
            if file_commit.filename in existing_files:
                # retrieve blame data
                if singleBlame: #only one set of blame data per file
                    self._addBlameRev(rev, file_commit,
                                      blameMsgCmtIds, link_type)
                else: # get one set of blame data for every commit made
                    # this option is computationally intensive thus the alternative
                    # singleBlame option is possible when speed is a higher
                    # priority than precision
                    [self._addBlameRev(cmt.id, file_commit,
                                       blameMsgCmtIds, link_type) for cmt in cmtList]

                #store fileCommit object to dictionary
                self._fileCommit_dict[fname] = file_commit

        #end for fnameList
        pbar.finish()

        #-------------------------------
        #capture old commits
        #-------------------------------
        '''
         If the analysis of the git blame messages is focused
         only on a temporal region (ie. not entire history) it
         can be the case that commits not found during the git
         log query (due to revision range) are present in the
         git blame messages. We must query git again to get the
         remaining commits otherwise we cannot reference them during
         the blame message analysis.
         '''

        if not(ignoreOldCmts):
            #find all commits that are missing from the commit dictionary
            #recall that fileCommit_dict stores all the commit ids for a
            #file to reference commit objects in the commit_dict
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

                pbar.finish()

    def _addBlameRev(self, rev, file_commit, blame_cmt_ids, link_type):
        '''
        saves the git blame output of a revision for a particular file
        '''
        '''
        -Input-
        rev: a revision, could be a commit hash or "v2.6.31"
        fname: string of a filename to call git blame on
        file_commit: a fileCommit object to store the resulting blame data
        blame_cmt_ids: a list to keep track of all commit ids seen in the blame
        '''

        #query git reppository for blame message
        blameMsg = self._getBlameMsg(file_commit.filename, rev)

        #parse the blame message, this extracts the line number
        #and corresponding commit hash, returns a dictionary
        #Key = line number, value = commit hash
        #basically a snapshot of what the file looked like
        #at the time of the commit
        (cmt_lines, src_lines) = self._parseBlameMsg(blameMsg)

        #store the dictionary to the fileCommit Object
        file_commit.addFileSnapShot(rev, cmt_lines)

        # locate all function lines in the file
        if link_type == LinkType.proximity:
            # separate the file commits into code structures
            self._getFunctionLines(src_lines, file_commit)
        elif link_type in (LinkType.feature_file, LinkType.feature):
            file_commit.set_feature_infos(
                get_feature_lines_from_file(src_lines, file_commit.filename))

        # else: do not separate file commits into code structures,
        #       this will result in all commits to a single file seen as
        #       related thus the more course grained analysis

        blame_cmt_ids.update( cmt_lines.values() )

    def _parseSrcFileDoxygen(self, src_file):
        log.debug("Running Doxygen analysis")
        curr_dir = os.path.dirname(os.path.abspath(__file__))
        conf_file = os.path.join(curr_dir, 'doxygen.conf')
        tmp_outdir = tempfile.mkdtemp()
        file_analysis = sourceAnalysis.FileAnalysis(src_file,
                                                    conf_file,
                                                    tmp_outdir)

        try:
            file_analysis.run_analysis()
        except Exception, e:
            log.critical("doxygen analysis error{0} - defaulting to Ctags".format(e))
            return {}, []

        # Delete tmp directory storing doxygen files
        shutil.rmtree(tmp_outdir)

        # Get src element bounds
        func_lines = {}
        for elem in file_analysis.src_elem_list:
            # Doxygen analysis index starts at 1
            start = int(elem['bodystart']) - 1
            end = int(elem['bodyend']) - 1
            name = elem['name']
            f_lines = {line_num:name  for line_num in range(start, end+1)}
            func_lines.update(f_lines)

        return func_lines, file_analysis.src_elem_list

    def _parseSrcFileCtags(self, src_file):
        # temporary file where we write transient data needed for ctags
        tag_file = tempfile.NamedTemporaryFile()

        # run ctags analysis on the file to create a tags file
        cmd = "ctags-exuberant -f {0} --fields=nk {1}".format(tag_file.name,
                                                              src_file).split()
        output = execute_command(cmd).splitlines()

        # parse ctags
        try:
            tags = CTags(tag_file.name)
        except:
            log.critical("failure to load ctags file")
            raise Error("failure to load ctags file")

        # locate line numbers and structure names
        entry = TagEntry()
        func_lines = {}
        # select the language structures we are interested in identifying
        # f = functions, s = structs, c = classes, n = namespace
        # p = function prototype, g = enum, d = macro, t= typedef, u = union
        structures = ["f", "s", "c", "n", "p", "g", "d", "t", "u"]
        # TODO: investigate other languages and how ctags assigns the
        #  structures tags, we may need more languages specific assignments
        #  in addition to java and c# files, use "ctags --list-kinds" to
        # see all tag meanings per language
        fileExt = os.path.splitext(src_file)[1].lower()
        if fileExt in (".java", ".j", ".jav", ".cs", ".js"):
            structures.append("m") # methods
            structures.append("i") # interface
        elif fileExt in (".php"):
            structures.append("i") # interface
            structures.append("j") # functions
        elif fileExt in (".py"):
            structures.append("m") # class members

        while(tags.next(entry)):
            if entry['kind'] in structures:
                ## Ctags indexes starting at 1
                line_num = int(entry['lineNumber']) - 1

                ## Ctags sometimes assigns line numbers 0 in .js files
                if line_num < 0:
                    line_num = 0

                func_lines[line_num] = entry['name']

        # clean up temporary files
        tag_file.close()

        return func_lines

    def _getFunctionLines(self, file_layout_src, file_commit):
        '''
        computes the line numbers of each function in the file
        '''
        '''
        - Input -
        file_name: original name of the file, used only to determine the
                    programming language (ie. file.c is a c-language file)
        file_layout_scr: dictionary with key=line number value = line of code
        file_commit: fileCommit instance where the results will be stored

        - Description -
        The file_layout is used to construct a source code file that can be
        parsed by ctags to generate a ctags file. The ctags file is then
        accessed to extract the function tags and line numbers to be save in
        the fileCommit object
        '''

        # grab the file extension to determine the language of the file
        fileExt = os.path.splitext(file_commit.filename)[1].lower()

        # setup temp file
        # generate a source code file from the file_layout_src dictionary
        # and save it to a temporary location
        srcFile = tempfile.NamedTemporaryFile(suffix=fileExt)
        for line in file_layout_src:
            srcFile.write(line)
        srcFile.flush()

        # For certain programming languages we can use doxygen for a more
        # precise analysis
        func_lines = {}
        if (fileExt in ['.java', '.cs', '.d', '.php', '.php4', '.php5',
                        '.inc', '.phtml', '.m', '.mm', '.py', '.f',
                        '.for', '.f90', '.idl', '.ddl', '.odl', '.tcl',
                        '.cpp', '.cxx', '.c', '.cc']):
            func_lines, src_elems = self._parseSrcFileDoxygen(srcFile.name)
            file_commit.setSrcElems(src_elems)
            file_commit.doxygen_analysis = True

        if not func_lines: # for everything else use Ctags
            func_lines = self._parseSrcFileCtags(srcFile.name)
            file_commit.doxygen_analysis = False

        # clean up src temp file
        srcFile.close()

        # save result to the file commit instance
        file_commit.setFunctionLines(func_lines)

        # save the implementation for each function
        rmv_char = '[.{}();:\[\]]'
        for line_num, src_line in enumerate(file_layout_src):
            src_line_rmv = re.sub(rmv_char, ' ', src_line.strip())
            file_commit.addFuncImplLine(line_num, src_line_rmv)


    def cmtHash2CmtObj(self, cmtHash):
        '''
        input: cmtHash
        output: a commit object with additional commit information added
        such as author, committer and date
        '''

        #query git for log information on this particular cmtHash
        logMsg = self._getSingleCommitInfo(cmtHash)

        #create commit object from the log message
        cmtObj = self._Logstring2Commit(logMsg[0])


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

    def addFiles4Analysis(self, cmt_id_list):
        '''
        use this to configue what files should be included in the
        file based analysis (ie. non-tag based method). This will
        query git for the file names and build the list automatically.
        -- Input --
        directories - a list of paths to limit the search for filenames
        '''
        cmd_base = 'git --git-dir={0} diff-tree'.format(self.repo).split()
        cmd_base.append("--diff-filter=ACMRTB")
        cmd_base.append("--no-commit-id")
        cmd_base.append("--name-only")
        cmd_base.append("-r")

        #get all files touched by all commits
        all_files = set()
        for cmt_id in cmt_id_list:
            cmd = cmd_base + [cmt_id]
            cmt_files = execute_command(cmd).splitlines()
            all_files.update(cmt_files)

        #filter results to only get implementation files
        fileExt = (".c", ".cc", ".cpp", ".cxx", ".cs", ".asmx", ".m", ".mm",
                   ".js", ".java", ".j", ".jav", ".php",".py", ".sh", ".rb",
                   '.d', '.php4', '.php5', '.inc', '.phtml', '.m', '.mm',
                   '.f', '.for', '.f90', '.idl', '.ddl', '.odl', '.tcl')

        fileNames = [fileName for fileName in all_files if
                     fileName.lower().endswith(fileExt)]

        self.setFileNames(fileNames)



################### Testing Functions ###########################

    def _testNonTagSNA(self,git):

        testRepo = "/home/au/workspace/codeface/.git"
        git.setRepository(testRepo)
        self._fileNames = ["cluster/cluster.py"]

        self.extractFileCommitData()

    def _testCtagsFunctionality(self):
       # setup
       filePath = '/Users/Mitchell/Documents/workspace/codeface/cluster/cluster.py'
       srcFile = open(filePath, 'r')
       file_layout = []
       line_number = 0
       for line in srcFile:
           file_layout.append(line)

       # test function call
       file_commit = fileCommit.FileCommit()
       file_commit.filename = filePath
       funcLines = self._getFunctionLines(file_layout, file_commit)


############################ Test cases #########################
if __name__ == "__main__":

    #test nontagSNA
    git = gitVCS()
    git._testNonTagSNA(git)


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
