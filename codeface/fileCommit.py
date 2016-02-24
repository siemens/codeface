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
# Copyright 2013, Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
# All Rights Reserved.

"""This class is a container to represent a commits relationship to the
other commits present in a particular file at the time of the commit.
The analysis is performed on a file by file basis. A commit can touch multiple
files however this class considers a given commit only in the context of a
single file.
"""

import bisect


class FileDict(object):
    """
    A generic dictionary for saving per-line information.
    We assume that this information is available on any line,
    and that the information only changes on some lines.
    So we only save the information on lines that change that info
    and use bisect to retrieve that information (for any line).
    """
    # TODO Candidate for deletion?
    def __init__(self, line_list, line_dict):
        """
        :rtype : FileDict
        """
        self.line_list = line_list
        self.line_dict = line_dict
        self.lastItem = line_list[-1]

    def __init__(self):
        """
        :rtype : FileDict
        """
        self.line_list = []
        self.line_dict = {}
        self.lastItem = -1

    def __iter__(self):
        return self.line_dict.__iter__()

    def get_line_info_raw(self, line_nr):
        """
        Returns the info for the given line
        (if the line was never set, the info for the last set line
        is returned)
        :param line_nr: the line to retrieve the information for.
        :return: the information for the given line.
        """
        i = bisect.bisect_right(self.line_list, line_nr)
        info_line = self.line_list[i - 1]
        return self.line_dict[info_line]

    def get_line_info(self, line_nr):
        return set(self.get_line_info_raw(line_nr))

    def add_line(self, line_nr, info):
        """
        Add the given information to the current dictionary.
        Note: while filling the dictionary the line_nr argument has to
        be incremented (this is only to make sure the caller
        gets the intended behavior)!
        :param line_nr: the line number of the information
        :param info: the information for the current line
        """
        if line_nr < self.lastItem:
            raise ValueError("can only incrementally add items")
        self.line_list.append(line_nr)
        self.line_dict[line_nr] = info

    def values(self):
        return self.line_dict.values()


class FileCommit(object):
    """Container for tracking all features and contributions to a single file.

    The state, contributions and features stored by an instance of this class
    represent the state of the file at a specific commit.

    Attributes:
        filename (str): Filename under investigation.
        fileSnapShots (dict): Dictionary of dicts, mapping commits to
            dicts mapping line numbers to commits, as seen on the first key.
            E.g.: {'commit': {'1': 'commit1', '2': 'commit2', ...}, ...}
            This is essentially a list of blame snapshots for each commit.
        revCmts (list): List of all commit hashes contributing to this file.
        doxygen_analysis (bool): Flag if the file was analysed with Doxygen.
            False can either mean "Analyzed with ctags" or "Not analyzed yet".
        feature_info (FileDict):
        feature_expression_info (FileDict):
    """
    # TODO Inconsistent naming scheme for methods
    def __init__(self):

        # filename under investigation
        self.filename = None

        # dictionary of dictionaries key is commit, value is a
        # dictionary with keys=lineNumbers value=commitHash, stores
        # the line number and corresponding commit hash for every
        # line of the file,
        self.fileSnapShots = {}

        # stores the commit hash of all contributions to the file for a
        # particular revision
        self.revCmts = []

        # dictionary with key = line number, value = function name
        file_level = -1
        # TODO Should this really be a public attribute?
        self.functionIds = {file_level: 'File_Level'}

        # list of function line numbers in sorted order, this is for
        # optimizing the process of finding a function Id given a line number
        # TODO Should this really be a public attribute?
        self.functionLineNums = [file_level]

        # Function Implementation
        # TODO Should this really be a public attribute?
        self.functionImpl = {}

        # doxygen flag
        # TODO doxygen_analysis = False has an ambiguous meaning!
        self.doxygen_analysis = False

        # source code element list
        # stores all source code elements of interest and
        # meta data
        # TODO Write only attribute?
        self._src_elem_list = []

        # dictionaries with key = line number,
        # value = feature list|feature expression
        self.feature_info = FileDict()
        self.feature_expression_info = FileDict()

    def getFileSnapShots(self):
        """Get all file snapshots.

        Returns:
            dict: Dictionary mapping commit hashes to dicts of line numbers
            mapping to commit hashes. E.g.:
            {'commit': {'1': 'commit1', '2': 'commit2', ...}, ...}
        """
        return self.fileSnapShots

    def getFileSnapShot(self):
        """Get the most recent file snapshot.

        Returns:
            dict: Dictionary of line numbers to commit hashes.
            E.g. {'1': 'commit1', '2': 'commit2', ...}
        """
        return self.fileSnapShots.values()[0]

    def getFilename(self):
        return self.filename

    def setCommitList(self, cmtList):
        self.revCmts = cmtList

    def getrevCmts(self):
        return self.revCmts

    def getFuncImpl(self, id):
        """Fetch the implementation of a specific function.

        Args:
            id (str): Function id, as returned by `findFuncId`.

        Returns:
            list: List of strings, representing each line of the implementation.
            The list is empty if the implementation or the function is unknown.

        """
        if id in self.functionImpl:
            return self.functionImpl[id]
        else:
            return []

    def setFunctionLines(self, functionIds):
        """Flags lines as belonging to a specific function.

        Notes:
            For successive calls, the line numbers used as keys must be
            strictly ascending, or it will cause unexpected side effects.

            Calls to this method have the side effect of discarding all
            previously recorded function implementations.

        Args:
            functionIds (dict): Dict mapping line numbers to function ids.
        """
        # TODO Is the key for functionIds a function id or a line number?
        self.functionIds.update(functionIds)
        for id in self.functionIds.values():
            self.functionImpl.update({id: []})
        # TODO Why does this discard ALL of functionImpl?
        self.functionLineNums.extend(sorted(self.functionIds.iterkeys()))
        # TODO dict.iterkeys() should be replaced by dict.keys()
        # There is no advantage of using iterkeys, it stops being a hard
        # reference the moment it's sorted.
        # TODO This does not guarantee that functionLineNums remains sorted!

    def setSrcElems(self, src_elem_list):
        self._src_elem_list.extend(src_elem_list)

    def set_feature_infos(self, feature_line_infos):
        self.feature_info = feature_line_infos[0]
        self.feature_expression_info = feature_line_infos[1]

    def addFileSnapShot(self, commit, snapshot):
        self.fileSnapShots[commit] = snapshot

    def findFuncId(self, line_num):
        """Find the function ID belonging to a given line number.

        Notes:
            The behavior of this method is controlled by `doxygen_analysis`.
            For `doxygen_analysis` = True, every line is expected to be tagged,
            untagged lines are treated as "File_Level" scope.
            For `doxygen_analysis` = False, untagged lines are treated as
            belonging to the same function as the last tagged line.

        Args:
            line_num (int): The line number.

        Returns:
            str: Name of the function, defaults to "File_Level" if the line
            can't be attributed to a specific function.

        """
        func_id = 'File_Level'
        line_num = int(line_num)
        # TODO Why does this method need to be aware who generated the index?
        # The index structure should be normalized to follow the Doxygen style!
        if self.doxygen_analysis:
            if line_num in self.functionIds:
                func_id = self.functionIds[line_num]
        else:
            i = bisect.bisect_right(self.functionLineNums, line_num)
            func_line = self.functionLineNums[i - 1]
            func_id = self.functionIds[func_line]
        return func_id

    def getLineCmtId(self, line_num):
        """Get the commit hash of the latest commit contributing to a line.

        Args:
            line_num (int): The line number.

        Returns:
            str: Commit hash of the latest contribution to the line.
        """
        line_num = str(line_num)
        # Retrieve the first file snap
        file_snapshot = self.getFileSnapShot()
        return file_snapshot[line_num]

    def getLength(self):
        return len(self.getFileSnapShot())

    def getIndx(self):
        return self.getFileSnapShot().keys()

    def addFuncImplLine(self, lineNum, srcLine):
        """Add a single source code line to the implementation of a function.

        Notes:
            For correct function, `setFunctionLines` needs to have been called
            before. Otherwise, the source would be attributed to "File_Level".

            Calling `setFunctionLines` will discard all source recorded.

        Args:
            lineNum (int): Original line number.
            srcLine (str): Line content.
        """
        id = self.findFuncId(lineNum)
        self.functionImpl[id].append(srcLine)

    def findFeatureList(self, line_index):
        return self.feature_info.get_line_info(int(line_index) + 1)

    def findFeatureExpression(self, line_index):
        return self.feature_expression_info.get_line_info(int(line_index) + 1)
