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
# Copyright 2013, Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
# All Rights Reserved.

import codeLine

class codeBlock:
    '''
    A code block is a contiguous set of lines of code from a single
    commit
    '''


    #===========================
    # Constructors
    #===========================

    def __init__(self, start=None, end=None, authorId=None, committerId=None, cmtHash=None):

        self.start       = start #start of the code block
        self.end         = end   #end of the code block
        self.authorId    = authorId
        self.committerId = committerId
        self.cmtHash     = cmtHash
    #===========================
    # Accessors and Modifiers
    #===========================

    def get_codeLines(self):
        return self.codeLines


    def add_codeLine(self, lineNum, cmtHash, authorId, committerId):
        self.codeLines.append( codeLine.codeLine(lineNum, cmtHash, authorId, committerId) )