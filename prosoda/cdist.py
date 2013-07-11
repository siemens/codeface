#! /usr/bin/env python
# Analyse a given source code repository and
# store the resulting data structure using serialisation

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

from VCS import gitVCS
from commit_analysis import createCumulativeSeries, createSeries
import kerninfo

git = gitVCS();
git.setRepository("/home/wolfgang/git-repos/linux-2.6/.git")
git.setRevisionRange("v2.6.14", "v2.6.33")
#git.setRevisionRange("v2.6.23", "v2.6.26")
#git.setRepository("/home/wolfgang/git-repos/perl/.git")
#git.setRevisionRange("8d063cd8450e", "HEAD")
#git.setSubsysDescription(kerninfo.subsysDescrLinux)

git.extractCommitData()

###################################################
print("Shelfing the git object")
import shelve
d = shelve.open("/home/wolfgang/linux-14-33")
d["git"] = git
d.close()
#
#print("Same in blue after unshelfing:")
#k = shelve.open("/tmp/git-shelf")
#git2 = k["git"]
#k.close()
###################################################

#res = createCumulativeSeries(git, "__main__")
#res = createCumulativeSeries(git, "block")
res = createSeries(git, subsys="__main__", revrange=["v2.6.23", "v2.6.26"])
print("Obtained a list with {0} commits".format(len(res)))

for i in range(0,10):
    print("{0}: {1}, {2}".format(res[i]["commit"].cdate, res[i]["value"][0],
                                 res[i]["commit"].getCommitMessageLines()))
