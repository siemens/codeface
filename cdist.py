#! /usr/bin/env python
# Analyse a given source code repository and 
# store the resulting data structure using serialisation

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

