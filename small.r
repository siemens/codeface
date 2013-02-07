source("includes.r")

repo.path <- "/home/wolfgang/projects/zk/QuantArch/nntp/repos/"
#ml <- "gmane.os.freebsd.architechture"
#ml <- "gmane.linux.kernel"
#ml <- "gmane.comp.version-control.git"
ml <- "gmane.comp.lang.d.general"
#ml <- "gmane.comp.compilers.clang.devel"
#ml <- "gmane.comp.mozilla.general"
#ml <- "gmane.os.openbsd.tech"
data.path <- "/media/disk/QA/"

## TODO: Filter out spam. There's an incredible amount in some gmane archives
## TODO: (this should also include filtering out non-english messages)
## The easiest thing would be to let SpamAssassin run over the mbox
## file, and then delete all messages marked as SPAM.

date()
options(error=recover)
tm_startCluster()
for (ml in c(#"gmane.comp.compilers.clang.devel",
             "gmane.comp.lang.d.general",
#             "gmane.comp.version-control.git",
#             "gmane.linux.kernel",
##             "gmane.os.freebsd.architechture",
             "gmane.os.openbsd.tech")
             ) {
  dispatch.all(ml, repo.path, data.path, doCompute=TRUE)
}
tm_stopCluster()
date()
