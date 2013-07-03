source("includes.r")

repo.path <- "/home/wolfgang/projects/zk/QuantArch/nntp/repos/"
#ml <- "gmane.os.freebsd.architechture"
#ml <- "gmane.linux.kernel"
#ml <- "gmane.comp.version-control.git"
#ml <- "gmane.comp.lang.d.general"
ml <- "gmane.comp.compilers.clang.devel"
#ml <- "gmane.comp.mozilla.general"
#ml <- "gmane.os.openbsd.tech"
data.path <- "/media/disk/QA/"

load(file.path(data.path, ml, "weekly.17", paste("vis.data", ml, sep=".")))
summary(res$networks.dat$ir[c("x", "y")])
res$networks.dat$ir[c("x", "y")]

load(file.path(data.path, ml, "weekly.1", paste("forest.corp", ml, sep=".")))

head(res$networks.dat$ir)
dispatch.plots("/tmp/plots", res)

networks.subj <- gen.cmp.networks(res$interest.networks$subject, res$communication.network)
