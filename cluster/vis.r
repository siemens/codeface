# Some snippets for visualising the data obtained from cluster.r
library(ggplot2)
library(lattice)
library(car) # for spm
library(MASS) # for rlm
library(cluster)
library(mclust)

dat <- read.table("/tmp/commits.txt", header=TRUE, sep="\t")
dat <- dat[c,("ChangedFiles", "DiffSize", "CmtMsgBytes", "CmtMsgLines",
              "NumSignedOffs", "NumTags", "TotalSubsys", "inRC", "Subsys",
              "AuthorSubsysSimilarity", "AuthorTaggersSimilarity",
              "TaggersSubsysSimilarity")]
# NOTE: The linear relation between CmtMsgBytes and CmtMsgLines
# is obvious, but could be used in the report to explain
# the reasoning behind the graph.
spm(dat, main="Commit Overview", data=dat)

# The diagrams are much better when log scale is used
# (TODO: Possibly adapt the graph properties, not the data)
# The size of a diff and the number of changed files can be 0, which
# is obviously problematic for the logarithm. Replace these cases
# with NA
dat$ChangedFiles <- log(dat$ChangedFiles)
dat$DiffSize <- log(dat$DiffSize)
dat$DiffSize[dat$DiffSize==-Inf]=0 #NA
dat$ChangedFiles[dat$ChangedFiles==-Inf]=0 #NA
dat$CmtMsgBytes <- log(dat$CmtMsgBytes)
dat$TotalSubsys <- log(dat$TotalSubsys)
dat$inRC <- as.factor(dat$inRC)
#dat$CmtMsgLines <- log(dat$CmtMsgLines)

# TODO: Use jitter.data and alpha in the panel display function
# to deal with overplotting. 
trellis.device("quartz") # for MacOS. Does not seem to work really reliably...
trellis.device("x11")
splom(~data.frame(ChangedFiles,DiffSize, CmtMsgBytes,NumSignedOffs,
                  TotalSubsys), groups=inRC, jitter.data=TRUE, alpha=0.7,
      data=dat, main="Commit Overview", pch="o", cex=0.5,
      key=list(title="RC phase", text=list(c("No", "Yes")),
        points=list(pch="o", cex=1.5)))


splom(~data.frame(ChangedFiles,DiffSize, CmtMsgBytes,NumSignedOffs,
                  TotalSubsys) | Subsys, jitter.x=TRUE, jitter.y=TRUE,
      data=dat, main="Commit Overview", pch=".", cex=0.5)

spm(~ChangedFiles+DiffSize+CmtMsgBytes+NumTags+NumSignedOffs+TotalSubsys|inRC,
    data=dat, main="Commit Overview", reg.line=lm, cex=0.3,
    ellipse=FALSE)

# TODO: Perform the above plots conditioned on subsystem
# and on rc/non-rc. Also
# about other suitable conditioning factors
spm(~DiffSize+NumSignedOffs+ChangedFiles+CmtMsgBytes+TotalSubsys+Subsys|inRC,
    data=dat, main="Does RC influence commits?", reg.line=FALSE, cex=0.5,
   ellipse=FALSE)

spm(~NumSignedOffs+DiffSize+ChangedFiles+inRC|TotalSubsys,
    data=dat, main="TotalSubsys", reg.line=lm, cex=0.5,
    ellipse=FALSE)

spm(~DiffSize+NumSignedOffs+ChangedFiles+CmtMsgBytes+inRC+TotalSubsys|Subsys,
    data=dat, main="Does RC influence commits?", reg.line=lm, cex=0.5,
    ellipse=FALSE)

# Use some random sampling to deal with the overplotting (using
# transparency would be better, though)
set.seed(42)
samples = runif(2000, min=1, max=dim(dat)[1])
dat.sampled <- dat[samples,]
spm(dat.sampled, main="Commit Overview", data=dat, pch=".", cex=3)
spm(~TaggersSubsysSimilarity + AuthorSubsysSimilarity +
    AuthorTaggersSimilarity | Subsys, main="Commit Overview",
    data=dat.sampled, cex=0.5)


# NOTE: Mention in the report that performing exploratory statistics
# is also a way to accept refute of subjective assumptions that are
# typically made about (kernel) development, for instance that
# commits receive a more thorough treatment in the RC cycle.
# NOTE: Another jusitification for this work is that understanding
# development and estimating the value of commits is a general software
# architecture/engineering problem. Since the Linux kernel is particularly
# well known, it serves as apt data basis. Additionally, it would
# be interesting to see differences between different projects.

# TODO: Use mosaic plots to see if there is any structure in the factors
# (and also exploit contingency plots for the factor contributions,
# esp. the subsystems)
# TODO: Introduce per-subsystem growth plots. Not that we've not had them yet.

dat2 <- scale(dat)
dat2.sampled <- scale(dat.sampled[,1:7])
spm(~DiffSize+NumSignedOffs+ChangedFiles|inRC,
    data=dat2, main="Does RC influence commits?",
    ellipse=FALSE)

# Runs for ages for dat2, so use dat2.sampled
library(mclust)
fit <- Mclust(dat2.sampled)
# TODO: Interpret whatever this does tell us...
plot(fit, dat2.sampled)


# Paritioning: Use, for example, inRC as outcome, and see how the
# available predictors are selected. From this, infer if there is
# any (for us) significant difference between the patches.
library(rpart)
# Interestingly, DiffSize is the only covariate that matters when
# the unsampled data are used. With the sampled data, more
# covariated (DiffSize, CmtMsgBytes and NumTags -- similar to the
# party result) matter.
# NOTE: The result strongly depends on the random sampling. This
# must not be the case, naturally!!!
fit <- rpart(inRC ~ DiffSize + CmtMsgBytes + NumTags + NumSignedOffs +
             AuthorSubsysSimilarity + AuthorTaggersSimilarity +
             TaggersSubsysSimilarity, 
             method="anova", data=dat.sampled)
printcp(fit)
plot(fit); text(fit)

# No reasonable partitioning an be found (only the root node remains)
fit <- rpart(NumSignedOffs ~ inRC + CmtMsgBytes + TotalSubsys,
             method="anova", data=dat)
printcp(fit)

# Partitioning/Classification with random forests (NOTE: Also takes
# too long with the unsampled data)
library(randomForest)
# TODO: WTF does this use regression and not classification when dat
# is used? (it uses classification with dat.sampled)? The
# response _is_ a factor.

# For 2000 Samples, this shows a quite distinct shapes
# that should be better understood (it's also fairly
# reproducible regarding the random samples)
fit <- randomForest(inRC~., data=dat.sampled, importance=TRUE, proximity=TRUE)
fit <- randomForest(Subsys~., data=dat.sampled, importance=TRUE, proximity=TRUE)
fit <- randomForest(DiffSize~., data=dat.sampled, importance=TRUE, proximity=TRUE)
print(fit)
# TODO: Interpret whatever this tells us
plot(fit)
MDSplot(fit, dat$inRC)
MDSplot(fit, dat$Subsys)

# Conditional inference tree
library(party)
# Using Subsys as covariate lets things flow over top. inRC is much better
fit <- ctree(Subsys ~ DiffSize + CmtMsgBytes + NumTags + NumSignedOffs +
             AuthorSubsysSimilarity + AuthorTaggersSimilarity +
             TaggersSubsysSimilarity, data=dat.sampled)
print(fit)
# NOTE: Cool. This at least includes CmtMsgBytes and NumSignedOffs into
# the distinction.
plot(fit)

# Principal components analysis
# TODO: Why does this not work with factors?
fit <- princomp(dat[,1:7], cor=TRUE)
biplot(fit)
plot(fit,type="lines")

# Clutering. As usual, does not work with factors.
fit <- kmeans(dat[,1:7], 3)
clusplot(dat[1:7], fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


# TODO: Partition the (quasi)-continuous quantities into discrete
# factors to enable the methods of correspondence analysis etc. to work.

# Correlations
library(corrgram)
# It's interesting that there are no correlations (beyond the obvious
# ones) in the data
corrgram(dat[,c(1:7,10:12)], order=TRUE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  diag.panel=panel.minmax,
  main="Correlations in the kernel data") 


# Subsystem classification via gam
library(mgcv)
# TODO: This does not work, but something like this is the way to go.
gam(Subsys~ChangedFiles + s(DiffSize) + s(CmtMsgBytes) +
              NumSignedOffs + NumTags + TotalSubsys + inRC +
              s(AuthorSubsysSimilarity) + s(AuthorTaggersSimilarity) +
              s(TaggersSubsysSimilarity), data=dat)
