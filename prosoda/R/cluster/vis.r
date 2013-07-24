## This file is part of prosoda.  prosoda is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2010, 2011 by Wolfgang Mauerer <wm@linux-kernel.net>
## Copyright 2012, 2013, Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Some snippets for visualising the data obtained from cluster.r
s <- suppressPackageStartupMessages
s(library(shiny))
s(library(igraph))
s(library(logging))
s(library(corrgram))
s(library(ggplot2))
s(library(lattice))
s(library(car))  ## for spm
s(library(MASS)) ## for rlm
s(library(cluster))
s(library(rpart))
s(library(randomForest))
s(library(party)) # ctree
rm(s)
source("../config.r", chdir=TRUE)
source("../db.r", chdir=TRUE)
source("../utils.r", chdir=TRUE)
source("../query.r", chdir=TRUE)
source("../commits.r", chdir=TRUE)

## Global variables
conf <- load.global.config("prosoda.conf")
conf <- init.db.global(conf)
projects.list <- query.projects(conf$con)

pid <- projects.list$id[[which(projects.list$name=="linux")]]
range.ids.list <- query.range.ids.con(conf$con, pid)

################################## Experiments #############################

## TODO: Perform the above plots conditioned on subsystem
## and on rc/non-rc. Also
## about other suitable conditioning factors
spm(~LogDiffSize+NumSignedOffs+ChangedFiles+LogCmtMsgBytes|inRC,
    data=cmt.info, main="Does RC influence commits?", reg.line=FALSE, cex=0.5,
   ellipse=FALSE)

## NOTE: Mention in the report that performing exploratory statistics
## is also a way to accept refute of subjective assumptions that are
## typically made about (kernel) development, for instance that
## commits receive a more thorough treatment in the RC cycle.
## NOTE: Another jusitification for this work is that understanding
## development and estimating the value of commits is a general software
## architecture/engineering problem. Since the Linux kernel is particularly
## well known, it serves as apt data basis. Additionally, it would
## be interesting to see differences between different projects.

## TODO: Use mosaic plots to see if there is any structure in the factors
## (and also exploit contingency plots for the factor contributions,
## esp. the subsystems)

## Paritioning: Use, for example, inRC as outcome, and see how the
## available predictors are selected. From this, infer if there is
## any (for us) significant difference between the patches.
fit <- rpart(inRC ~ ., method="anova", data=cmt.info.scaled)
printcp(fit)
plotcp(fit)
plot(fit); text(fit, use.n=TRUE, all=TRUE, cex=.8)

library(tree)
fit <- tree(inRC ~ ., data=cmt.info)
summary(fit)

## Partitioning/Classification with random forests (NOTE: Also takes
## too long with the unsampled data)
## TODO: WTF does this use regression and not classification when dat
## is used? (it uses classification with dat.sampled)? The
## response _is_ a factor.

fit <- randomForest(inRC~., data=na.omit(cmt.info), importance=TRUE, proximity=TRUE)
print(fit)
## TODO: Interpret whatever this tells us
plot(fit)
## TODO: Is there a separation between RC and non-RC commits for a project?
## This would be an indicator that the engineering is good because there is
## a separation (does not seem to be so for qemu and linux, at least)
## Careful: This can take quite a while to compute.
MDSplot(fit, cmt.info$inRC)

## Conditional inference tree.
## NOTE: This does not at all work well for the Linux kernel and qemu.
fit <- ctree(inRC ~ DiffSize + CmtMsgBytes + NumTags + NumSignedOffs + ChangedFiles,
             data=cmt.info)
fit <- ctree(inRC ~ ., data=cmt.info.scaled)
table(predict(fit, type="response", cmt.info.scaled), cmt.info.scaled$inRC)
print(fit)
plot(fit)


## TODO: Partition the (quasi)-continuous quantities into discrete
## factors to enable the methods of correspondence analysis etc. to work.

## Subsystem classification via gam
library(mgcv)
## TODO: This does not work, but something like this is the way to go.
gam(inRC~ChangedFiles + s(DiffSize) + s(CmtMsgBytes) +
              NumSignedOffs + NumTags, data=na.omit(cmt.info))

## Logistic regression to model the categorical covariable
fit <- glm(inRC~., data=na.omit(cmt.info), family=binomial)
## TODO: Can we use this result as a predictor?
