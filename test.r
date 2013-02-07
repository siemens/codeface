# TODO: Create a class that carries all intermediate results so
# that they can easily be passed to project-specific post-processing
# functions
# TODO: We could use the same mining techniques on the repository:
# Check which authors touched whose code (to get author-author relationships),
# and use the commit messages to get textual information
# Combine both pieces as in this analysis
# TODO: Analyse fixed time batches of mailing lists (for instance, over one
# month) to generate time series from the results computed for each
# batch -- this will give some insights how a project developed over time
# Question is: Should we split by time, or should we split by a fixed
# number of messages? (e.g, using tm::makeChunk)
# TODO: When plots with stemmed words are displayed, we should use
# tm::stemCompletion to (try to) provide complete words in the graph
# TODO: Use the MPI facilities provided by tm: tm_startCluster(), tm_endCluster()
doCompute <- TRUE # If set to F, load computed results from cached files
library(tm)
library(tm.plugin.mail)
library(sna)
#library(wordnet)
library(ggplot2)
library(igraph)
library(lsa)
library(Rgraphviz) # See www.bioconductor.org/packages/2.9/bioc/html/Rgraphviz.html
library(lubridate)
library(multicore)

source("local.r")
source("analysis.r")
source("project.spec.r")
source("keyword.list.r")
# Load the contents of snatm manually -- things are still quite in flux
snatm.path <- "/home/wolfgang/projects/zk/QuantArch/src/snatm/pkg/R"
sourceDir(snatm.path)
# Wordnet database can be downloaded from
# http://wordnet.princeton.edu/wordnet/download/current-version/
#setDict("/home/wolfgang/projects/zk/QuantArch/wordnet/dict")

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

# TODO: The analysis (sometimes...) takes an order of magnitude longer with
# UTF-8 vs. latin1?! Could be the stemming, but check with Rprof.
date()
options(error=recover)
dispatch.all(ml, repo.path, data.path, doCompute=TRUE)
date()

quit()
## TODO: Sometimes, the analysis runs for aaaaaaages. In the next run
## (after a restart of R, so in a fresh environment), everything is fine?! -> Why?!
## NOTE: It occurs especially overnight that the computation seems to freeze
## at some point (when run from ESS, batch mode needs to be checked). Seems like
## something is waiting for something, perhaps the RNG generator.
## TODO: This would be a good candidate for cluster-level parallelism

for (ml in c("gmane.comp.compilers.clang.devel",
             "gmane.comp.lang.d.general",
             "gmane.comp.version-control.git", "gmane.linux.kernel",
             "gmane.os.freebsd.architechture", "gmane.os.openbsd.tech")) {
  cat ("Analysing ", ml, ". Start: ", date(), "\n", sep="")
  t <- Sys.time()
  dispatch.all(ml, repo.path, data.path, doCompute=TRUE)
  cat ("Finished: ", date(), "\n", sep="")
  cat ("Duration: ", Sys.time() - t, "\n")
}

################## TODO: Experimental stuff #####################
##################### Classical text mining analysis ##################
findAssocs(doc.matrices$tdm, "submodul", 0.75)
findAssocs(doc.matrices$tdm, "llvm", 0.6)
# TODO: Explore further standard methods described in the tm manual

################### Classification ##################
# TODO: Classify the messages into interesting categories. This is
# very important -- for instance, it could be used to automatically
# filter out security relevant topics (and then track their progress
# to the repository)
# The tm package has some standard means for doing this, check if the
# methods deliver satisfactory results

########## Exploratory/descriptive statistics for the data ###############
# Compute some exploratory statistics on the data
# TODO: See how far this is already handled by generic methods
# provided by the tm package
forest <- forest.corp$forest
authors.per.thread <- function(i) {
  length(unique(forest[forest[,"threadID"]==i, "author"]))
}

messages.per.thread <- function(i) {
  length(forest[forest[,"threadID"]==i, "subject"])
}

num.authors <- sapply(unique(forest[,"threadID"]), authors.per.thread)
num.messages <- sapply(unique(forest[,"threadID"]), messages.per.thread)

# Suboptimal (but we need a direct visualisation of the densities, not just
# the ecdf)
ggplot(data.frame(x=num.messages), aes(x=x)) + 
  geom_density(aes(y=..density..)) + scale_y_log10()

gen.ecdf <- function(dat, normalise=T) {
  .ecdf <- data.frame(x=sort(unique(dat)), y=ecdf(dat)(sort(unique(dat))))

  if (!normalise) {
    .ecdf.y <- .ecdf.y * length(dat)
  }

  return(.ecdf)
}

dat <- cbind(gen.ecdf(num.authors), type="Authors")
dat <- rbind(dat, cbind(gen.ecdf(num.messages), type="Messages"))
ggplot(data=dat, aes(x=x, y=y)) + geom_step() +
  facet_wrap(~type, scales="free_x", ncol=1) +
  xlab("Number of authors/messages per thread") +
  ylab("ecdf")

# How does the number of authors scale with the number of messages per thread?
ggplot(data.frame(x=num.messages, y=num.authors), aes(x=x, y=y)) + geom_point() +
  xlab("Number of messages") + ylab("Number of authors") + geom_smooth()

# NOTE/TODO: It should be possible to detect topics/threads of large
# relevance/interest using a weighted selection by number of authors and
# number of messages.
get.subject <- function(i) {
  as.character(forest[forest[,"threadID"]==i, "subject"][1])
}
get.authors <- function(threadID) {
  unique(forest[forest[,"threadID"]==threadID, "author"])
}
# Select the 10 largest threads (measured by the number of messages in the
# threads)
largest.threads <- as.integer(names(sort(num.messages, decreasing=T)[1:10]))
# ... and determine the subjects that started the threads
sapply(largest.threads, get.subject)

# Same thing in blue, this time with the number of contributing authors
# as size measure
largest.threads <- as.integer(names(sort(num.authors, decreasing=T)[1:10]))
sapply(largest.threads, get.subject)
get.authors(largest.threads[2])

# Determine all authors who contributed to the largest threads
# (interestingly, there does not seem to be too much overlap for the linux
# kernel: 162 authors with repetition, 128 unique authors.
# Linux: 0.79, Freebsd: 0.69 (-> less overlap))
authlist <- unlist(sapply(largest.threads, get.authors))
length(unique(authlist))/length(authlist)
unique(
       unlist(sapply(largest.threads, get.authors))
       )

# Find the most active authors (i.e., loudmouths ;)) in the complete archive
# (taken from the tm docs)
authors <- lapply(corp, Author)
authors <- sapply(authors, paste, collapse = " ")
sort(table(authors), decreasing = TRUE)[1:10]

# Same in blue for the headings (TODO: Check if this delivers the same
# results as the thread analysis)
# NOTE: The initiating message is not counted. Messages with subjects
# along the line "Re: Re: subject", "AW: Re: AW: subject" will also
# not be counted to a thread. The results are therefore only approximative
headings <- lapply(corp, Heading)
headings <- sapply(headings, paste, collapse = " ")
bigTopicsTable <- sort(table(headings), decreasing = TRUE)[1:10]
names(bigTopicsTable)
# TODO: This delivers a different result than our two custom methods.
# Check how to use tm to determine importance of a thread.


######### Descriptive statistics ###########
#- Lengths of threads (# of messages, #of days, #of authors, everything time resolved)
#- mails per day, time-resolved
                       
                      
#################### Keyword extraction ######################
library(RKEA)
tmpdir <- tempfile()
dir.create(tmpdir)
model <- file.path(tmpdir, "testModel")
# TODO: How the heck is the indexing supposed to work if there is
# no pre-specified model? Should be possible as the the KEA documentation,
# but the question is how this works with R
createModel(corp, c(), model)

# TODO: package wordcloud could come handy when we need to visualise our results


############## Experimental section ###########
# NOTE: For VCorpus objects, object[n] will trigger the print method (which just states
# that the object is a corpus with a single document), whereas the document
# itself can be retrieved via object[[n]]

library(openNLP)
doc <- corp[[10160]]
tm_map(doc, tmSentDetect)
sentDetect(Content(doc))

## Some date operations
snatm.path <- "/home/wolfgang/projects/zk/QuantArch/src/snatm/pkg/R"
tm.path <- "/home/wolfgang/projects/zk/QuantArch/src/tm/pkg/R"
source("local.r")
sourceDir(snatm.path)
sourceDir(tm.path)
library(tm.plugin.mail)
corp <- tm::Corpus(
         DirSource("/home/wolfgang/projects/zk/QuantArch/nntp/repos/gmane.os.openbsd.tech",
                   encoding="UTF-8"),
                   readerControl =
                   list(reader=readMail(DateFormat = "%a, %d %b %Y %H:%M:%S")))

corp <- tm::Corpus(
         DirSource("/home/wolfgang/projects/zk/QuantArch/nntp/repos/gmane.comp.compilers.clang.devel",
                   encoding="UTF-8"),
                   readerControl =
                   list(reader=readMail(DateFormat = "%a, %d %b %Y %H:%M:%S")))

forest.corp <- gen.forest(ml, repo.path, data.path, doCompute=FALSE)
dates <- do.call(c,
                 lapply(seq_along(forest.corp$corp),
                        function(i) as.POSIXct(DateTimeStamp(forest.corp$corp[[i]])))
                 )

gen.aligned.range <- function(dates, interval="week") {
  dates <- dates[!is.na(dates)]
  t.start <- ceiling_date(min(dates), interval)
  t.end <- floor_date(max(dates), interval)
  
  return(range(t.start, t.end))
}

num.weeks <- new_interval(gen.aligned.range(dates, "week")[[1]],
                          gen.aligned.range(dates, "week")[[2]])

max(dates.cleaned)-min(dates.cleaned)
range(dates[which(dates >= m.start & dates < m.end)])

# TODO: Analyse in two sequence modes: Weekly and four-weekly (or monthly?).
# Align at the start of a week (aligning at the start of a month would likely
# not bring any added benefits)
