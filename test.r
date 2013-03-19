## TODO: Create a class that carries all intermediate results so
## that they can easily be passed to project-specific post-processing
## functions
## TODO: We could use the same mining techniques on the repository:
## Check which authors touched whose code (to get author-author relationships),
## and use the commit messages to get textual information
## Combine both pieces as in this analysis
## TODO: Analyse fixed time batches of mailing lists (for instance, over one
## month) to generate time series from the results computed for each
## batch -- this will give some insights how a project developed over time
## Question is: Should we split by time, or should we split by a fixed
## number of messages? (e.g, using tm::makeChunk)
## TODO: When plots with stemmed words are displayed, we should use
## tm::stemCompletion to (try to) provide complete words in the graph
doCompute <- FALSE # TRUE # If set to FALSE, load computed results from cached files
analysis.base <- "/home/wolfgang/projects/swi/"
data.path.base <- "/home/wolfgang/projects/swi/ml_results"

project <- "git"
data.prefix="4weekly"
i <- 2

## When the analysis uses the standard directory layout, these
## definitions are not supposed to be changed
repo.path <- paste(analysis.base, "nntp/repos/", sep="/")
snatm.path <- paste(analysis.base, "src/snatm/pkg/R", sep="")
source("includes.r")

data.path <- file.path(data.path.base, project, "ml", paste(data.prefix, i, sep="."))

## Read in res (as saved by dispatch.steps)
load(file=file.path(data.path, "vis.data"))

## Read in forest.corp{,.sub} (as saved by dispatch.sub.sequences)
load(file=file.path(data.path, "forest.corp"))
forest.corp <- forest.corp.sub
forest <- forest.corp$forest
corp <- forest.corp$corp
rm(forest.corp.sub)


# ################# TODO: Experimental stuff #####################
# #################### Classical text mining analysis ##################
findAssocs(res$doc.matrices$tdm, "submodul", 0.75)
findAssocs(res$doc.matrices$tdm, "llvm", 0.6)
# TODO: Explore further standard methods described in the tm manual

## ################# Classification ##################
## TODO: Classify the messages into interesting categories. This is
## very important -- for instance, it could be used to automatically
## filter out security relevant topics (and then track their progress
## into the repository)
## The tm package has some standard means for doing this, check if the
## methods deliver satisfactory results

## ######## Exploratory/descriptive statistics for the data ###############
## Compute some exploratory statistics on the data
## NOTE: Some of these are also approximately handled by generic tm methods,
## but the implementation here is more accurate

authors.per.thread <- function(i) {
  length(unique(forest[forest[,"threadID"]==i, "author"]))
}

messages.per.thread <- function(i) {
  length(forest[forest[,"threadID"]==i, "subject"])
}

## Determine authors and messages _per thread_
num.authors <- sapply(unique(forest[,"threadID"]), authors.per.thread)
num.messages <- sapply(unique(forest[,"threadID"]), messages.per.thread)
thread.info <- data.frame(authors=num.authors, messages=num.messages,
                          tid=attr(num.messages, "names"))

## Are there some focused discussions, or do changes 
## TODO: Add a 45 degree slope line or a linear regression
ggplot(thread.info, aes(x=authors, y=messages)) + geom_point()

thread.info.molten <- melt(thread.info)
ggplot(thread.info.molten, aes(x=variable, y=value)) + geom_boxplot() +
  scale_y_log10()

## Generate time series (authors active per time, messages per time,
## durtation of discussions, TODO: what else)


## Suboptimal (but we need a direct visualisation of the densities, not just
## the ecdf)
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

## How does the number of authors scale with the number of messages per thread?
## TODO: Also not really optimal. The ecdfs look way to similar for fairly different
## mailing lists.
ggplot(data.frame(x=num.messages, y=num.authors), aes(x=x, y=y)) + geom_point() +
  xlab("Number of messages") + ylab("Number of authors") + geom_smooth()

## NOTE/TODO: It should be possible to detect topics/threads of large
## relevance/interest using a weighted selection by number of authors and
## number of messages.
get.subject <- function(i) {
  as.character(forest[forest[,"threadID"]==i, "subject"][1])
}
get.authors <- function(threadID) {
  unique(forest[forest[,"threadID"]==threadID, "author"])
}
## Select the largest threads (measured by the number of messages in the
## threads)
largest.threads <- as.integer(names(sort(num.messages, decreasing=T)[1:20]))
## ... and determine the subjects that started the threads
sapply(largest.threads, get.subject)

dat.msg <- data.frame(tid=1:length(largest.threads),
                      num.messages=sapply(largest.threads, function(x) length(get.subject(x))))

ggplot(dat.msg[which(dat.msg$num.authors > 3),], aes(x=num.messages)) + geom_density() +
  xlab("Messages per thread") + ylab("Density")

## Same thing in blue, this time with the number of contributing authors
## as size measure
## TODO: Automatically select the number of threads under consideration
largest.threads <- as.integer(names(sort(num.authors, decreasing=T)[1:250]))
sapply(largest.threads, get.subject)

dat.msg <- data.frame(tid=1:length(num.authors),
                      num.authors=sort(num.authors, decreasing=T))
ggplot(dat.msg[1:15,], aes(x=tid, y=num.authors)) + geom_bar(stat="identity") +
       xlab("Thread (arbitrary ID)") + ylab("Authors in thread")
## TODO: Most threads do only have one message. This seems curious to me. Check.

## Looks like an exponential decay would describe this very well, at least for LKML
ggplot(dat.msg[which(dat.msg$num.authors > 1),], aes(x=num.authors)) +
       geom_histogram(binwidth=1) + scale_y_log10()
  xlab("Authors per thread") + ylab("Density")

## Determine all authors who contributed to the largest threads
## (interestingly, there does not seem to be too much overlap for the linux
## kernel: 162 authors with repetition, 128 unique authors.
## Linux: 0.79, Freebsd: 0.69 (-> less overlap))
authlist <- unlist(sapply(largest.threads, get.authors))
length(unique(authlist))/length(authlist)
unique(
       unlist(sapply(largest.threads, get.authors))
       )

## Find the most active authors (i.e., loudmouths ;)) in the complete archive
## (taken from the tm docs)
authors <- lapply(corp, Author)
authors <- sapply(authors, paste, collapse = " ")
sort(table(authors), decreasing = TRUE)[1:50]

## TODO: Export as table and plot distribution

## TODO: Repeat this calculation for the different time periods, and then
## compute the intersections of the author names with a sliding scale to see
## how stable the number of central developers is.

## Same in blue for the headings (TODO: Check if this delivers the same
## results as the thread analysis)
## NOTE: The initiating message is not counted. Messages with subjects
## along the line "Re: Re: subject", "AW: Re: AW: subject" will also
## not be counted to a thread. The results are therefore only approximative
headings <- lapply(corp, Heading)
headings <- sapply(headings, paste, collapse = " ")
bigTopicsTable <- sort(table(headings), decreasing = TRUE)[1:10]
names(bigTopicsTable)
## TODO: This delivers a different result than our two custom methods.
## Check how to use tm to determine importance of a thread.


## ####### Descriptive statistics ###########
#- Lengths of threads (# of messages, #of days, #of authors, everything time resolved)
##- mails per day, time-resolved
                       
                      
#################### Keyword extraction ######################
library(RKEA)
tmpdir <- tempfile()
dir.create(tmpdir)
model <- file.path(tmpdir, "testModel")
## TODO: How the heck is the indexing supposed to work if there is
## no pre-specified model? Should be possible as the the KEA documentation,
## but the question is how this works with R
createModel(corp, c(), model)

## TODO: package wordcloud could come handy when we need to visualise our results


## ############ Experimental section ###########
## NOTE: For VCorpus objects, evaluating object[n] will trigger the print
## method (which just states that the object is a corpus with a single
## document), whereas the document itself can be retrieved via object[[n]]

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

## TODO: Analyse in two sequence modes: Weekly and four-weekly (or monthly?).
## Align at the start of a week (aligning at the start of a month would likely
## not bring any added benefits)
