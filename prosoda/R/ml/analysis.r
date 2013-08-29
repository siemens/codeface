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
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(lubridate))
source("../db.r", chdir=TRUE)
source("../utils.r", chdir=TRUE)
source("../query.r", chdir=TRUE)
source("../id_manager.r", chdir=TRUE)

gen.forest <- function(conf, repo.path, resdir) {
  ## TODO: Use apt ML specific preprocessing functions, not always the
  ## lkml variant
  corp.file <- file.path(resdir, paste("corp.base", conf$listname, sep="."))
  doCompute <- !(file.exists(corp.file))

  if (doCompute) {
    corp.base <- gen.corpus(conf$listname, repo.path, suffix=".mbox",
                            marks=c("^_{10,}", "^-{10,}", "^[*]{10,},",
                                   # Also remove inline diffs. TODO: Better
                                   # heuristics for non-git projects
                                   "^diff --git", "^@@",
                                   "^The following changes since commit",
                                   # Generic majordomo? (note that "--" is
                                   # often used instead of "-- "
                                   "^--$", "^---$", "^To unsubscribe",
                                   # The following are specific for Linux kernel
                                   # style projects
                                   "^Signed-off-by", "^Acked-by", "CC:"),
                              encoding="UTF-8",
                              preprocess=linux.kernel.preprocess)
    save(file=corp.file, corp.base)
  } else {
    load(file=corp.file)
  }

  return(corp.base)
}


compute.doc.matrices <- function(forest.corp, data.path) {
  ## TODO: Should we set the minimal wordlength to something larger than 3?
  ## (see ?termFreq for possible options)
  tdm.file <- file.path(data.path, "tdm")
  dtm.file <- file.path(data.path, "dtm")

  doCompute <- !(file.exists(tdm.file)) && !(file.exists(dtm.file))

  if (doCompute) {
    ## TODO: Any arguments against creating dtm as transpose of tdm?
    tdm <- TermDocumentMatrix(forest.corp$corp,
                              list(stemming = FALSE, stopwords = FALSE))
    dtm <- DocumentTermMatrix(forest.corp$corp,
                              list(stemming = FALSE, stopwords = FALSE))
    ## Oookay, this is just for the linguistically curious: Check how well
    ## Zipf's and Heap's law are fulfilled.
###Zipf_plot(dtm)
###Heaps_plot(dtm)
###plot(tdm) # Pointless without a restriction to frequent keywords

    ## NOTE: Computing the dissimilarity matrix is computationally expensive
###diss <- dissimilarity(tdm, method = "cosine")
####test.hclust <- hclust(diss)
    ## TODO: Once we have the dissimilarity matrix, we can apply all the
    ## standard clustering techniques. The results will need to be interpreted,
    ## though. For instance, use hclust. Albeit this is fairly pointless by
    ## now -- we need to find some criteria to cluster for.

    save(file=tdm.file, tdm)
    save(file=dtm.file, dtm)
    ##save(file=file.path(data.path, "diss"), diss)
  } else {
    load(file=tdm.file)
    load(file=dtm.file)
    ##load(file=file.path(data.path, "diss")
  }

  return(list(tdm=tdm, dtm=dtm))
}


compute.commnet <- function(forest.corp, data.path) {
  commnet.file <- file.path(data.path, "commnet")
  doCompute <- !(file.exists(commnet.file))

  if (doCompute) {
    commnet <- adjacency(createedges(forest.corp$forest))
    save(file=commnet.file, commnet)
  } else {
    load(file=commnet.file)
  }

  return(commnet)
}


## These two calls are only relevant for the side effects -- they populate
## basedir/<ml>/subject resp. /content
## Iterate over all terms in termfreq, and create the adjacency matrix
## for the communication network associated with each term
extract.commnets <- function(forest.corp, termfreq, repo.path, data.path) {
  cont.dir <- file.path(data.path, "commnet.terms", "content")
  subj.dir <- file.path(data.path, "commnet.terms", "subject")

  doCompute <- !(file.exists(cont.dir)) && !(file.exists(subj.dir))

  if (doCompute) {
    extract.commnet(forest.corp, termfreq, "content", data.path)
    extract.commnet(forest.corp, termfreq, "subject", data.path)
  }
}


compute.interest.networks <- function(termfreq, NUM.NET.SUBJECT, NUM.NET.CONTENT,
                                      data.path) {
  subject.file <- file.path(data.path, "net.subject")
  content.file <- file.path(data.path, "net.content")

  doCompute <- !(file.exists(subject.file)) && !(file.exists(content.file))

  if (doCompute) {
    net.subject <- gen.net("subject", termfreq, data.path, NUM.NET.SUBJECT)
    net.content <- gen.net("content", termfreq, data.path, NUM.NET.CONTENT)
    save(file=subject.file, net.subject)
    save(file=content.file, net.content)
  } else {
    load(file=subject.file)
    load(file=content.file)
  }

  return(list(subject=net.subject, content=net.content))
}


analyse.networks <- function(forest, interest.networks, communication.network) {
  ######### Analyse interest and communication (ICC) networks #######
  ## (very fast, no persistent storing necessary)
  networks.subj <- gen.combined.network(interest.networks$subject,
                                        communication.network)
  networks.cont <- gen.combined.network(interest.networks$content,
                                        communication.network)
  dat.subj <- gen.networks.df(networks.subj)
  dat.cont <- gen.networks.df(networks.cont)

  dat.icc <- data.frame(dat.subj, source="subject")
  dat.icc <- rbind(dat.icc, data.frame(dat.cont, source="content"))
  rm(dat.subj); rm(dat.cont)

  ####### Initiation-response (IR) structure for the mailing list ######
  ## TODO: Determine if any extremal values are outliers
  ## (this plot seems to be quite informative. Compare for multiple projects)
  dat.subj <- compute.initiate.respond(forest, networks.subj$communication,
                                       networks.subj$centrality)
  dat.cont <- compute.initiate.respond(forest, networks.cont$communication,
                                       networks.cont$centrality)
  dat.ir <- data.frame(dat.subj, source="subject")
  dat.ir <- rbind(dat.ir, data.frame(dat.cont, source="content"))
  rm(dat.subj); rm(dat.cont)

  return(list(icc=dat.icc, ir=dat.ir))
}


## ################### Analysis dispatcher ######################
## ################### Let the above rip ########################

dispatch.all <- function(conf, repo.path, resdir) {
  loginfo("Starting mailinglist analysis", logger="ml.analysis")
  corp.base <- gen.forest(conf, repo.path, resdir)
  loginfo("corp.base finished", logger="ml.analysis")
  ## TODO: When we consider incremental updates, would it make sense
  ## to just update the corpus, and let all other operations run
  ## from scratch then? This would likely be the technically easiest
  ## solution..

  ## #######
  ## Split the data into smaller chunks for time-resolved analysis
  ## Get all message timestamps, and clean the invalid ones
  dates <- do.call(c,
                   lapply(seq_along(corp.base$corp),
                          function(i) as.POSIXct(DateTimeStamp(corp.base$corp[[i]])))
                   )
  ## Sort the dates to enable faster algorithms. This also removes NAs
  dates <- sort(dates)

  ## Select weekly and monthly intervals (TODO: With the new flexible
  ## intervals in place, we could select proper monthly intervals)
  iter.weekly <- gen.iter.intervals(dates, 1)
  iter.4weekly <- gen.iter.intervals(dates, 4)

  ## Compute a list of intervals for the project release cycles
  cycles <- get.cycles(conf)

  ## NOTE: We store the lubridatye intervals in a list (instead of
  ## simply appending them to the cycles data frame) because they
  ## are coerced to numeric by the conversion to a data frame.
  release.intervals <- list(dim(cycles)[1])
  for (i in 1:(dim(cycles)[1])) {
    release.intervals[[i]] <- new_interval(cycles$date.start[[i]],
                                           cycles$date.end[[i]])
  }

  release.labels <- as.list(cycles$cycle)

  ## The mailing list data may not cover the complete timeframe of
  ## the repository, so remove any empty intervals
  nonempty.release.intervals <- get.nonempty.intervals(dates, release.intervals)
  release.intervals <- release.intervals[nonempty.release.intervals]
  release.labels <- release.labels[nonempty.release.intervals]

  if (length(nonempty.release.intervals) == 0) {
    stop("Mailing list does not cover any release range.")
  }

  ## TODO: Find some measure (likely depending on the number of messages per
  ## time) to select suitable time intervals of interest. For many projects,
  ## weekly (and monthly) are much too short, and longer intervals need to
  ## be considered.
  periodic.analysis <- FALSE
  if (periodic.analysis) {
    loginfo("Periodic analysis", logger="ml.analysis")
    analyse.sub.sequences(conf, corp.base, iter.weekly, repo.path, resdir,
                          paste("weekly", 1:length(iter.weekly), sep=""))
    analyse.sub.sequences(conf, corp.base, iter.4weekly, repo.path, resdir,
                          paste("4weekly", 1:length(iter.4weekly), sep=""))
  }

  loginfo("Analysing subsequences", logger="ml.analysis")
  ## Obtain a unique numerical ID for the mailing list
  ml.id <- gen.clear.ml.id.con(conf$con, conf$listname, conf$pid)
  ## Also obtain a clear plot for the mailing list activity
  activity.plot.name <- str_c(conf$listname, " activity")
  activity.plot.id <- get.clear.plot.id(conf, activity.plot.name)
  analyse.sub.sequences(conf, corp.base, release.intervals, repo.path, resdir,
                        release.labels, ml.id, activity.plot.id)

  ## #######
  ## Global analysis
  loginfo("Global analysis", logger="ml.analysis")
  ## NOTE: We only compute the forest for the complete interval to allow for creating
  ## descriptive statistics.
  corp <- corp.base$corp

  ## NOTE: conf must be present in the defining scope
  do.normalise.bound <- function(authors) {
    return(do.normalise(conf, authors))
  }
  forest.corp <- list(forest=make.forest(corp, do.normalise.bound),
                      corp=corp,
                      corp.orig=corp.base$corp.orig)

  resdir.complete <- file.path(resdir, "complete")
  gen.dir(resdir.complete)
  save(file=file.path(resdir.complete, "forest.corp"), forest.corp)
}


analyse.sub.sequences <- function(conf, corp.base, iter, repo.path,
                                  data.path, labels, ml.id, activity.plot.id) {
  if (length(iter) != length(labels))
    stop("Internal error: Iteration sequence and data prefix length must match!")

  timestamps <- do.call(c, lapply(seq_along(corp.base$corp),
                                  function(i) DateTimeStamp(corp.base$corp[[i]])))
  
  loginfo(paste(length(corp.base$corp), "messages in corpus"), logger="ml.analysis")
  loginfo(paste("Date range is", as.character(int_start(iter[[1]])), "to",
      as.character(int_end(iter[[length(iter)]]))), logger="ml.analysis")
  loginfo(paste("=> Analysing ", conf$listname, "in", length(iter), "subsets"),
          logger="ml.analysis")

  ## Prepare a single-parameter version of do.normalise that does
  ## not expose the conf object -- the concept is not known to snatm
  ## NOTE: conf must be present in the defining scope
  do.normalise.bound <- function(authors) {
    return(do.normalise(conf, authors))
  }

  res <- mclapply(1:length(iter), function(i) {
    ## Determine the corpus subset for the interval
    ## under consideration
    loginfo(paste("Processing interval ", i, ": ", labels[[i]]), logger="ml.analysis")

    curr.int <- iter[[i]]
    idx <- which(timestamps >= int_start(curr.int) & timestamps < int_end(curr.int))
    corp.sub <- corp.base$corp[idx]

    forest.corp.sub <- list(forest=make.forest(corp.sub, do.normalise.bound),
                            corp=corp.sub,
                            corp.orig=corp.base$corp.orig[idx])

    ## ... and perform all analysis steps
    data.path.local <- file.path(data.path, labels[[i]])
    gen.dir(data.path.local)
    save(file=file.path(data.path.local, "forest.corp"), forest.corp.sub)

    cycles <- get.cycles(conf)
    dispatch.steps(conf, repo.path, data.path.local, forest.corp.sub,
                   cycles[i,], ml.id, activity.plot.id)
    loginfo(paste(" -> Finished interval ", i, ": ", labels[[i]]), logger="ml.analysis")
  })
  ## Check for errors in mclapply
  for (r in res) {
    if (inherits(r, "try-error")) {
      stop(r)
    }
  }
}

## User needs to make sure that data.path exists and is writeable
## dispatch.steps is called for every time interval that is considered
## in the analysis
dispatch.steps <- function(conf, repo.path, data.path, forest.corp, cycle,
                           ml.id, activity.plot.id) {
  ## TODO: Check how we can speed up prepare.text. And think about if the
  ## function is really neccessary. With stemming activated, I doubt
  ## that it really pays off.
###prep <- prepare.text(forest, progress=TRUE)
####save(file=file.path(data.path, paste("prep", ml, sep=".")), prep)
  communication.network <- compute.commnet(forest.corp, data.path)

  ## Returns tdm and dtm
  doc.matrices <- compute.doc.matrices(forest.corp, data.path)

  ## TODO: Provide per-ml keyword collections for the exclusion words
  termfreq <- find.high.freq(doc.matrices$tdm, exclude.list=unique(c(terms.d,
                                                 terms.coll, terms.c,
                                                 terms.programming)))
  write.table(data.frame(term=as.character(termfreq),
                         count=as.numeric(attr(termfreq, "names"))),
              file=file.path(data.path, "termfreq.txt"), sep="\t",
              row.names=FALSE, quote=FALSE)

  ## NOTE: For most projects, technical left-overs (like footers from majordomo
  ## etc.) will appear in the termfreq list. To find out which elements need
  ## to be removed from emails by grepping for the artefacts, use
  ## id <- function(x) return(x)
  ## text <- sapply(forest.corp$corp[1:5000], id)
  ## grep("keyword", text)
  ## ... and then inspect the appropriate messages in corp.orig to see which additional
  ## filter needs to be applied

  extract.commnets(forest.corp, termfreq, repo.path, data.path)

  ## TODO: Find justifiable heuristics for these configurable parameters
  NUM.NET.SUBJECT <- 25
  NUM.NET.CONTENT <- 50
  interest.networks <- compute.interest.networks(termfreq, NUM.NET.SUBJECT,
                                                 NUM.NET.CONTENT,
                                                 data.path)

  networks.dat <- analyse.networks(forest.corp$forest, interest.networks,
                                   communication.network)


  ## Compute base data for time series analysis
  msgs <- lapply(forest.corp$corp, function(x) { as.POSIXct(DateTimeStamp(x)) })
  msgs <- do.call(c, msgs)

  series <- xts(rep(1,length(msgs)), order.by=msgs)
  series.daily <- apply.daily(series, sum)

  ## ... and store it into the data base, appending it to the activity plot.
  ts.df <- gen.df.from.ts(series.daily, "Mailing list activity")
  dat <- data.frame(time=as.character(ts.df$time),
                    value=ts.df$value,
                    value.scaled=ts.df$value.scaled,
                    plotId=activity.plot.id)

  ## NOTE: We append new values to the existing content. This way,
  ## we can plot arbitrary subsets of the series by selecting
  ## subranges, without the need to concatenate parts together
  res <- dbWriteTable(conf$con, "timeseries", dat, append=TRUE, row.names=FALSE)
  if (!res) {
    stop("Internal error: Could not write timeseries into database!")
  }

  ## Compute descriptive statistics
  ## NOTE: forest needs to available in the defining scope for the
  ## following four helper functions
  ## NOTE: Thread IDs are valid only locally (per release range and project)
  forest <- forest.corp$forest
  authors.per.thread <- function(i) {
    length(unique(forest[forest[,"threadID"]==i, "author"]))
  }
  messages.per.thread <- function(i) {
    length(forest[forest[,"threadID"]==i, "subject"])
  }
  get.subject <- function(i) {
    as.character(forest[forest[,"threadID"]==i, "subject"][1])
  }
  get.authors <- function(threadID) {
    unique(forest[forest[,"threadID"]==threadID, "author"])
  }
  ## Get all email IDs of a thread
  get.emailIDs <- function(threadID) {
    forest[forest[,"threadID"]==threadID, "emailID"]
  }
  ## Get the email ID that started a thread
  get.initial.emailID <- function(threadID) {
    as.numeric(forest[forest[,"threadID"]==threadID, "emailID"][1])
  }
  ## Get the email IDs of all follow-up emails of a thread (list can be empty)
  get.followup.emailIDs <- function(threadID) {
    as.numeric(forest[forest[,"threadID"]==threadID, "emailID"][-1])
  }
  get.timestamp <- function(mailID) {
    DateTimeStamp(forest.corp$corp[[mailID]])
  }

  ## Determine authors and messages _per thread_, plus the thread id
  num.authors <- sapply(unique(forest[,"threadID"]), authors.per.thread)
  num.messages <- sapply(unique(forest[,"threadID"]), messages.per.thread)
  thread.info <- data.frame(authors=num.authors, messages=num.messages,
                            tid=attr(num.messages, "names"))


  ## Infer the larges threads as measured by the number of messages per thread,
  ## and sort thread.info accordingly
  thread.info <- thread.info[order(thread.info$messages, decreasing=TRUE),]

  ## ... and determine the subjects that started the threads, together
  ## with the IDs of the first email in the thread
  subjects.msgs <- sapply(thread.info$tid, get.subject)
  start.email.ids <- sapply(thread.info$tid, get.initial.emailID)
  thread.info <- cbind(thread.info, subject=subjects.msgs,
                       start.email.id=start.email.ids)

  MAX.SUBJECTS <- 200
  ## We store at most 200 subjects. This should be plenty for all
  ## reasonable purposes
  ## TODO: Identical messages appear multiple times in the corpus. This
  ## needs to be fixed. Grepping in the FS revealed that it seems to
  ## be a problem of gmane -- identical messages are already present in
  ## the data source (for instance, 01075 13718 16269 for openssl are identical,
  ## only the time stamps differ slightly)
  if (dim(thread.info)[1] > MAX.SUBJECTS) {
    thread.info.cut <- thread.info[1:200,]
  } else {
    thread.info.cut <- thread.info
  }

  ## freq_subjects stores the subjects that received the highest
  ## attention
  dat <-  data.frame(projectId=conf$pid, mlId=ml.id, releaseRangeId=cycle$range.id,
                     subject=thread.info.cut$subject, count=thread.info.cut$messages)
  res <- dbWriteTable(conf$con, "freq_subjects", dat, append=TRUE, row.names=FALSE)
  if (!res) {
    stop("Internal error: Could not write freq_subjects into database!")
  }

  ## TODO: Can we classify the messages into content catgories, e.g., technical
  ## discussions, assistance (helping users), and code submissions?

  ## Populate the database. First, create a new entry for each thread
  ## TODO: Should we store the raw mails as such from the corpus?
  ## NOTE: Unique in-DB author IDs are already in forest.corp$forest[,"author"].
  ## These can be used to write data into the DB.

  ## forest already provides the in-DB ids for the authors
  ## The list is ordered by local mail id, so we can compute a mapping
  ## between mail IDs and unique author ids. (NOTE: There can be NAs!)
  mailID.to.authorID <- forest.corp$forest[,"author"]
  authorIDs <- as.numeric(mailID.to.authorID[thread.info$start.email.id])

  ## Obtaining the creation dates of thread initiator emails works similarly
  creationDates <- sapply(thread.info$start.email.id,
                          function(mail.id) {
                            return(as.character(get.timestamp(mail.id)))
                          })

  dat <- data.frame(subject=thread.info$subject, createdBy=authorIDs,
                    projectId=conf$pid, releaseRangeId=cycle$range.id,
                    mlId=ml.id, mailThreadId=thread.info$tid,
                    creationDate=creationDates,
                    numberOfAuthors=thread.info$authors,
                    numberOfMessages=thread.info$messages)

  ## Remove tabs in subjects -- dbWriteTable cannot handle this properly
  dat$subject <- as.character(dat$subject)
  dat$subject <- gsub("\t", " ", dat$subject, fixed=TRUE, useBytes=TRUE)

  res <- dbWriteTable(conf$con, "mail_thread", dat, append=TRUE, row.names=FALSE)
  if (!res) {
    stop("Could not add to table mail_thread!")
  }

  ## Compute the two-mode graphs linking users with their interests
  twomode.graphs <- compute.twomode.graphs(conf, interest.networks)

  ## Populate table thread_responses
  ## For each thread, determine the local mail ids of all responses,
  ## find the in-DB IDs for mail and author, and store the information
  ## into the database
  ml.id.map <- query.mlid.map(conf$con, ml.id)

  dat.replies <- lapply(unique(forest[,"threadID"]), function(thread.id) {
    replies <- lapply(get.followup.emailIDs(thread.id), function(mail.id) {
      who <- as.numeric(mailID.to.authorID[mail.id])
      if (!is.na(who)) {
        return(data.frame(who=who,
                          mailThreadID=ml.thread.loc.to.glob(ml.id.map, thread.id),
                          mailDate=as.character(get.timestamp(mail.id))))
      } else {
        return(NULL)
      }
    })

    return(do.call(rbind, replies))
  })

  dat.replies <- do.call(rbind, dat.replies)

  if (!is.null(dat.replies)) {
    res <- dbWriteTable(conf$con, "thread_responses", dat.replies, append=T,
                        row.names=F)
    if (!res) {
      stop("Could not add to table thread_responses!")
    }
  } else {
    logwarn("No responses to add to thread_responses")
  }

  ## TODO: This should be represented by a class
  res <- list(doc.matrices=doc.matrices, termfreq=termfreq,
              interest.networks=interest.networks,
              twomode.graphs=twomode.graphs,
              networks.dat=networks.dat,
              thread.info=thread.info)
  save(file=file.path(data.path, "vis.data"), res)

  ####### End of actual computation. Generate graphs and store data etc. #######
  dispatch.plots(conf, data.path, res)
  store.data(conf, res, cycle$range.id, ml.id)
}

compute.twomode.graphs <- function(conf, interest.networks) {
  ## TODO: Should max.persons be made configurable? Better would
  ## be to find a heuristic to compute a good value
  g.subj <- construct.twomode.graph(interest.networks$subject$edgelist,
                                    interest.networks$subject$adj.matrix,
                                    NA, max.persons=30)
  g.cont <- construct.twomode.graph(interest.networks$content$edgelist,
                                    interest.networks$content$adj.matrix,
                                    NA, max.persons=40)

  return(list(subject=g.subj, content=g.cont))
}

store.twomode.graphs <- function(conf, twomode.graphs, ml.id, range.id) {
  store.twomode.graph(conf$con, twomode.graphs$subject, "subject",
                      ml.id, range.id)
  store.twomode.graph(conf$con, twomode.graphs$content, "content",
                      ml.id, range.id)
}

store.initiate.response <- function(conf, ir, ml.id, range.id) {
  dat <- cbind(releaseRangeId=range.id, mlId=ml.id,
               ir[,c("name", "responses", "initiations",
                 "responses.received", "deg", "source")])
  dat$source <- mapvalues(dat$source, from=c("subject", "content"), to=c(0,1))

  ## Create SQL-compatible column names
  colnames(dat) <- c("releaseRangeId", "mlId", "personId", "responses",
                     "initiations", "responses_received", "deg", "source")

  res <- dbWriteTable(conf$con, "initiate_response", dat, append=TRUE, row.names=FALSE)
  if (!res) {
    stop("Internal error: Could not write thread.info into database!")
  }
}

## Dispatcher for all data storing functions above
store.data <- function(conf, res, range.id, ml.id) {
  store.initiate.response(conf, res$networks.dat$ir, ml.id, range.id)
  store.twomode.graphs(conf, res$twomode.graphs, ml.id, range.id)
}

create.network.plots <- function(conf, plots.path, res) {
  ## NOTE: The correlation threshold is quite critical.
  ## TODO: Find some automatical means based on the maximal number of edges.
  pdf(file.path(plots.path, "tdm_plot.pdf"))
  plot(res$doc.matrices$tdm, terms=res$termfreq, corThreshold=0.15, weighting=TRUE)
  dev.off()

  ## Visualise the correlation between communication network and interests
  ## (not sure if this is really the most useful piece of information)
  g <- ggplot(res$networks.dat$icc, aes(x=centrality, y=dist, colour=type)) +
    geom_line() +
      geom_point() + facet_grid(source~.)
  ggsave(file.path(plots.path, "interest.communication.correlation.pdf"), g)
}

dispatch.plots <- function(conf, data.path, res) {
  plots.path <- file.path(data.path, "plots")
  gen.dir(plots.path)

  create.network.plots(conf, plots.path, res)
}
