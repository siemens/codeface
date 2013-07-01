library(zoo)
library(xts)
library(lubridate)
source("../prosoda/db.r")
source("../prosoda/utils.r")
source("../prosoda/query.r")
source("../prosoda/id_manager.r")

gen.forest <- function(conf, repo.path, resdir) {
  ## TODO: Use apt ML specific preprocessing functions, not always the
  ## lkml variant
  corp.file <- file.path(resdir, "corp.base")
  doCompute <- !(file.exists(corp.file))

  if (doCompute) {
    corp.base <- gen.corpus(conf$ml, repo.path, suffix=".mbox",
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
extract.commnets <- function(forest, termfreq, repo.path, data.path) {
  cont.dir <- file.path(data.path, "commnet.terms", "content")
  subj.dir <- file.path(data.path, "commnet.terms", "subject")

  doCompute <- !(file.exists(cont.dir)) && !(file.exists(subj.dir))

  if (doCompute) {
    extract.commnet(forest, termfreq, "content", data.path)
    extract.commnet(forest, termfreq, "subject", data.path)
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
timestamp <- function(text) {
  cat (text, ": ", date(), "\n")
}

dispatch.all <- function(conf, repo.path, resdir) {
  timestamp("start")
  corp.base <- gen.forest(conf, repo.path, resdir)
  timestamp("corp.base finished")
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
  dates <- dates[!is.na(dates)]

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

  ## TODO: Find some measure (likely depending on the number of messages per
  ## time) to select suitable time intervals of interest. For many projects,
  ## weekly (and monthly) are much too short, and longer intervals need to
  ## be considered.
  periodic.analysis <- FALSE
  if (periodic.analysis) {
    analyse.sub.sequences(conf, corp.base, iter.weekly, repo.path, resdir,
                          paste("weekly", 1:length(iter.weekly), sep=""))
    analyse.sub.sequences(conf, corp.base, iter.4weekly, repo.path, resdir,
                          paste("4weekly", 1:length(iter.4weekly), sep=""))
  }

  analyse.sub.sequences(conf, corp.base, release.intervals, repo.path, resdir,
                        release.labels)

  ## #######
  ## Global analysis
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
                                  data.path, labels) {
  if (length(iter) != length(labels))
    stop("Internal error: Iteration sequence and data prefix length must match!")

  timestamps <- do.call(c, lapply(seq_along(corp.base$corp),
                                  function(i) DateTimeStamp(corp.base$corp[[i]])))
  
  cat(length(corp.base$corp), "messages in corpus\n")
  cat("Date range is", as.character(int_start(iter[[1]])), "to",
      as.character(int_end(iter[[length(iter)]])), "\n")
  cat("=> Analysing ", conf$ml, "in", length(iter), "subsets\n")

  ## Prepare a single-parameter version of do.normalise that does
  ## not expose the conf object -- the concept is not known to snatm
  ## NOTE: conf must be present in the defining scope
  do.normalise.bound <- function(authors) {
    return(do.normalise(conf, authors))
  }

  res <- mclapply(1:length(iter), function(i) {
    ## Determine the corpus subset for the interval
    ## under consideration
    cat("Processing interval ", i, ": ", labels[[i]], "\n")

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
    dispatch.steps(conf, repo.path, data.path.local, forest.corp.sub, cycles[i,])
    cat(" -> Finished interval ", i, ": ", labels[[i]], "\n")
  })
}

## User needs to make sure that data.path exists and is writeable
## dispatch.steps is called for every time interval that is considered
## in the analysis
dispatch.steps <- function(conf, repo.path, data.path, forest.corp, cycle) {
  ## TODO: Check how we can speed up prepare.text. And think about if the
  ## function is really neccessary. With stemming activated, I doubt
  ## that it really pays off.
###prep <- prepare.text(forest, progress=TRUE)
####save(file=file.path(data.path, paste("prep", ml, sep=".")), prep)
  communication.network <- compute.commnet(forest.corp, data.path)
  
  ## Returns tdm and dtm
  doc.matrices <- compute.doc.matrices(forest.corp, data.path)

  ## TODO: Provide per-ml keyword collections for the exclusion words
  termfreq <- findHighFreq(doc.matrices$tdm, exclude.list=unique(c(terms.d,
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

  ## ... and store it into the data base
  ts.df <- gen.df.from.ts(series.daily, "Mailing list activity")
  plot.name <- str_c(conf$ml, " activity")
  plot.id <- get.plot.id(conf, plot.name)

  dat <- data.frame(time=as.character(ts.df$time),
                    value=ts.df$value,
                    value.scaled=ts.df$value.scaled,
                    plotId=plot.id)

  ## NOTE: We append new values to the existing content. This way,
  ## we can plot arbitrary subsets of the series by selecting
  ## subranges, without the need to concatenate parts together
  res <- dbWriteTable(conf$con, "timeseries", dat, append=T, row.names=F)
  if (!res) {
    stop("Internal error: Could not write timeseries into database!")
  }

  ## Compute descriptive statistics
  ## NOTE: forest needs to available in the defining scope for the
  ## following four helper functions
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

  ## Determine authors and messages _per thread_
  num.authors <- sapply(unique(forest[,"threadID"]), authors.per.thread)
  num.messages <- sapply(unique(forest[,"threadID"]), messages.per.thread)
  thread.info <- data.frame(authors=num.authors, messages=num.messages,
                            tid=attr(num.messages, "names"))

  ## Infer the larges threads as measured by the number of messages per thread
  largest.threads.msgs <- sort(thread.info$messages, decreasing=T, index.return=T)

  ## ... and determine the subjects that started the threads
  subjects.msgs <- sapply(largest.threads.msgs$ix, get.subject)
  subjects.counts <- largest.threads.msgs$x

  MAX.SUBJECTS <- 200
  ## We store at most 200 subjects. This should be plenty for all
  ## reasonable purposes
  if (length(subjects.msgs) > MAX.SUBJECTS) {
    subjects.msgs <- subjects.msgs[1:MAX.SUBJECTS]
    subjects.counts <- largest.threads.msgs$x[1:MAX.SUBJECTS]
  }

  ## freq_subjects stores the subjects that received the highest
  ## attention, at most 20 of them.
  dat <-  data.frame(projectId=conf$pid, releaseRangeId=cycle$range.id,
                     subject=subjects.msgs, count=subjects.counts)
  res <- dbWriteTable(conf$con, "freq_subjects", dat, append=T, row.names=F)
  if (!res) {
    stop("Internal error: Could not write freq_subjects into database!")
  }

  ## thread_info.txt stores the number of authors and messages
  ## per thread (each thread is identified with a unique tid)
  write.table(thread.info,
              file=file.path(data.path, "thread_info.txt"), sep="\t",
              row.names=FALSE, quote=FALSE)

  ## TODO: Can we classify the messages into content catgories, e.g., technical
  ## discussions, assistance (helping users), and code submissions?

  ## Compute the two-mode graphs linking users with their interests
  twomode.graphs <- compute.twomode.graphs(conf, interest.networks)

  ## TODO: This should be represented by a class
  res <- list(doc.matrices=doc.matrices, termfreq=termfreq,
              interest.networks=interest.networks,
              twomode.graphs=twomode.graphs,
              networks.dat=networks.dat,
              thread.info=thread.info)
  save(file=file.path(data.path, "vis.data"), res)
  
  ####### End of actual computation. Generate graphs and store data etc. #######
  dispatch.plots(conf, data.path, res)
  store.data(conf, res, cycle$range.id)
}

compute.twomode.graphs <- function(conf, interest.networks) {
  ## TODO: Should max.persons be made configurable? Better would
  ## be to find a heuristic to compute a good value
  g.subj <- construct.twomode.graph(interest.networks$subject$edgelist,
                                    interest.networks$subject$adj.matrix,
                                    NA, conf$con, max.persons=30)
  g.cont <- construct.twomode.graph(interest.networks$content$edgelist,
                                    interest.networks$content$adj.matrix,
                                    NA, conf$con, max.persons=40)

  return(list(subject=g.subj, content=g.cont))
}

store.twomode.graphs <- function(conf, twomode.graphs, range.id) {
  store.twomode.graph(conf$con, twomode.graphs$subject, "subject",
                      conf$ml, range.id)
  store.twomode.graph(conf$con, twomode.graphs$content, "content",
                      conf$ml, range.id)
}

store.initiate.response <- function(conf, ir, ml, range.id) {
  dat <- cbind(releaseRangeId=range.id, ml=ml,
               ir[,c("name", "responses", "initiations",
                 "responses.received", "deg", "source")])
  dat$source <- mapvalues(dat$source, from=c("subject", "content"), to=c(0,1))

  ## Create SQL-compatible column names
  colnames(dat) <- c("releaseRangeId", "ml", "personId", "responses",
                     "initiations", "responses_received", "deg", "source")

  res <- dbWriteTable(conf$con, "initiate_response", dat, append=T, row.names=F)
  if (!res) {
    stop("Internal error: Could not write thread.info into database!")
  }
}

## Dispatcher for all data storing functions above
store.data <- function(conf, res, range.id) {
  store.initiate.response(conf, res$networks.dat$ir, conf$ml, range.id)
  store.twomode.graphs(conf, res$twomode.graphs, range.id)
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

create.descriptive.plots <- function(conf, plots.path, res) {
  ## How focused are discussions, respectively how does the number
  ## of authors scale with the number of messages per thread?
  g <- ggplot(res$thread.info, aes(x=authors, y=messages)) + geom_point() +
    xlab("Authors per thread") + ylab("Messages per thread") + geom_smooth() +
    ggtitle(conf$project)
  ggsave(file.path(plots.path, "auth_msg_scatter.pdf"), g)

  ## Distribution of authors and messages per thread
  thread.info.molten <- melt(res$thread.info)
  g <- ggplot(thread.info.molten, aes(x=variable, y=value)) + geom_boxplot() +
    scale_y_log10() + xlab("Type") + ylab("Number per thread") +
    ggtitle(conf$project)
  ggsave(file.path(plots.path, "auth_msg_dist.pdf"), g)

  thread.combined <- rbind(data.frame(num=res$thread.info$authors,
                                      type="Authors"),
                           data.frame(num=res$thread.info$messages,
                                      type="Messages"))
  g <- ggplot(thread.combined, aes(x=num, colour=type, fill=type)) +
    geom_histogram(binwidth=1, position="dodge") + scale_y_sqrt() +
    xlab("Amount of thread contributions") +
    ylab("Number of threads (sqrt transformed)") +
    scale_size("Type of contribution") + ggtitle(conf$project)
  ggsave(file.path(plots.path, "thread_contributions.pdf"), g)
}

dispatch.plots <- function(conf, data.path, res) {
  plots.path <- file.path(data.path, "plots")
  gen.dir(plots.path)

  create.network.plots(conf, plots.path, res)
  create.descriptive.plots(conf, plots.path, res)
}
