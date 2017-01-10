## This file is part of Codeface. Codeface is free software: you can
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
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(tm.plugin.mail))
suppressPackageStartupMessages(library(snatm))

source("../db.r", chdir=TRUE)
source("../utils.r", chdir=TRUE)
source("../query.r", chdir=TRUE)
source("../id_manager.r", chdir=TRUE)
source("../mc_helpers.r")
source("project.spec.r")
source("ml_utils.r")

gen.forest <- function(conf, repo.path, resdir, use.mbox=TRUE) {
  ## TODO: Use apt ML specific preprocessing functions, not always the
  ## lkml variant
  corp.file <- file.path(resdir, paste("corp.base", conf$listname, sep="."))

  if (use.mbox) {
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
  } else if (!use.mbox & file.exists(corp.file)) {
    loginfo("Loading mail data from precomputed corpus instead of mbox file")
    load(file=corp.file)
  } else {
    logerror(sprintf("Corpus file not found: %s", corp.file))
    stop()
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
    edgelist <- createedges(forest.corp$forest)
    if (length(edgelist) == 1 && is.na(edgelist)) {
      return(NA)
    }

    commnet <- adjacency(edgelist)
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
    gen.dir(cont.dir)
    extract.commnet(forest.corp$forest, termfreq, "content", data.path)

    gen.dir(subj.dir)
    extract.commnet(forest.corp$forest, termfreq, "subject", data.path)
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


## Check corpus for conditions that must be statisfied by the
## documents
check.corpus.precon <- function(corp.base) {
  ######
  ## Preconditions
  ######
  ## Condition #1: Emails must have at most one reference Id
  get.ref.id.lines <- function(x) { grep("^References:", meta(x, tag="header"),
                                    value = FALSE, useBytes = TRUE)}
  rmv.multi.refs <- function(doc) {
                      ref.id.lines <- get.ref.id.lines(doc)
                      rmv.lines <- ref.id.lines[-1]
                      header <- meta(doc, tag="header")

                      if(length(rmv.lines) != 0) {
                        ## Log number of removed reference id lines and message id
                        message.id <- meta(doc, tag="id")
                        msg <- sprintf(paste("Removing %d id references",
                                             "from corpus due to precondition",
                                             "violation(s) while checking message %s", sep=" "),
                                       length(rmv.lines), message.id)
                        loginfo(msg, logger="ml.analysis")
                        header <- header[-rmv.lines]
                      }

                      return(header)
                    }

  ## Condition #2: Authors must be specified using "name <email>" format
  fix.author <- function(doc) {
    author <- meta(doc, tag="author")

    if(identical(author, character(0))) {
      author <- "unknown"
    }

    ## Remove problematic punctuation characters
    problem.characters <- c("\"", ",", ";", "\\(", "\\)")
    for (p.char in problem.characters) {
      author <- gsub(p.char, " ", author)
    }

    ## Trim trailing and leading whitespace
    author <- str_trim(author)

    ## Replace textual ' at  ' with @, sometimes
    ## we can recover an email
    author <- sub(' at ', '@', author)
    author <- sub(' AT ', '@', author)

    ## Handle case where author is like
    ## Adrian Prantl via llvm-dev <llvm-dev@lists.llvm.org>
    pattern <- " via [[:print:]]*"
    if (grepl(pattern,author, TRUE)) {
      ## Extract name and replace email part
      name <- gsub(pattern, author, replacement="")

      ## Generate ficticious email from name part
      email <- paste("<", gsub(" ", "." ,name), "@unknown.tld>", sep="")

      author <- paste(name, email)
    }

    ## Check if email exists
    email.exists <- grepl("<.+>", author, TRUE)

    if(!email.exists) {
      msg <- str_c("Incorrectly formatted author field (expected XXX XXX ",
                   "<xxxyyy@abc.tld>); attempting to recover from: ", author)
      logdevinfo(msg, logger="ml.analysis")

      ## Check for @ symbol
      r <- regexpr("\\S+@\\S+", author, TRUE)
      email <- substr(author, r, r + attr(r,"match.length")-1)

      ## Use fixed=TRUE to handle synthetic email addresses like
      ## thomas.beckmann-kcH4OoMoNbE4Q++5jOxPmw@public.gmane.org that
      ## would otherwise be interpreted as invalid regexp
      ## Additionally, only perform the substitution if email
      ## is not empty, since otherwise sub() will fail.
      if(email == "") {
        ## email address could not be resolved, use a good (but invalid)
        ## replacement
        email <- "could.not.resolve@unknown.tld"
        name <- author
      } else {
        ## If an email address was detected, use the author
        ## string minus the new email part as name, and construct
        ## a valid name/email combination
        name <- sub(email, "", author, fixed=TRUE)
        name <- fix.name(name)
      }

      ## In some cases only an email is provided
      if (name=="") {
        name <- gsub("\\.", " ", gsub("@.*", "", email))
      }

      author <- paste(name, ' <', email, '>', sep="")
    }
    else {
      ## There is a correct email address. Ensure that the order is correct
      ## and fix cases like "<hans.huber@hubercorp.com> Hans Huber"

      ## Get email and name parts
      r <- regexpr("<.+>", author, TRUE)
      ## email is at start
      if(r == 1) {
        ## Check if only an email is provided
        if(attr(r, "match.length") == nchar(author)) {
          ## Only an email like "<hans.huber@hubercorp.com>" is provided
          email <- substr(author, r + 1, r + nchar(author) - 2)
          name <- gsub("\\.", " ", gsub("@.*", "", email))
        } else {
          ## email and name both are provided
          email <- substr(author, r, r + attr(r, "match.length") - 1)
          name <- sub(email, "", author, fixed=TRUE)
          name <- fix.name(name)
        }

        email <- str_trim(email)
        author <- paste(name, ' <', email, '>', sep="")
      }
    }

    ## Check if name looks like an email address (i.e., there are more than
    ## one @ symbol in the author string): Since that causes parsing problems
    ## in further steps of the analysis and the ID service, we use only the
    ## local part of an email address as name.
    ## E.g., "'hans.huber@hubercorp.com' <hans.huber@hubercorp.com>"
    if (length(gregexpr(pattern = "@", author, fixed = TRUE)[[1]]) > 1) {
      ## Get email and name parts first
      r <- regexpr("<.+>", author, TRUE)
      email <- substr(author, r, r + attr(r, "match.length") - 1)
      name <- sub(email, "", author, fixed=TRUE)
      name <- fix.name(name)

      if(regexpr("\\S+@\\S+", name, TRUE) == 1) {
        ## Name looks like an email address. Use only local part as name.
        name <- gsub("\\.", " ", gsub("@.*", "", name))
      }

      author <- paste(name, email)
    }

    ## return new author string
    return(author)
  }

  ## Condition #3: Date information should incorporate time-zone information and should be present
  fix.date <- function(doc) {
    ## re-parse date headers to incorporate time-zone data.
    ## this needs to be done, because the date inside the mbox file is initially parsed with
    ## the pattern "%a, %d %b %Y %H:%M:%S" which does not incorporate time-zone data (%z) [1], which is,
    ## on the other side, incorporated in the commit analysis.
    ## [1] (see https://github.com/wolfgangmauerer/snatm/blob/master/pkg/R/makeforest.r#L47)

    ## get the date header as inside the mbox file
    headers = meta(doc, tag = "header")
    date.header = grep("^Date: ", headers, value = TRUE, useBytes = TRUE, ignore.case = TRUE)
    date.header = gsub("^Date: ", "", date.header, ignore.case = TRUE)

    ## break early if 'Date' header is missing
    if (length(date.header) == 0) {
      logwarn(paste("Mail is missing header 'Date':", meta(doc, tag = "id")))
      return(list(NA, 0))
    }

    ## only consider first date header in document if more are given
    if (length(date.header) > 1) {
      date.header = date.header[1]
    }

    ## patterns without time-zone pattern
    date.formats.without.tz = c(
      "%a, %d %b %Y %H:%M:%S",  # initially used format; e.g., "Date: Tue, 20 Feb 2009 20:24:54 +0100"
      "%d %b %Y %H:%M:%S",  # missing weekday; e.g., "Date: 20 Feb 2009 20:24:54 +0100"
      "%a, %d %b %Y %H:%M"  # missing seconds; e.g. "Date: Wed, 21 Aug 2013 15:02 +0200"
    )
    ## append time-zone part and incorporate pattern without time-zone indicator
    date.formats = c(
      paste(date.formats.without.tz, "%z", sep = " "),
      date.formats.without.tz
    )

    ## try to re-parse the header using adapted patterns:
    ## parse date until any match with a pattern is found (date.new is not NA)
    date.format.matching = NA
    for (date.format in date.formats) {
      date.new = strptime(date.header, format = date.format, tz = "GMT")

      # if the date has been parsed correctly, break the loop
      if (!is.na(date.new)) {
        date.format.matching = date.format
        break()
      }
    }

    ## store time offset (i.e., time zone) away from GMT
    if (!is.na(date.format.matching)) {
      date.offset = format(strptime(date.header, format = date.format.matching, tz = ""), format = "%z")
      date.offset = as.integer(date.offset)
    } else {
      date.offset = 0
    }

    return(list(date.new, date.offset))
  }

  ## Fix subject (remove problematic characters)
  fix.subject <- function(doc) {
    ## get subject from headers
    subject = meta(doc, tag = "heading")

    ## Remove TABS -- dbWriteTable cannot handle these properly
    subject <- gsub("\t", " ", subject, fixed=TRUE, useBytes=TRUE)

    return(subject)
  }

  ## Apply checks of conditions to all documents
  fix.corpus.doc <- function(doc) {
    meta(doc, tag="header") <- rmv.multi.refs(doc)
    meta(doc, tag="author") <- fix.author(doc)

    fixed.date = fix.date(doc)
    meta(doc, tag="datetimestamp") <- fixed.date[[1]]
    meta(doc, tag="datetimestampOffset") <- fixed.date[[2]]

    meta(doc, tag="heading") <- fix.subject(doc)

    return(doc)
  }

  ## Apply checks and fixes for all preconditions to all
  ## documents in the corpus
  corp.base$corp <- tm_map(corp.base$corp, fix.corpus.doc)
  class(corp.base$corp) <- class(corp.base$corp.orig)

  return(corp.base)
}
## ################### Analysis dispatcher ######################
## ################### Let the above rip ########################

dispatch.all <- function(conf, repo.path, resdir) {
  loginfo("Starting mailinglist analysis", logger="ml.analysis")
  corp.base <- gen.forest(conf, repo.path, resdir, use.mbox = !conf$use_corpus)
  loginfo("corp.base finished", logger="ml.analysis")
  ## TODO: When we consider incremental updates, would it make sense
  ## to just update the corpus, and let all other operations run
  ## from scratch then? This would likely be the technically easiest
  ## solution..

  ## Remove documents in the corpus that do not satisfy the necessary
  ## preconditions
  corp.base <- check.corpus.precon(corp.base)

  ## #######
  ## Split the data into smaller chunks for time-resolved analysis
  ## Get all message timestamps, and clean the invalid ones
  dates <- do.call(c,
                   mclapply(seq_along(corp.base$corp),
                          function(i) {
                            return(as.POSIXct(meta(corp.base$corp[[i]], tag="datetimestamp")))
                          }
                   ))
  ## Sort the dates to enable faster algorithms. This also removes NAs
  dates <- sort(dates)

  ## Compute a list of intervals for the project release cycles
  cycles <- get.cycles(conf, allow.empty.ranges=TRUE)

  if (nrow(cycles) != 0) {
    ## NOTE: We store the lubridate intervals in a list (instead of
    ## simply appending them to the cycles data frame) because they
    ## are coerced to numeric by the conversion to a data frame.
    release.intervals <- list(dim(cycles)[1])
    for (i in 1:(dim(cycles)[1])) {
      release.intervals[[i]] <- interval(cycles$date.start[[i]],
                                         cycles$date.end[[i]])
    }

    release.labels <- as.list(cycles$cycle)

    ## The mailing list data may not cover the complete timeframe of
    ## the repository, so remove any empty intervals
    nonempty.release.intervals <- get.nonempty.intervals(dates, release.intervals)
    release.intervals <- release.intervals[nonempty.release.intervals]
    release.labels <- release.labels[nonempty.release.intervals]
  }
  else {
    nonempty.release.intervals <- NULL
  }

  ## Obtain a unique numerical ID for the mailing list (and clear
  ## any existing results on the way)
  ml.id <- gen.clear.ml.id.con(conf$con, conf$listname, conf$pid)

  if (length(nonempty.release.intervals) == 0) {
    loginfo("Mailing list does not cover any release range.")
  }
  else {
    ## TODO: Find some measure (likely depending on the number of messages per
    ## time) to select suitable time intervals of interest. For many projects,
    ## weekly (and monthly) are much too short, and longer intervals need to
    ## be considered.
    periodic.analysis <- FALSE
    if (periodic.analysis) {
      loginfo("Periodic analysis", logger="ml.analysis")
      ## Select weekly and monthly intervals (TODO: With the new flexible
      ## intervals in place, we could select proper monthly intervals)
      iter.weekly <- gen.iter.intervals(dates, 1)
      iter.4weekly <- gen.iter.intervals(dates, 4)

      analyse.sub.sequences(conf, corp.base, iter.weekly, repo.path, resdir,
                            paste("weekly", 1:length(iter.weekly), sep=""))
      analyse.sub.sequences(conf, corp.base, iter.4weekly, repo.path, resdir,
                            paste("4weekly", 1:length(iter.4weekly), sep=""))
    }

    loginfo("Analysing subsequences", logger="ml.analysis")

    ## Also obtain a clear plot for the mailing list activity
    activity.plot.name <- str_c(conf$listname, " activity")
    activity.plot.id <- get.clear.plot.id(conf, activity.plot.name)
    analyse.sub.sequences(conf, corp.base, release.intervals, repo.path, resdir,
                          release.labels, ml.id, activity.plot.id)

  }

  ## #######
  ## Global analysis
  loginfo("Global analysis", logger="ml.analysis")
  ## NOTE: We only compute the forest for the complete interval to allow for creating
  ## descriptive statistics.
  corp <- corp.base$corp
  ## Remove duplicate mails
  corp <- corp[!duplicated(meta(corp, "id"))]

  ## NOTE: conf must be present in the defining scope
  do.normalise.bound <- function(authors) {
    return(do.normalise(conf, authors))
  }

  ## sort corpus by date
  dates.vector = do.call(c, meta(corp, "datetimestamp"))
  dates.order = order(dates.vector)
  corp = corp[dates.order]

  forest.corp <- list(forest=make.forest(corp, do.normalise.bound),
                      corp=corp,
                      corp.orig=corp.base$corp.orig)

  ## Store all mails to database
  store.mail(conf, forest.corp$forest, corp, ml.id)

  ## Store all mails to local storage
  resdir.complete <- file.path(resdir, "complete")
  gen.dir(resdir.complete)
  save(file=file.path(resdir.complete, "forest.corp"), forest.corp)
}


analyse.sub.sequences <- function(conf, corp.base, iter, repo.path,
                                  data.path, labels, ml.id, activity.plot.id) {
  if (length(iter) != length(labels))
    stop("Internal error: Iteration sequence and data prefix length must match!")

  timestamps <- do.call(c, lapply(seq_along(corp.base$corp),
                                  function(i) meta(corp.base$corp[[i]], tag="datetimestamp")))

  loginfo(paste(length(corp.base$corp), "messages in corpus"), logger="ml.analysis")
  loginfo(paste("Date range is", as.character(int_start(iter[[1]])), "to",
      as.character(int_end(iter[[length(iter)]]))), logger="ml.analysis")
  loginfo(paste("=> Analysing ", conf$listname, "in", length(iter), "subsets"),
          logger="ml.analysis")

  ## NOTE: Everything that is supposed to be computed in parallel needs to
  ## go into this loop.
  res <- mclapply.db(conf, 1:length(iter), function(conf, i) {
    ## Determine the corpus subset for the interval
    ## under consideration
    loginfo(paste("Processing interval ", i, ": ", labels[[i]]),
            logger="ml.analysis")

    ## Prepare a single-parameter version of do.normalise that does
    ## not expose the conf object -- the concept is not known to snatm
    ## NOTE: conf must be present in the defining scope. In particular,
    ## it must be the correct per-core conf object in parallel calculations,
    ## which is why we define the function locally here.
    do.normalise.bound <- function(authors) {
      return(do.normalise(conf, authors))
    }

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
    res <- dispatch.steps(conf, repo.path, data.path.local, forest.corp.sub,
                          cycles[i,], ml.id, activity.plot.id)
    if (is.na(res)) {
      loginfo(paste("Failed to process interval ", i, ": ", labels[[i]]),
              logger="ml.analysis")
    } else {
      loginfo(paste(" -> Finished interval ", i, ": ", labels[[i]]),
              logger="ml.analysis")
    }
  })

  ## No need to return anything since the results are available as
  ## side effects in the database.
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
  if (!is.matrix(communication.network)) {
    ## We could not create a communication network, so
    ## give up on this interval
    return(NA)
  }

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
  msgs <- lapply(forest.corp$corp, function(x) { as.POSIXct(meta(x, tag="datetimestamp")) })
  msgs <- do.call(c, msgs)

  series <- xts(rep(1,length(msgs)), order.by=msgs)
  series.daily <- apply.daily(series, sum)

  ## ... and store it into the data base, appending it to the activity plot.
  ts.df <- gen.df.from.ts(series.daily, "Mailing list activity")
  dat <- data.frame(time=as.character(ts.df$time),
                    value=ts.df$value,
                    value_scaled=ts.df$value.scaled,
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
    meta(forest.corp$corp[[mailID]], tag="datetimestamp")
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
              thread.info=thread.info,
              communication.network=communication.network)
  save(file=file.path(data.path, "vis.data"), res)

  ####### End of actual computation. Generate graphs and store data etc. #######
  dispatch.plots(conf, data.path, res)
  store.data(conf, res, cycle$range.id, ml.id)

  return(0) ## Returning a non-NA value signifies success
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

## Write all mails in forest to database
store.mail <- function(conf, forest, corp, ml.id ) {
  columns <- c("threadID", "author", "subject")
  dat <- as.data.frame(forest)[, columns]
  dat$ID <- rownames(dat)
  dat$mlId <- ml.id
  dat$projectId <- conf$pid

  ## Extract dates from corpus and add them to the data frame
  dates <- meta(corp, "datetimestamp")
  dates.df <- data.frame(ID=names(dates),
                         creationDate=sapply(dates, as.character))
  dat <- merge(dat, dates.df, by="ID")

  ## Extract date offsets from corpus and add them to the data frame
  date.offsets <- meta(corp, "datetimestampOffset")
  date.offsets.df <- data.frame(ID=names(date.offsets),
                         creationDateOffset=sapply(date.offsets, as.character))
  dat <- merge(dat, date.offsets.df, by="ID")

  ## set proper column headings
  colnames(dat)[which(colnames(dat)=="ID")] <- "messageId"
  colnames(dat)[which(colnames(dat)=="threadID")] <- "threadId"

  ## Re-order columns to match the order as defined in the database to
  ## improve the stability
  dat = dat[c("projectId", "threadId", "mlId", "author", "subject",
              "creationDate", "creationDateOffset", "messageId")]

  res <- dbWriteTable(conf$con, "mail", dat, append=TRUE, row.names=FALSE)

  if (!res) {
    stop("Internal error: Could not write mails into database!")
  }
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
    stop("Internal error: Could not write initiate_response into database!")
  }
}

store.communication.graph <- function(conf, communication.network, range.id) {
  g <- graph.adjacency(communication.network)
  edgelist <- get.data.frame(g, what="edges")
  colnames(edgelist) <- c("fromId", "toId")
  edgelist <- gen.weighted.edgelist(edgelist)
  type <- "email"
  write.graph.db(conf, range.id, type, edgelist, -1)
}

## Dispatcher for all data storing functions above
store.data <- function(conf, res, range.id, ml.id) {
  store.initiate.response(conf, res$networks.dat$ir, ml.id, range.id)
  store.twomode.graphs(conf, res$twomode.graphs, ml.id, range.id)
  store.communication.graph(conf, res$communication.network, range.id)
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
