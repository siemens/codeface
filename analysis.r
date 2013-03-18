gen.forest <- function(ml, repo.path, data.path, doCompute) {
  ## TODO: Use apt ML specific preprocessing functions, not always the
  ## lkml variant
  if (doCompute) {
    corp.base <- gen.corpus(ml, repo.path,
                            suffix=".mbox", marks=c("^_{10,}", "^-{10,}", "^[*]{10,},",
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
    save(file=file.path(data.path, "corp.base"), corp.base)
  } else {
    load(file=file.path(data.path, "corp.base"))
  }

  return(corp.base)
}


compute.doc.matrices <- function(forest.corp, data.path, doCompute) {
  ## NOTE: Stemming seems to have some encoding problem with UTF-8.
  ## And it takes some amount of time: About one hour for 10000 documents.
  ## TODO: Let this run in parallel (is this already supported by the MPI
  ## methods of package tm?)
  
  ## TODO: Should we set the minimal wordlength to something larger than 3?
  ## (see ?termFreq for possible options)
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

    save(file=file.path(data.path, "tdm"), tdm)
    save(file=file.path(data.path, "dtm"), dtm)
    ##save(file=file.path(data.path, "diss"), diss)
  } else {
    load(file=file.path(data.path, "tdm"))
    load(file=file.path(data.path, "dtm"))
    ##load(file=file.path(data.path, "diss")
  }

  return(list(tdm=tdm, dtm=dtm))
}


compute.commnet <- function(forest.corp, data.path, doCompute) {
  if (doCompute) {
    commnet <- adjacency(createedges(forest.corp$forest))
    save(file=file.path(data.path, "commnet"), commnet)
  } else {
    load(file=file.path(data.path, "commnet"))
  }
  
  return(commnet)
}


# These two calls are only relevant for the side effects -- they populate
# basedir/<ml>/subject resp. /content
# Iterate over all terms in termfreq, and create the adjacency matrix
# for the communication network associated with each term
extract.commnets <- function(forest, termfreq, ml, repo.path, data.path,
                             doCompute) {
  if (doCompute) {
    extract.commnet(forest, termfreq, "content", data.path)
    extract.commnet(forest, termfreq, "subject", data.path)
  }
}


compute.interest.networks <- function(termfreq, ml,
                                      NUM.NET.SUBJECT, NUM.NET.CONTENT,
                                      data.path, doCompute) {
  if (doCompute) {
    net.subject <- gen.net("subject", termfreq, ml, data.path, NUM.NET.SUBJECT)
    net.content <- gen.net("content", termfreq, ml, data.path, NUM.NET.CONTENT)
    save(file=file.path(data.path, "net.subject"), net.subject)
    save(file=file.path(data.path, "net.content"), net.content)
  } else {
    load(file=file.path(data.path, "net.subject"))
    load(file=file.path(data.path, "net.content"))
  }

  return(list(subject=net.subject, content=net.content))
}


analyse.networks <- function(forest, interest.networks, communication.network) {
  ######### Analyse interest and communication (ICC) networks #######
  ## (very fast, no persistent storing necessary)
  networks.subj <- gen.cmp.networks(interest.networks$subject, communication.network)
  networks.cont <- gen.cmp.networks(interest.networks$content, communication.network)
  dat.subj <- gen.networks.df(networks.subj)
  dat.cont <- gen.networks.df(networks.cont)

  dat.icc <- data.frame(dat.subj, source="subject")
  dat.icc <- rbind(dat.icc, data.frame(dat.cont, source="content"))
  rm(dat.subj); rm(dat.cont)

  ####### Initiation-response (IR) structure for the mailing list ######
  ## TODO: Determine if any extremal values are outliers
  ## (this plot seems to be quite informative. Compare for multiple projects)
  dat.subj <- compute.initiate.respond(forest, networks.subj[[2]], networks.subj[[3]])
  dat.cont <- compute.initiate.respond(forest, networks.cont[[2]], networks.cont[[3]])
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

dispatch.all <- function(ml, repo.path, data.path, doCompute) {
  timestamp("start")
  corp.base <- gen.forest(ml, repo.path, data.path, doCompute)
  timestamp("corp.base finished")
  ## #######
  ## Split the data into smaller chunks for time-resolved analysis
  dates <- do.call(c,
                   lapply(seq_along(corp.base$corp),
                          function(i) as.POSIXct(DateTimeStamp(corp.base$corp[[i]])))
                   )
  dates.cleaned <- dates[!is.na(dates)]
  iter.weekly <- gen.iter.intervals(dates.cleaned, 1)
  iter.4weekly <- gen.iter.intervals(dates.cleaned, 4)
  
  ## TODO: Parallellise with mclapply once things work properly in serial
  ## Weekly analysis

  dispatch.sub.sequences(corp.base, iter.weekly, repo.path, data.path,
                         ml, doCompute)
##  dispatch.sub.sequences(corp.base, iter.4weekly, repo.path, data.path,
##                         ml, doCompute)

  
  ## TODO: Should we restrict the maximal analysis length to one month?
  ## Not sure if the results make sense for a longer time frame
  ## Some descriptive statistics do make sense, but for instance not
  ## things like computing text-document matrices. We could compute the
  ## descriptive things directly from the corpus.
  
  ## #######
  ## Global analysis
  corp <- corp.base$corp
  forest.corp <- list(forest=make.forest(corp),
                      corp=corp,
                      corp.orig=corp.base$corp.orig)

  data.path.local <- file.path(data.path, ml)
  gen.dir(data.path.local)
  dispatch.steps(ml, repo.path, data.path.local, forest.corp, doCompute)
}


dispatch.sub.sequences <- function(corp.base, iter, repo.path,
                                   data.path, ml, doCompute) {
  timestamps <- do.call(c, lapply(seq_along(corp.base$corp),
                                  function(i) DateTimeStamp(corp.base$corp[[i]])))
  
  cat(length(corp.base$corp), "messages in corpus\n")
  cat("Date range is ", as.character(int_start(iter[[1]])), "to",
      as.character(int_end(iter[[1]])), "\n")
  cat("=> Analysing ", ml, "in", length(iter), "subsets\n")

  clusterExport(getSetCluster(), "ml")
  clusterExport(getSetCluster(), "data.path")
  clusterExport(getSetCluster(), "repo.path")

  lapply.cluster <- function(x, FUN, ...) {
    snow::parLapply(getSetCluster(), x, FUN, ...)
  }
  if (tm::clusterAvailable() && length(iter) > 1) {
    do.lapply <- lapply.cluster
    clusterCall(getSetCluster(), function() { source("includes.r"); return(NULL) })
  } else {
    do.lapply <- lapply
  }
  res <- do.lapply(1:length(iter), function(i) {
    ## Determine the corpus subset for the interval
    ## under consideration
    cat("Processing interval ", i, "\n");
    curr.int <- iter[[i]]
    idx <- which(timestamps >= int_start(curr.int) & timestamps < int_end(curr.int))
    corp.sub <- corp.base$corp[idx]
    
    forest.corp.sub <- list(forest=make.forest(corp.sub),
                            corp=corp.sub,
                            corp.orig=corp.base$corp.orig[idx])
    
    ## ... and perform all analysis steps
    data.path.local <- file.path(data.path, paste(data.prefix, i, sep="."))
    gen.dir(data.path.local)
    save(file=file.path(data.path.local, "forest.corp"), forest.corp.sub)
    
    dispatch.steps(ml, repo.path, data.path.local, forest.corp.sub, doCompute)
    cat(" -> Finished week ", i, "\n")
  })
}

dispatch.steps <- function(ml, repo.path, data.path, forest.corp, doCompute) {
  ## TODO: Check how we can speed up prepare.text. And think about if the
  ## function is really neccessary. With stemming activated, I doubt
  ## that it really pays off.
###prep <- prepare.text(forest, progress=TRUE)
####save(file=file.path(data.path, paste("prep", ml, sep=".")), prep)
  communication.network <- compute.commnet(forest.corp, data.path, doCompute)
  timestamp("communication.network finished")
  
  ## Returns tdm and dtm
  doc.matrices <- compute.doc.matrices(forest.corp, data.path, doCompute)
  timestamp("doc.matrices finished")
  
  ## TODO: Provide per-ml keyword collections
  termfreq <- findHighFreq(doc.matrices$tdm, 40,
                           unique(c(terms.d, terms.coll, terms.c, terms.programming)))
  timestamp("termfreq finished")
  
  ## NOTE: For most projects, technical left-overs (like footers from majordomo
  ## etc.) will appear in the termfreq list. To find out which elements need
  ## to be removed from emails by grepping for the artefacts, use
  ## id <- function(x) return(x)
  ## text <- sapply(forest.corp$corp[1:5000], id)
  ## grep("keyword", text)
  ## ... and then inspect the appropriate messages in corp.orig to see which additional
  ## filter needs to be applied
  
  gen.dir(file.path(data.path, "commnet.terms"))
  extract.commnets(forest.corp, termfreq, ml, repo.path, data.path, doCompute)
  timestamp("extract.commnets finished")
  
  ## TODO: Find justifiable heuristics for these configurable parameters
  NUM.NET.SUBJECT <- 25
  NUM.NET.CONTENT <- 50
  interest.networks <- compute.interest.networks(termfreq, ml,
                                                 NUM.NET.SUBJECT, NUM.NET.CONTENT,
                                                 data.path, doCompute)
  
  networks.dat <- analyse.networks(forest.corp$forest, interest.networks,
                                   communication.network)
  timestamp("networks finished")

  # TODO: This should go into a class
  res <- list(doc.matrices=doc.matrices, termfreq=termfreq,
              interest.networks=interest.networks, networks.dat=networks.dat)
  save(file=file.path(data.path, "vis.data"), res)
  
  ## ######### End of actual computation. Generate graphs etc. ##############
  dispatch.plots(data.path, res)
}


dispatch.plots <- function(data.path, res) {
  plots.path <- file.path(data.path, "plots")
  gen.dir(plots.path)
  ## NOTE: The correlation threshold is quite critical.
  ## TODO: Find some automatical means based on the maximal number of edges.
  pdf(file.path(plots.path, "tdm_plot.pdf"))
  plot(res$doc.matrices$tdm, terms=res$termfreq, corThreshold=0.15, weighting=TRUE)
  dev.off()

  ## NOTE: larger threshold -> less authors
  ## edgelist is interest.networks$subject[[1]]
  ## adjacency matrix (net in Bohn's notation) is interest.networks$subject[[2]]
  ## respectively same elements in net.content
  gen.termplot(res$interest.networks$subject[[1]], res$interest.networks$subject[[2]],
               NA, file.path(plots.path, "termplot_subject.pdf"), max.persons=30)
  gen.termplot(res$interest.networks$content[[1]], res$interest.networks$content[[2]],
               NA, file.path(plots.path, "termplot_content.pdf"), max.persons=40)

  ## Visualise the correlation between communication network and interests
  ## (not sure if this is really the most useful piece of information)
  g <- ggplot(res$networks.dat$icc, aes(x=centrality, y=dist, colour=type)) + geom_line() +
    geom_point() + facet_grid(source~.)
#  print(g)
  ggsave(file.path(plots.path, "interest.communication.correlation.pdf"), g)

  g <- ggplot(res$networks.dat$ir, aes(x=x, y=y)) + geom_point(aes(size=deg, colour=col)) +
    scale_x_log10() + scale_y_log10() + opts(title=ml) + facet_grid(source~.) +
      xlab("Messages initiated (log. scale)") + ylab("Responses (log. scale)")
  print(g)
  ggsave(file.path(plots.path, "init.response.log.pdf"), g)

  ## TODO: Perform automatic outlier detection, and plot the region which
  ## contains most elements
  ## TODO: Maybe we should jitter the points a little
  g <- ggplot(res$networks.dat$ir, aes(x=x, y=y)) + geom_point(aes(size=deg, colour=col)) +
    opts(title=ml) + xlab("Messages initiated") + ylab("Responses")
  ggsave(file.path(plots.path, "init.response.pdf"), g)
}
