# Some local helper functions for QuantArch
# sourceDir is stolen from somewhere, but I don't recall from where. TODO: Check
# before publishing
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

## Create a directory if it does not yet exist
gen.dir <- function(dir) {
  if (!file_test("-d", dir)) {
    if (!dir.create(dir, recursive=TRUE)) {
      stop("Cannot create directory ", dir)
    }
  }
}

# Based on code from the snatm repository (numerical optimisation
# was added by WM)
gen.termplot <- function(edgelist, net, threshold, outfile, max.persons=NA, verbose=FALSE) {
  peoplelist <- edgelist[,1]

  exclude.list <- c() # TODO: Make this a function argument, and use it
  peoplelist <- peoplelist[!(peoplelist %in% exclude.list)]

  twomode <- net
  people <- which(is.element(rownames(twomode), unique(peoplelist)))

  if (!is.na(max.persons)) {
    # Determine the optimal value of threshold so as to include at most
    # max.persons persons into the network

    # f(t) \equiv sum(rowSums(subs) > 0) is the quantity of interest: It
    # determines how many persons will contribute to the network because they
    # are correlated with at lest one term. By setting all values below a (to
    # be optimised) threshold to zero, we can decrease the number of
    # persons. So the optimisation problem is: Find threshold such that
    # sum(rowSums(subs)) is close to max.persons, but not larger.
    # In other words:
    # Minimise max.persons-f(t) using t subject to f(t) <= max.persons
    # In order to avoid the computationally more complex constrained
    # optimisation, we simply minimise abs(max.persons-f(t)). This way,
    # we may end up with slightly more persons then desired, but this
    # is of no concern.
    objective <- function(thresh) {
      # TODO: This is the wrong objective function.
      # We need to compute the results of sna::component.largest etc.
      # to actually determine the number of persons
      tmp <- twomode
      tmp[tmp < thresh] <- 0
      deg <- sna::degree(tmp, cmode="freeman")
      tmp <- tmp[,deg>0]
      tmp <- tmp[deg>0,]
      tmp <- sna::component.largest(tmp,connected="weak",result="graph")

      # tmp can have shrunk (a lot) after applying component.largest,
      people <- which(is.element(rownames(tmp), unique(peoplelist)))
      if (verbose)
        cat("Result for threshold ", thresh, ": ", length(people), "\n")
      return (abs(max.persons - length(people)))
    }

    # Found the optimal threshold, adapt the network accordingly
    threshold <- optimise(objective, c(0,1))
    if (verbose)
      cat("Found optimal threshold ", threshold$minimum)
    twomode[twomode < threshold$minimum] <- 0
  } else {
    twomode[twomode < threshold] <- 0
  }

  deg <- sna::degree(twomode, cmode="freeman")
  twomode <- twomode[,deg>0]
  twomode <- twomode[deg>0,]
  twomode <- sna::component.largest(twomode,connected="weak",result="graph")
  deg <- sna::degree(twomode)

  labelcol <- rep(rgb(0,0,1,0.75),dim(twomode)[1])
  
  # component.largest shrinks the adjacency matrix, so we need to recompute
  # the people list indices
  people <- which(is.element(rownames(twomode), unique(peoplelist)))

  labelcol[people] <- "red"
  pdf(file=outfile)
  par(mar=c(0,0,0,0))
  gplot.snatm(twomode
              ,gmode="graph"
              ,vertex.col="white"
              ,vertex.cex=1
              ,label=rownames(twomode)
              ,label.col=labelcol
              ,label.cex=(deg^0.25)*0.35
              ,label.pos=5
              ,boxed.labels=FALSE
              ,edge.lwd=0.1
              ,vertex.border="white"
              ,edge.col="grey")
  dev.off()
}

# TODO: Do something with the sizes output of edgelist (which describes the weight
# of each keyword), for instance visualise the distribution as a sanity indicator
gen.net <- function(type, termfreq, ml, data.path, max.terms) {
  if (!(type %in% c("subject", "content"))) {
    stop ("Please specify either 'subject' or 'content' for type")
  }
  res <- centrality.edgelist(termfreq, type, data.path, max.terms)
  net <- adjacency(res[[1]], mode="addvalues", directed=F)
  
#  print(ggplot(data.frame(x=res[[2]]), aes(x=x)) + geom_histogram(binwidth=1))
  return(list(res[[1]], net))
}

fixup.network <- function(.net) {
  # Replace NA entries with explicit NA strings
  # TODO: Under which circumstances can this happen?
  if (any(is.na(rownames(.net)))){
    for (i in 1:which(is.na(rownames(.net)))){
      rownames(.net)[is.na(rownames(.net))][i] <-
        colnames(.net)[is.na(rownames(.net))][i] <- paste("NA",i,sep="")
    }
  }

  return(.net)
}

do.normalise <- function(authors) {
  # Author name  normalisation (taken from the snatm repository)
  # TODO: We apply similar techniques in the git analysis. This should be
  # unified.
  authors <- normalizeauthors(authors)
  authors <- sapply(authors,sortnames,USE.NAMES=FALSE)
  authors <- sapply(authors,emailfirst,USE.NAMES=FALSE)
  # TODO: Use/generate project specific  replacement clusters
  authors <- final(authors)

  return(authors)
}

# Method is adapted from snatm. The function defined there seems wrong -- the
# centrality index is always scaled to [0,1], even if, say, only the first
# part of the interval is populated, and the second part consists solely of NSa
# (because there are no such central persons in the network)
gen.networks.df <- function(networks) {
  # For each centrality measure, compute a mapping from centrality to correlation:
  # Given a desired centrality c, select only the adjacency matrix entrie that
  # exceed c from the interest and communication networks. Then, re-arrange the numbers
  # into a vector and compute the correlation between the vectors. If the
  # correlation is large, then the same authors appear in both facedes of the
  # networks
  interestnet <- networks[[1]]
  network.red <- networks[[2]]
  cty.list <- networks[[3]]
  ret <- NULL
  
  for (cty.idx in seq_along(cty.list)) {
    # cty = centrality
    cty.values <- seq(0, max(cty.list[[cty.idx]]), length.out=100)

    ## NOTE: The paper by Bohn et al. uses correlation as distance measure;
    ## since this can become negative, it does not seem to be the easiest to
    ## interpret choice.  The cosine distance, on the other hand, is
    ## normalised on [0,1] (see the proxy package for alternative distance
    ## measures)
#    distfn <- cor
    distfn <- cosine
    
    compute.similarity <- function(cty.minval) {
      data.frame(dist=distfn(as.vector(interestnet[cty.list[[cty.idx]] >= cty.minval,
                                                   cty.list[[cty.idx]] >= cty.minval]),
                             as.vector(network.red[cty.list[[cty.idx]] >= cty.minval,
                                                   cty.list[[cty.idx]] >= cty.minval])),
                 centrality=cty.minval/max(cty.list[[cty.idx]]))
    }

    tmp <- do.call(rbind, lapply(cty.values, compute.similarity))
    ret <- rbind(ret, cbind(as.data.frame(tmp), type=names(cty.list[cty.idx])))
  }

  ret$type <- as.factor(ret$type)
  return(ret)
}

findHighFreq <- function(x, num=10, exclude.list=list()) {
  if (inherits(x, "DocumentTermMatrix")) 
        x <- t(x)
  tmp <- sort(slam::row_sums(x), decreasing=T)[1:(num+length(exclude.list))]

  rs <- names(tmp)
  names(rs) <- tmp

  rs <- rs[!(rs %in% exclude.list)]
  if (length(rs) > num)
    rs <- rs[1:num]
  
  return(rs)
}

# Compute the interest network (adapted from the snatm paper)
# TODO: Get rid of the indexing crap and use named lists
gen.cmp.networks <- function(basenets, commnet) {
  people <- which(is.element(rownames(basenets[[2]]), unique(basenets[[1]][,1])))
  interestnet <- shrink(basenets[[2]], by="row", keep=people, values="min")
  
  commnet <- fixup.network(commnet)
  interestnet <- fixup.network(interestnet)

  ## Remove any persons in interestnet that are not contained in commnet.
  ## This can happen if some wrote a single mail in a given time period
  ## (the person then has interests, but did not communicate with
  ## anyone). Especially, this can occur if a thread is cut off by temporal
  ## sub-span selection.
  ## TODO: In general, the complete approach to network selection seems clumsy
  ## and should be refactored.
  network.red <- commnet[is.element(rownames(commnet), rownames(interestnet)),
                         is.element(rownames(commnet), rownames(interestnet))]
  # Permute the communication network such that the order
  # of elements is identical to the interest network
  network.red <- permutation(network.red, rownames(interestnet))

  # TODO: Why are different packages used to compute the graph measures?
  # They should all be supported by SNA
  network.red.ig <- graph.adjacency(network.red, mode="directed")
  deg <- sna::degree(network.red, cmode="freeman", gmode="graph", ignore.eval=TRUE)
  betw <- igraph::betweenness(network.red.ig, directed=F)
  clo <- igraph::closeness(network.red.ig)
  cty.list <- list(deg, betw, clo)
  names(cty.list) <- c("Degree", "Betweenness", "Closeness")

  return(list(interestnet, network.red, cty.list))
}


### Initiating threads vs. replying. Adapted from the snatm repository ###
compute.initiate.respond <- function(forest, network.red, cty.list) {
  # The ansquest function is in severe need of renaming: An initial posting
  # need not necessarily be a question, but can be a bug report, a patch,
  # or whatever)
  initiate.respond <- ans.quest

  ir <- initiate.respond(forest)
  browser()
  deg <- cbind(rownames(network.red), cty.list[[1]])
  cent <- c()
  for (i in 1:nrow(deg)) {
    cent <- rbind(cent,
                  c(as.numeric(ir[deg[i,1]==ir[,1],2]),
                    as.numeric(ir[deg[i,1]==ir[,1],3])))
  }
  cent <- cbind(cent,as.numeric(deg[,2]))
  rownames(cent) <- deg[,1]
  colnames(cent) <- c("Responses", "Initiations", "deg")

  ## TODO: Why would we want to construct an outlier here?!
##  cent[dim(cent)[1],] <- c(max(cent[,1])+40, max(cent[,2])+100, 0)
  deg <- normalize(cent[,3])
  col <- rep("Low deg",dim(cent)[1])
  col[deg>0.3] <- "High deg"
#col[dim(cent)[1]] <- "transparent"

  return(data.frame(x=cent[,1], y=cent[,2], deg=deg, col=col))
}

gen.iter.intervals <- function(dates.cleaned, interval.length) {
  ## Given a list of dates and an interval length in weeks, construct
  ## pairs of date boundaries (start, end) that can be used to
  ## select all pieces of information within the iterval
  ## using the query start <= date(info) < end
  t.start <- ceiling_date(min(dates.cleaned), "week")
  num.intervals <- floor((max(dates.cleaned)-t.start)/dweeks(interval.length))
  if (num.intervals < 1) {
    ## Pathological case for repositories that contain less than one
    ## week
    num.intervals <- 1
  }
  
  boundaries <- t.start + c(0:num.intervals)*dweeks(interval.length)

  ## Transform the boundaries into a list of intervals. These can then be
  ## directly utilised by lapply, leading to an easy prey for parallelisation
  intervals.list <- lapply(1:(length(boundaries)-1),
                           function(i) new_interval(boundaries[i], boundaries[i+1]))

  return(intervals.list)
}





####### Fixes against upstream tm.plugin.mail ######
# NOTE: Original function from tm package does not handle corner case properly
# where In-Reply-To is contained twice in the message (patch has been sent
# upstream)
get.thread.id <- function(parentID, ht) {
  threadID <- NA
  threadLevel <- 2

  if (!identical(parentID, "") && length(parentID)!=0 && is.numeric(ht[[parentID]][1])) {
    threadID <- as.integer(ht[[parentID]][1])
  }

  if (!identical(parentID, "") && length(parentID)!=0 && is.numeric(ht[[parentID]][2])) {
    threadLevel <- as.integer(ht[[parentID]][2] + 1)
  }

  return(list(threadID=threadID, threadLevel=threadLevel))
}

threads <- function (x)
{
  ht <- new.env()
  tid <- 1
  threadIDs <- threadLevels <- integer(length(x))

  for (i in seq_along(x)) {
    messageID <- tm::ID(x[[i]])
    parentID <- gsub("In-Reply-To: ", "", grep("^In-Reply-To:",
                     attr(x[[i]], "Header"), value = TRUE, useBytes = TRUE))
    refID <- gsub("References: ", "", grep("^References:",
                     attr(x[[i]], "Header"), value = TRUE, useBytes = TRUE))
    refID <- sub(",$", "", refID)
    if (!length(parentID) & !length(refID)) {
      ht[[messageID]] <- c(tid, 1)
      threadIDs[i] <- tid
      threadLevels[i] <- 1
      tid <- tid + 1
    }
    else {
      info <- get.thread.id(refID, ht)
      if (!is.na(info$threadID)) {
        ht[[messageID]] <- c(info$threadID, info$threadLevel)
        threadIDs[i] <- info$threadID
        threadLevels[i] <- info$threadLevel
        next
      }

      parentID <- unique(parentID)
      if (length(parentID) > 1) {
        for (i in 1:length(parentID)) {
          info <- get.thread.id(parentID[[i]], ht)
          if (!is.na(info$ThreadID))
            next
        }
      } else {
        info <- get.thread.id(parentID, ht)
      }

      if (is.na(info$threadID)) {
        # The message is a reply to some other message, but the parent
        # eMail is not available -- for instance, it could be a follow-up
        # to a discussion from a separate mailing list. Create a new thread.
        ht[[messageID]] <- c(tid, 1)
        threadIDs[i] <- tid
        threadLevels[i] <- 1
        tid <- tid + 1
      }
      else {
        ht[[messageID]] <- c(info$threadID, info$threadLevel)
        threadIDs[i] <- info$threadID
        threadLevels[i] <- info$threadLevel
      }
    }
  }

  return(list(ThreadID = threadIDs, ThreadDepth = threadLevels))
}

removeMultipart <- function(x) UseMethod("removeMultipart", x)
removeMultipart.character <- function(x) {
    # http://en.wikipedia.org/wiki/Multipart_message#Multipart_Messages
    # We are only interested in text/plain parts
    i <- grep("^Content-Type: text/plain", x, useBytes = TRUE)
    r <- character(0)
    k <- 2
    for (j in i) {
        end <- if (k <= length(i)) i[k]-1 else length(x)
        content <- x[j:end]
        ## Find boundary (starting with "--")
        # In most cases the boundary is just one line before the Content-Type header
        start <- j - 1
        while (j > 0) {
            if (substr(x[j], 1, 2) == "--") {
                start <- j
                break
            }
            else
                j <- j - 1
        }
        index <- grep(x[start], content, useBytes = TRUE, fixed=TRUE)
        index <- if (length(index) == 0) length(content) else (index[1] - 1)
        content <- content[1:index]
        # Now remove remaining headers
        index <- grep("^$", content, useBytes = TRUE)
        index <- if (length(index) == 0) 1 else (index[1] + 1)
        r <- c(r, content[index:length(content)])
        k <- k + 1
    }

    if (length(r) == 0) x else r
}
removeMultipart.MailDocument <- function(x) {
    # tm::Content(x) <- ... does not work :-(
    Content(x) <- removeMultipart.character(tm::Content(x))
    x
}

removeCitation <- function(x, removeQuoteHeader) UseMethod("removeCitation", x)
removeCitation.character <- function(x, removeQuoteHeader=FALSE) {
    citations <- grep("^[[:blank:]]*>", x, useBytes = TRUE)
    if (removeQuoteHeader) {
      headers <- grep("wrote:$|writes:$", x)
      # A quotation header must immediately preceed a quoted part,
      # possibly with one empty line in between
      headers <- union(headers[(headers+1) %in% citations],
                       headers[(headers+2) %in% citations])
      citations <- union(headers, citations)
    }
    if (length(citations)) x[-citations] else x
}
removeCitation.MailDocument <- function(x, removeQuoteHeader=FALSE) {
    # tm::Content(x) <- ... does not work :-(
    Content(x) <- removeCitation.character(tm::Content(x), removeQuoteHeader)
    x
}
#############################
