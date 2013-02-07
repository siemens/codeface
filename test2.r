snatm.path <- "/home/wolfgang/projects/zk/QuantArch/src/snatm/pkg/R"

# Load the contents of snatm manually -- things are still in flux
library(tm.plugin.mail)
library(sna)
library(wordnet)
library(ggplot2)
# Read all files in a given directory (handy when packages are being developed)
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

# Since we modified snatm, we cannot load it via library()
sourceDir(snatm.path)
# Wordnet database can be downloaded from
# http://wordnet.princeton.edu/wordnet/download/current-version/
setDict("/home/wolfgang/projects/zk/QuantArch/wordnet/dict")

# NOTE: Original function from tm package does not handle corner case properly
# where In-Reply-To is contained twice in the message (patch has been sent
# upstream)
threads <- function (x) 
{
    ht <- new.env()
    tid <- 1
    threadIDs <- threadLevels <- integer(length(x))
    for (i in seq_along(x)) {
        messageID <- tm::ID(x[[i]])
        parentID <- gsub("In-Reply-To: ", "", grep("^In-Reply-To:", 
            attr(x[[i]], "Header"), value = TRUE, useBytes = TRUE))
        if (!length(parentID)) {
            ht[[messageID]] <- c(tid, 1)
            threadIDs[i] <- tid
            threadLevels[i] <- 1
            tid <- tid + 1
        }
        else {
            parentID <- unique(parentID)
            if (length(parentID) > 1) {
              warning("Multiple different Reply-To entries in message ", i, 
                      ". Using first Reply-To address")
              parentID <- parentID[[1]]
            }

          threadID <- if (identical(parentID, "") || !is.numeric(ht[[parentID]][1])) 
                NA
            else as.integer(ht[[parentID]][1])
            threadLevel <- if (identical(parentID, "") || !is.numeric(ht[[parentID]][2])) 
                2
            else as.integer(ht[[parentID]][2] + 1)
            ht[[messageID]] <- c(threadID, threadLevel)
            threadIDs[i] <- threadID
            threadLevels[i] <- threadLevel
        }
    }
    list(ThreadID = threadIDs, ThreadDepth = threadLevels)
  }

# as.one.file() is naturally not required when the complete archive is in one file already
# as.one.file(c(mbox.file), list="", dest="/home/wolfgang/projects/zk/QuantArch/nntp/allthremakad.txt")
repo.path <- "/home/wolfgang/projects/zk/QuantArch/nntp/repos/"
tmp <- makeforest(month=paste(repo.path, "gmane.os.freebsd.architechture", sep=""),
                  suffix=".mbox", marks=c("^_{10,}", "^-{10,}", "^[*]{10,}"))
forest <- tmp[[1]]
corp <- tmp[[2]]
#forest <- makeforest(month="/home/wolfgang/projects/zk/QuantArch/nntp/repos/gmane.os.openbsd.tech", suffix=".mbox")

#forest <- makeforest(month="/home/wolfgang/projects/zk/QuantArch/nntp/repos/gmane.linux.kernel", suffix=".mbox")
save(file="/media/disk/forest.linux-kernel.Rdata", forest)
load(file="/media/disk/forest.linux-kernel.Rdata")
#load(file="/media/disk/forest.openbsd.Rdata")

# Play with the corpus
workingobject <- tm::Corpus(DirSource("/home/wolfgang/projects/zk/QuantArch/nntp/repos/gmane.os.freebsd.architechture"), readerControl = list(reader = readMail(DateFormat = "%a, %d %b %Y %H:%M:%S")))

# Does not work
# peng <- sapply(workingobject, tm.plugin.mail::removeCitation)

# This is reasonably fast (6s/1000msg)
date(); for (i in seq_along(workingobject)) { workingobject[[i]] <- removeSignature(removeCitation(workingobject[[i]]), marks=c("^_{10,}", "^-{10,}", "^[*]{10,}")) }; date()
