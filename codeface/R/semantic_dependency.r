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
## Copyright 2016 by Siemens AG, Mitchell Joblin <mitchell.joblin.ext@siemens.com>
## All Rights Reserved.

s <- suppressPackageStartupMessages
s(library(tm))
s(library(lsa))
s(library(compiler))

genArtifactCorpus <- function(depend.df) {
    depend.df$impl <- str_replace_all(depend.df$impl,
                                      "(\\.|\\{|\\}|\\(|\\)|;|:|\\[|\\]|\n|\r)", " ")

    colnames(depend.df) <- str_replace(colnames(depend.df), "^impl$", "text")
    colnames(depend.df) <- str_replace(colnames(depend.df), "^entity$", "doc_id")

    corp <- VCorpus(DataframeSource(depend.df))

    return(corp)
}


processCorpus <- function(corp) {
  ## Handle factors that are specific to
  ## source code
  camel_1 <- "(.)([A-Z][a-z]+)"
  camel_2 <- "([a-z0-9])([A-Z])"
  removeSoft <- function(x) {
                   ## split CamelCase
                   s1 <- gsub(camel_1,"\\1_\\2",x,perl=T)
                   s2 <- gsub(camel_2,"\\1_\\2",s1,perl=T)
                   s3 <- gsub("__", "_",s2)
                   ## Replace underscore with space
                   s4 <- gsub("_"," ",s3)
                   ## Repalce non-alphanumeric with space
                   s5 <- gsub("[^[:alnum:] ]", " ", s4)
                   return(s5)}

  funs <- list(stripWhitespace, # Remove excess whitespace
               stemDocument,
               function(x) removeWords(x, stopwords("english")), # Remove stop words,
               content_transformer(removeSoft)) # Handle prog lang specifics)

  corp_mod <- tm_map(corp, FUN=tm_reduce, tmFuns=funs)

  return(corp_mod)
}


processTermDocMat <- function(corp) {
  ## create term document matrix
  tdm <- TermDocumentMatrix(corp)

  if(length(corp) > 1) {
    n_docs <- tdm$ncol
    max_sparsity <- min(0.95 ,1 - (1 / n_docs))

    ## remove terms occuring very infrequently
    tdm <- removeSparseTerms(tdm, max_sparsity)
  }

  return(tdm)
}


computeDocSimilarity <- function(tdm) {
  ## Perform local and global document weighting
  ## Use local binary weighting and global inverse document frequency
  td.mat.w <- as.matrix(weightSMART(tdm, spec="btn"))

  ## Compute low rank approximation to term document matrix
  latent.space <- lsa(td.mat.w)

  ## Compute document simlarity using cosine similarity
  cmpCosine <- cmpfun(cosineVectorized)
  similarity.mat <- cmpCosine(diag(latent.space$sk) %*% t(latent.space$dk))

  ## After document weighting, some documents have no elements and
  ## the similarity computation results in nan
  similarity.mat[is.nan(similarity.mat)] <- 0

  return(similarity.mat)
}


cosineVectorized <- function(x){
  crossprod(x) / sqrt(tcrossprod(apply(x, 2, crossprod)))
}


plotMSA <- function(dist.mat){
  fit <- cmdscale(dist.mat, eig=T, k=2)
  points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
  qplot(x, y, data=points, geom="point", alpha=I(1/5))

  return(points)
}

## High level functions to identify semantic relationships between two source code
## artifacts, the artifact can be a various grainularities (e.g., file, function, feature)
computeSemanticCouplingCon <- function(con, project.id, start.date, end.date,
                                       entity.type="Function") {
  ## Retrieve entities and the implementation from the database
  depend.df <- query.dependency(con, project.id, entity.type, 30, start.date,
                                end.date, impl=TRUE, rmv.dups=TRUE)

  ## Compute semantic coupling between entities
  res <- computeSemanticCoupling(depend.df)

  return(res)
}


getSimilarDocIds <- function(dist.mat, threshold) {
  dist.mat[is.nan(dist.mat)] <- 0
  edgelist <- matrix(ncol=2,nrow=0)

  if (nrow(dist.mat) < 2) return(data.frame())

  for (i in 2:ncol(dist.mat)) {
    for (j in 1:(i-1)) {
      if (dist.mat[i,j] >= threshold) {
        edgelist <- rbind(edgelist, c(i,j))
       }
    }
  }

  return(data.frame(edgelist))
}


getSimDocIds <- function(dist.mat, threshold) {
  ## Generate lower-tri index
  num.rows <- nrow(dist.mat)
  row.seq <- seq(num.rows)
  edgelist <- cbind(
    X1=unlist(lapply(2:num.rows, function(x) x:num.rows), use.names=FALSE),
    X2=rep(row.seq[-length(row.seq)], times=rev(tail(row.seq,-1))-1))

  values <- dist.mat[edgelist]

  ## Return document pairs that exceed similarity threshold
  keep <- values >= threshold
  edgelist <- edgelist[keep, ]

  return(data.frame(edgelist))
}


remapIndx <- function (m, z, diag = FALSE) {
    m.dim <- ncol(m)
    index1 <- ((z-1) %/% m.dim) + 1
    index2 <- ((z-1) %% m.dim) + 1

    return(c(index1, index2))
}

computeSemanticCoupling <- function(depend.df, threshold=0.5) {
  if (nrow(depend.df) > 0) {
    ## Remove entity duplicates
    depend.df <- depend.df[!duplicated(depend.df[c("entity")]), ]
  }

  if(nrow(depend.df) < 2) {
    return(list(edgelist=data.frame(), vertex.data=data.frame()))
  }

  loginfo(sprintf("Computing semantic simlarity for %i entities",
                   nrow(depend.df)), logger="")

  ## Generate corpus of artifacts
  corp <- genArtifactCorpus(depend.df)

  ## Process corpus using stop word removal, stemming, and some software related
  ## conventions like splitting of camelcase terms
  corp <- processCorpus(corp)
  tdm <- processTermDocMat(corp)

  ## Compute document similarity using latent semantic analysis
  dist.mat <- computeDocSimilarity(tdm)

  ## Remove documents that have low similarity
  edgelist <- cmpfun(getSimDocIds)(dist.mat, threshold)

  ## Mapping of document ids to document names
  vertex.data <- data.frame(name=unlist(meta(corp, tag="id")),
                            id=unlist(names(corp)),
                            stringsAsFactors=FALSE)

  res <- list(edgelist=edgelist, vertex.data=vertex.data)

  return(res)
}
