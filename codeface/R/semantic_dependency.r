library(tm)
library(lsa)
library(compiler)

genArtifactCorpus <- function(depend.df) {
  myReader <- readTabular(mapping=list(content="impl", heading="entity"))
  corp <- VCorpus(DataframeSource(depend.df), readerControl=list(reader=myReader))

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

  ct <- content_transformer(removeSoft)
  corp_mod <- tm_map(corp, ct)

  ## Remove stop words
  corp_mod <- tm_map(corp_mod, removeWords, stopwords("english"))

  ## Stem words
  corp_mod <- tm_map(corp_mod, stemDocument)

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
  ## Matrix is symetric so we don't need to make all comparisons
  edgelist <- which(dist.mat >= threshold, arr.ind=TRUE,
                    useNames=FALSE)

  ## Remove the upper diagonal including diagonal indices
  edgelist <- edgelist[edgelist[ ,1] > edgelist[ ,2],]

  return(data.frame(edgelist))
}


remapIndx <- function (m, z, diag = FALSE) {
    m.dim <- ncol(m)
    index1 <- ((z-1) %/% m.dim) + 1
    index2 <- ((z-1) %% m.dim) + 1

    return(c(index1, index2))
}

computeSemanticCoupling <- function(depend.df, threshold=0.5) {
  ## Remove entity duplicates
  depend.df <- depend.df[!duplicated(depend.df[c("entity")]), ]

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
                            id=unlist(meta(corp, tag="heading")),
                            stringsAsFactors=FALSE)

  res <- list(edgelist=edgelist, vertex.data=vertex.data)

  return(res)
}
