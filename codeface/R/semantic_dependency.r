library(tm)
library(lsa)


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
  td.mat <- as.matrix(tdm)

  ## Perform local and global document weighting
  td.mat.w <- lw_bintf(td.mat) * gw_idf(td.mat)
  latent.space <- lsa(td.mat.w)

  ## Compute document simlarity using cosine similarity
  similarity.mat <- cosine(diag(latent.space$sk) %*% t(latent.space$dk))

  return(similarity.mat)
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


computeSemanticCoupling <- function(depend.df, threshold=0.7) {
  ## Remove entity duplicates
  depend.df <- depend.df[!duplicated(depend.df[c("entity")]), ]

  if(nrow(depend.df) < 2) {
    return(list(edgelist=data.frame(), vertex.data=data.frame()))
  }

  ## Generate corpus of artifacts
  corp <- genArtifactCorpus(depend.df)

  ## Process corpus using stop word removal, stemming, and some software related
  ## conventions like splitting of camelcase terms
  corp <- processCorpus(corp)
  tdm <- processTermDocMat(corp)

  ## Compute document similarity using latent semantic analysis
  similarity.mat <- computeDocSimilarity(tdm)

  ## Remove documents that have low similarity
  high.similarity.relations <- which(similarity.mat >= threshold, arr.ind=TRUE,
                                     useNames=FALSE)
  edgelist.df <- data.frame(high.similarity.relations)

  ## Mapping of document ids to document names
  vertex.data <- data.frame(name=unlist(meta(corp, tag="id")),
                            id=unlist(meta(corp, tag="heading")),
                            stringsAsFactors=FALSE)

  res <- list(edgelist=edgelist.df, vertex.data=vertex.data)

  return(res)
}