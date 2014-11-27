library(tm)
library(lsa)

genArtifactCorpus <- function(con, project.id, start.date, end.date, entity.type) {
  depend.df <- query.dependency(con, project.id, entity.type, 30, start.date,
                                end.date, impl=TRUE, rmv.dups=TRUE)

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

  if(length(corp) > 0) {
    n_docs <- length(corp)
    max_sparsity <- 1 - (1 / n_docs)

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


testAnalysis <- function() {
  con <- connect.db("../../../codeface.conf")$con
  p.id <- 2
  start.date <- "2012-12-10"
  end.date <- "2013-12-12"

  corp <- genDependencyCorpus(con, p.id, start.date, end.date)
  corp <- processCorpus(corp)
  tdm <- processTermDocMat(corp)
  dist.mat <- computeDistMat(tdm)
  points <- plotMSA(dist.mat)
  browser()
}