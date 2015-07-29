library(testthat)

source("analysis.r")

conf <- list(listname="gmane.comp.emulators.qemu")
path <- "test_data"
res.dir <- "test_data"

test.genforest <- function() {
  corp <- gen.forest(conf, path, res.dir)
  file.remove(file.path(res.dir, paste("corp.base", conf$listname, sep=".")))
  check.eq <- unlist(meta(corp$corp.orig, tag="datetimestamp")) == unlist(meta(corp$corp, tag="datetimestamp"))  
  return(all(check.eq))
}

test.check.corpus.precon <- function() {
  corp <- gen.forest(conf, path, res.dir)
  file.remove(file.path(res.dir, paste("corp.base", conf$listname, sep=".")))
  corp <- check.corpus.precon(corp)
  check.eq <- unlist(meta(corp$corp.orig, tag="datetimestamp")) == unlist(meta(corp$corp, tag="datetimestamp"))
  return(all(check.eq))
}

test_that("Forest generation functions correctly", {
            expect_true(test.genforest())
          })

test_that("Corpus precondiction checks works", {
            expect_true(test.check.corpus.precon())
          })