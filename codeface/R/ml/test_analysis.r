library(testthat)

source("../db.r", chdir=T)
source("analysis.r")

conf <- connect.db("../../../codeface_testing.conf")
conf$listname <- "gmane.comp.emulators.qemu"

path <- "test_data"
res.dir <- "test_data"

fixed.timestamps = list(
  "<20150129233351.GD32706@tesla.redhat.com>" = strptime("2015-01-01 23:30:51 GMT", format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
  "<54CAC5C3.4080706@redhat.com>" = strptime("2015-01-29 23:44:03 GMT", format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
  "<201501ef29233351.GD32706@tesla.redhat.com>" = strptime("2015-02-23 23:44:03 GMT", format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
  "<54CAC5C3.4080745506@redhat.com>" = strptime("2015-01-30 23:44:03 GMT", format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
)

fixed.timestamps.offsets = list(
  "<20150129233351.GD32706@tesla.redhat.com>" = 100,
  "<54CAC5C3.4080706@redhat.com>" = -700,
  "<201501ef29233351.GD32706@tesla.redhat.com>" = -700,
  "<54CAC5C3.4080745506@redhat.com>" = -700
)

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
  check.eq <- unlist(meta(corp$corp, tag="datetimestamp")) == unlist(fixed.timestamps)
  check.eq.offsets <- unlist(meta(corp$corp, tag="datetimestampOffset")) == unlist(fixed.timestamps.offsets)
  return(all(check.eq, check.eq.offsets))
}

test.global.analysis <- function () {
  project.name <- "test_mail"
  analysis.method <- "none"
  start.date <- "2000-01-01"
  end.date <- "2020-01-01"

  conf$pid <- gen.clear.project.id.con(conf$con, project.name, analysis.method)
  start.id <- gen.revision.id(conf, "v1.0", start.date)
  end.id <- gen.revision.id(conf, "v1.1", end.date)
  range <- gen.range.id(conf, start.id, end.id)

  ## Run analysis
  dispatch.all(conf, path, res.dir)

  ## Query for edgelist
  edgelist <- query.mail.edgelist(conf$con, conf$pid, start.date, end.date)

  ## Get author id to name mapping
  id.to.name <- sapply(unique(unlist(edgelist[,c(1,2)])),
                        function(id) {
                            name = query.person.name(conf$con, id)
                            attr(name, "names") = id
                            return(name)
                          }
                       )
  ## The edge list is composed of character type global person ids and
  ## this will return a named vector to map the global ids to person names
  edgelist[,c(1,2)] <- sapply(edgelist[,c(1,2)], function(id) id.to.name[as.character(id)])

  ## Generate graph from database data
  g.db <- graph.data.frame(edgelist)

  ## Generate target graph that corresponds to the manipulated mbox test file
  edgelist.target <- data.frame(from=c("chatty kathy", "nasty nate"),
                                to=c("bossy bill", "sneaky sam"),
                                weight=1)
  g.target <- graph.data.frame(edgelist.target)

  ## Test for edge agreement
  res <- all(E(g.target) == E(g.db))
}

test_that("Forest generation functions correctly", {
            expect_true(test.genforest())
          })

test_that("Corpus precondiction checks works", {
            expect_true(test.check.corpus.precon())
          })

test_that("Global mail analysis returns correct network", {
           expect_true(test.global.analysis())
          })
