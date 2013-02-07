setClass("testClass",
         representation=
         representation(kw.exclude="character"))

setGeneric(name="add.kw", def=function(object, kw.lst) standardGeneric("add.kw"))
setMethod("add.kw", signature=signature(object="testClass"),
          function(object, kw.lst) {
            object@kw.exclude <- unique(c(object@kw.exclude, kw.lst))
            cat("Length after adding: ", length(object@kw.exclude), "\n")
            return(object)
          })

setGeneric(name="show", def=function(obj) standardGeneric("show"))
setMethod("show", signature=signature(obj="testClass"),
          function(obj) {
            cat ("Class contains", length(obj@kw.exclude), "exclusion keywords.\n")
          })

setGeneric(name="show.all", def=function(obj) standardGeneric("show.all"))
setMethod("show.all", signature=signature(obj="testClass"),
          function(obj) {
            cat ("Class contains", length(obj@kw.exclude), "exclusion keywords:\n")
            for (i in seq_along(obj@kw.exclude)) {
              cat("  -> ", obj@kw.exclude[[i]], "\n")
            }
          })

setGeneric("kw.exclude<-", function(obj, value) { standardGeneric("kw.exclude<-") })
setReplaceMethod("kw.exclude", signature(obj="testClass"),
                 function(obj, value) {
                   slot(obj, "kw.exclude") <-
                     unique(c(value, slot(obj, "kw.exclude")))
#                   validObject(object)
                   show.all(obj)
                   return(obj)
                 })

setClass("Sequences",
         representation=representation(
           sequences="character"))

setValidity("Sequences", function(object) {
  msg <- NULL
  atgc <- grep("[^atcg]", sequences(object))
  if (length(atgc)>0)
    msg <- c(msg, "'sequences' must be a, t, c, or g")
  if (is.null(msg)) TRUE
  else msg
})

setClass("Sequences",
         representation=representation(
           sequences="character"))

setClass("DNASequences",
         contains="Sequences",
         representation=representation(
           chromosome="character"))

setGeneric("sequences",
           function(object) {
             standardGeneric("sequences")
           })
setMethod("sequences",
          signature(object="Sequences"),
          function(object) {
            slot(object, "sequences")
          })

setGeneric("sequences<-",
           function(object, value) {
             standardGeneric("sequences<-")
           })
setReplaceMethod("sequences",
                 signature(object="Sequences"),
                 function(object, value) {
                   slot(object, "sequences") <-
                     unique(c(sequences(object), "hallo", value))
                   validObject(object)
                   object
                 })

test <- new("Sequences")
sequences(test) <- "acacac"
sequences(test)
