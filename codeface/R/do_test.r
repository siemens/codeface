#! /usr/bin/env Rscript
## Helper scripts to dispatch and evaluate R unit/integration tests

suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(stringr))

do.tests <- function(dir) {
    res <- test_dir(dir);
    if (length(res$failures) > 0) {
        cat(str_c("Some R tests failed for directory '", dir, "'"))

        for (i in 1:length(res$failures)) {
            cat(str_c("Failing test ", i, ": ", res$failures[[i]], "\n"))
        }

        return(FALSE)
    }
    return(TRUE)
}

res <- sapply(c("./", "cluster/"), function(dir) {
    do.tests(dir)
})

if (!all(res)) {
    stop("Error exiting because of test failures")
}
