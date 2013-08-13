library(testthat)
source("utils.r")

test_that("tstamp.to.POSIXct works as expected", {
    expect_that(tstamp.to.POSIXct(42), is_a("POSIXct"))
})


test_that("scale.data returns expected values", {
    expect_that(scale.data(c(-100, 100, 0)), equals(c(0,1,0.5)))
    expect_that(scale.data(c(42,42,42)), equals(c(1,1,1)))
    expect_that(scale.data(c(2)), equals(c(1)))
})

test_that("gen.weighted.edgelist works"
