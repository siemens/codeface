library(testthat)
source("developer_classification.r")

get.developer.class.test <- function() {
  threshold <- 0.8
  sample.size <- 1000

  commit.df <- data.frame(author=sample(1:50, size=sample.size, replace=T))
  developer.class <- get.developer.class(commit.df, threshold)

  res <- sum(developer.class$core$freq) < threshold*sample.size
  return(res)
}

test_that("get.developer.class returns expected values", {
      expect_true(get.developer.class.test())
    })
