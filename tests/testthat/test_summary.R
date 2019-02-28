## Basic summary methods

library(fticRanalysis)
context("summary method")

test_that("summary.icrData method", {

  data("examplePeakData")  
  icrSumm <- summary(examplePeakData)  
  expect_true(inherits(icrSumm, "icrDataSummary"))
  expect_true(inherits(icrSumm, "list"))
  expect_equal(object = length(icrSumm), expected = 3)  
  expect_true(all(c("Samples", "Molecules", "Percent_Missing") %in% names(icrSumm)))

})