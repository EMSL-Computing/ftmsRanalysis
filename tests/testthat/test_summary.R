## Basic summary methods

library(ftmsRanalysis)
context("summary method")

test_that("summary.ftmsData method", {

  data("examplePeakData")  
  summObj <- summary(examplePeakData)  
  expect_true(inherits(summObj, "ftmsDataSummary"))
  expect_true(inherits(summObj, "list"))
  expect_equal(object = length(summObj), expected = 3)  
  expect_true(all(c("Samples", "Molecules", "Percent_Missing") %in% names(summObj)))

})