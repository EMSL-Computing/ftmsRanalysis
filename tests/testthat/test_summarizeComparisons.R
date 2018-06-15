## Tests on summarizeComparisons

library(fticRanalysis)
context("summarizeComparisons function")

test_that("test of summarizeComparisons on a groupComparison object", {
  data("peakIcrProcessed")
  
  grpComp <- divideByGroupComparisons(peakIcrProcessed, comparisons = "all")[[1]]$value

  grpCompSummary <- summarizeComparisons(grpComp, "uniqueness_gtest")
  
  expect_true(inherits(grpCompSummary, "comparisonSummary"))
  expect_false(inherits(grpCompSummary, "groupComparison"))
  expect_equal(ncol(grpCompSummary$e_data), 2)
  expect_true(getEDataColName(grpComp) %in% colnames(grpCompSummary$e_data))
  expect_true(all(dim(grpComp$e_meta) == dim(grpCompSummary$e_meta)))
  expect_true(all(colnames(grpComp$e_meta) %in% colnames(grpCompSummary$e_meta)))
  expect_true(all(grpCompSummary$f_data$Summary_Function_Name == "uniqueness_gtest"))
  expect_equal(nrow(grpCompSummary$f_data), 1)
  expect_true(all(unlist(lapply(grpCompSummary$e_data[, 2], function(x) is.factor(x)))))
  
})

test_that("test of summarizeComparisons on a ddo", {
  data("peakIcrProcessed")
  
  grpComp <- divideByGroupComparisons(peakIcrProcessed, comparisons = "one-factor")
  
  grpCompSummary <- summarizeComparisons(grpComp, "uniqueness_gtest")
  
  expect_true(inherits(grpCompSummary, "ddo"))
  expect_equal(length(grpCompSummary), length(grpComp))
  
  # test one subset
  i <- 3
  val <- grpCompSummary[[i]]$value
  expect_true(inherits(val, "comparisonSummary"))
  expect_false(inherits(val, "groupComparison"))
  expect_equal(ncol(val$e_data), 2)
  expect_true(getEDataColName(grpComp[[i]]$value) %in% colnames(val$e_data))
  expect_true(all(dim(grpComp[[i]]$value$e_meta) == dim(val$e_meta)))
  expect_true(all(colnames(grpComp[[i]]$value$e_meta) %in% colnames(val$e_meta)))
  expect_equal(nrow(val$f_data), 1)
  expect_true(all(grpCompSummary$f_data$Summary_Function_Name == "uniqueness_gtest"))
  expect_equal(nrow(val$f_data), 1)
  expect_true(all(unlist(lapply(val$e_data[, 2], function(x) is.factor(x)))))
  
})

## TODO: test with more than one comparison summary function (currently there's only one option)
  