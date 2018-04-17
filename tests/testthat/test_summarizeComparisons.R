## Tests on summarizeComparisons

library(fticRanalysis)
context("summarizeComparisons function")

test_that("test of summarizeComparisons on a groupComparison object", {
  data("peakIcrProcessed")
  
  comparisons <- comparisonMatrix(icrObj = peakIcrProcessed, comparisons = "all")
  
  grpComp <- divideByGroupComparisons(peakIcrProcessed, comparisons)[[1]]$value
  groups <- as.character(unique(getGroupDF(grpComp)$Group))
  
  grpCompSummary <- summarizeComparisons(grpComp, "n_present")
  
  expect_true(inherits(grpCompSummary, "comparisonSummary"))
  expect_false(inherits(grpCompSummary, "groupComparison"))
  expect_equal(ncol(grpCompSummary$e_data), 3)
  expect_true(all(groups %in% colnames(grpCompSummary$e_data)))
  expect_true(getEDataColName(grpComp) %in% colnames(grpCompSummary$e_data))
  expect_true(all(dim(grpComp$e_meta) == dim(grpCompSummary$e_meta)))
  expect_true(all(colnames(grpComp$e_meta) %in% colnames(grpCompSummary$e_meta)))
  expect_equal(nrow(grpCompSummary$f_data), length(groups))
  expect_true(all(groups %in% grpCompSummary$f_data$Group))
  expect_true(all(unlist(lapply(grpCompSummary$e_data[, groups], function(x) is.numeric(x)))))
  
})

test_that("test of summarizeComparisons on a ddo", {
  data("peakIcrProcessed")
  
  comparisons <- comparisonMatrix(icrObj = peakIcrProcessed, comparisons = "all")
  
  grpComp <- divideByGroupComparisons(peakIcrProcessed, comparisons)
  
  grpCompSummary <- summarizeComparisons(grpComp, "n_present")
  
  expect_true(inherits(grpCompSummary, "ddo"))
  expect_equal(length(grpCompSummary), length(grpComp))
  
  # test one subset
  i <- 3
  val <- grpCompSummary[[i]]$value
  groups <- as.character(unique(getGroupDF(grpComp[[i]]$value)$Group))
    
  expect_true(inherits(val, "comparisonSummary"))
  expect_false(inherits(val, "groupComparison"))
  expect_equal(ncol(val$e_data), 3)
  expect_true(all(groups %in% colnames(val$e_data)))
  expect_true(getEDataColName(grpComp[[i]]$value) %in% colnames(val$e_data))
  expect_true(all(dim(grpComp[[i]]$value$e_meta) == dim(val$e_meta)))
  expect_true(all(colnames(grpComp[[i]]$value$e_meta) %in% colnames(val$e_meta)))
  expect_equal(nrow(val$f_data), length(groups))
  expect_true(all(groups %in% val$f_data$Group))
  expect_true(all(unlist(lapply(val$e_data[, groups], function(x) is.numeric(x)))))
  
})
  