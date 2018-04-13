## Tests on divideByGroupComparisons

library(fticRanalysis)
context("divideByGroupComparisons function")

test_that("basic tests on divideByGroupComparisons", {
  data("peakIcrProcessed")
  
  comparisons <- comparisonMatrix(icrObj = peakIcrProcessed, comparisons = "all")

  grpComp <- divideByGroupComparisons(peakIcrProcessed, comparisons)  
  
  expect_true(inherits(grpComp, "ddo"))
  expect_true(length(grpComp) == ncol(comparisons))
  
  i <- 3
  testSubset <- grpComp[[i]]$value
  expect_true(inherits(testSubset, "groupComparison"))
  
  grps <- comparisons[,i]
  samples <- as.character(dplyr::filter(getGroupDF(peakIcrProcessed), Group %in% grps)$SampleID)
  expect_true(all(samples %in% colnames(testSubset$e_data)))
  
  expect_true(all(samples %in% getGroupDF(testSubset)$SampleID))
  expect_true(all(getGroupDF(testSubset)$SampleID %in% samples))
  expect_true(all(grps %in% getGroupDF(testSubset)$Group))
  expect_true(all(getGroupDF(testSubset)$Group %in% grps))
  
})