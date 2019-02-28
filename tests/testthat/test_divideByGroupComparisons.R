## Tests on divideByGroupComparisons

library(fticRanalysis)
context("divideByGroupComparisons function")

test_that("basic tests on divideByGroupComparisons with all comparisons", {
  data("exampleProcessedPeakData")
  
  comparisons <- fticRanalysis:::comparisonMatrix(exampleProcessedPeakData, "all")
  
  grpComp <- divideByGroupComparisons(exampleProcessedPeakData, comparisons="all")  
  
  expect_true(inherits(grpComp, "ddo"))
  expect_true(length(grpComp) == ncol(comparisons))
  
  i <- 3
  testSubset <- grpComp[[i]]$value
  expect_true(inherits(testSubset, "groupComparison"))
  
  grps <- comparisons[,i]
  samples <- as.character(dplyr::filter(getGroupDF(exampleProcessedPeakData), Group %in% grps)$SampleID)
  expect_true(all(samples %in% colnames(testSubset$e_data)))
  
  expect_true(all(samples %in% getGroupDF(testSubset)$SampleID))
  expect_true(all(getGroupDF(testSubset)$SampleID %in% samples))
  expect_true(all(grps %in% getGroupDF(testSubset)$Group))
  expect_true(all(getGroupDF(testSubset)$Group %in% grps))
  
})

test_that("divideByGroupComparisons works without a groupDF", {
  data("examplePeakData")
  
  comparisons <- fticRanalysis:::comparisonMatrix(examplePeakData, comparisons="control", control="EM0061_sample")
  
  grpComp <- divideByGroupComparisons(examplePeakData, comparisons="control", control="EM0061_sample")  
  
  expect_true(inherits(grpComp, "ddo"))
  expect_true(length(grpComp) == ncol(comparisons))
  
  n_samples <- length(unique(examplePeakData$f_data[, getFDataColName(examplePeakData)]))
  expect_true(length(grpComp) == n_samples-1)
  
  i <- 5
  testSubset <- grpComp[[i]]$value
  expect_true(inherits(testSubset, "groupComparison"))
  
  grps <- comparisons[,i]
  samples <- grps
  expect_true(all(samples %in% colnames(testSubset$e_data)))
  
  expect_true(all(samples %in% getGroupDF(testSubset)$SampleID))
  expect_true(all(getGroupDF(testSubset)$SampleID %in% samples))
  expect_true(all(grps %in% getGroupDF(testSubset)$Group))
  expect_true(all(getGroupDF(testSubset)$Group %in% grps))
  
})


test_that("divideByGroupComparisons with one-factor comparisons", {
  data("exampleProcessedPeakData")
  
  comparisons <- fticRanalysis:::comparisonMatrix(exampleProcessedPeakData, "one-factor")
  
  grpComp <- divideByGroupComparisons(exampleProcessedPeakData, comparisons="one-factor")  
  
  expect_true(inherits(grpComp, "ddo"))
  expect_true(length(grpComp) == ncol(comparisons))
  
  i <- 3
  testSubset <- grpComp[[i]]$value
  expect_true(inherits(testSubset, "groupComparison"))
  
  grps <- comparisons[,i]
  samples <- as.character(dplyr::filter(getGroupDF(exampleProcessedPeakData), Group %in% grps)$SampleID)
  expect_true(all(samples %in% colnames(testSubset$e_data)))
  
  expect_true(all(samples %in% getGroupDF(testSubset)$SampleID))
  expect_true(all(getGroupDF(testSubset)$SampleID %in% samples))
  expect_true(all(grps %in% getGroupDF(testSubset)$Group))
  expect_true(all(getGroupDF(testSubset)$Group %in% grps))
  
})
