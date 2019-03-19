## Tests on summarizeGroupComparisons

library(ftmsRanalysis)
context("summarizeGroupComparisons function")

test_that("test of summarizeGroupComparisons on a groupComparison object", {
  data("exampleProcessedPeakData")
  
  grpComp <- divideByGroupComparisons(exampleProcessedPeakData, comparisons = "all")[[1]]$value

  grpCompSummary <- summarizeGroupComparisons(grpComp, summary_functions="uniqueness_gtest", 
                                         summary_function_params=list(
                                           uniqueness_gtest=list(pres_fn="nsamps", pres_thresh=2, pvalue_thresh=0.05)
                                         ))
  
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

test_that("test of summarizeGroupComparisons on a ddo", {
  data("exampleProcessedPeakData")
  
  grpComp <- divideByGroupComparisons(exampleProcessedPeakData, comparisons = "one-factor")
  
  grpCompSummary <- summarizeGroupComparisons(grpComp, summary_functions="uniqueness_gtest", 
                                         summary_function_params=list(
                                           uniqueness_gtest=list(pres_fn="nsamps", pres_thresh=2, pvalue_thresh=0.05)
                                         ))
  
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
  
test_that("test of summarizeGroupComparisons with multiple summary functions on a groupComparison object", {
  data("exampleProcessedPeakData")
  
  grpComp <- divideByGroupComparisons(exampleProcessedPeakData, comparisons = "all")[[1]]$value
  
  grpCompSummary <- summarizeGroupComparisons(grpComp, summary_functions=c("uniqueness_gtest", "uniqueness_nsamps", "uniqueness_prop"), 
                                         summary_function_params=list(
                                           uniqueness_gtest=list(pres_fn="nsamps", pres_thresh=2, pvalue_thresh=0.05),
                                           uniqueness_nsamps=list(pres_thresh=3, absn_thresh=1),
                                           uniqueness_prop=list(pres_thresh=0.5, absn_thresh=0.2)
                                         ))

  expect_true(inherits(grpCompSummary, "comparisonSummary"))
  expect_false(inherits(grpCompSummary, "groupComparison"))
  expect_equal(ncol(grpCompSummary$e_data), 4)
  expect_true(getEDataColName(grpComp) %in% colnames(grpCompSummary$e_data))
  expect_true(all(dim(grpComp$e_meta) == dim(grpCompSummary$e_meta)))
  expect_true(all(colnames(grpComp$e_meta) %in% colnames(grpCompSummary$e_meta)))
  expect_true(all(c("uniqueness_gtest", "uniqueness_nsamps", "uniqueness_prop") %in% 
                    grpCompSummary$f_data$Summary_Function_Name))
  expect_equal(nrow(grpCompSummary$f_data), 3)
  expect_true(all(unlist(lapply(grpCompSummary$e_data[, 2], function(x) is.factor(x)))))
  
})
