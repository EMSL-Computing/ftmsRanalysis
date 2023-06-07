## Tests of concat function

library(ftmsRanalysis)
context("concat function")

test_that("tests of concat function on group DDOs", {
  
  data("exampleProcessedPeakData")
  exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Location")
  grpDdo1 <- divideByGroup(exampleProcessedPeakData)
  
  exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Crop.Flora")
  grpDdo2 <- divideByGroup(exampleProcessedPeakData)
  
  allGrpDdo <-concat(grpDdo1, grpDdo2)

  expect_equal(length(allGrpDdo), length(grpDdo1)+length(grpDdo2))
  expect_true(all(names(allGrpDdo) %in% c(names(grpDdo1), names(grpDdo2))))
  
  expect_true(all(class(allGrpDdo[[1]]) == class(grpDdo1[[1]])))
  
  grpSummaries <- summarizeGroups(allGrpDdo, c("n_present", "prop_present"))
  
  expect_equal(length(grpSummaries), length(allGrpDdo))
  expect_true(all(names(grpSummaries) %in% names(allGrpDdo)))
  
})

test_that("tests of concat function on group summary DDOs", {
  
  data("exampleProcessedPeakData")
  exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Location")
  grpDdo1 <- divideByGroup(exampleProcessedPeakData)
  grpDdo1 <- summarizeGroups(grpDdo1, summary_functions = c("n_present", "prop_present"))
  
  exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Crop.Flora")
  grpDdo2 <- divideByGroup(exampleProcessedPeakData)
  grpDdo2 <- summarizeGroups(grpDdo2, summary_functions = c("n_present", "prop_present"))
  
  allGrpDdo <-concat(grpDdo1, grpDdo2)
  
  expect_equal(length(allGrpDdo), length(grpDdo1)+length(grpDdo2))
  expect_true(all(names(allGrpDdo) %in% c(names(grpDdo1), names(grpDdo2))))
  
  expect_true(all(class(allGrpDdo[[1]]) == class(grpDdo1[[1]])))
  
})

test_that("tests of concat function on group comparison DDOs", {
  
  data("exampleProcessedPeakData")
  exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Location")
  grpDdo1 <- divideByGroupComparisons(exampleProcessedPeakData, "all")
  
  exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Crop.Flora")
  grpDdo2 <- divideByGroupComparisons(exampleProcessedPeakData, "all")
  
  allGrpDdo <-concat(grpDdo1, grpDdo2)
  
  expect_equal(length(allGrpDdo), length(grpDdo1)+length(grpDdo2))
  expect_true(all(names(allGrpDdo) %in% c(names(grpDdo1), names(grpDdo2))))
  
  expect_true(all(class(allGrpDdo[[1]]) == class(grpDdo1[[1]])))
  
  compSummaries <- summarizeGroupComparisons(allGrpDdo, summary_functions = "uniqueness_gtest",
                                        summary_function_params = list(uniqueness_gtest=
                                                                         list(pres_fn="prop",
                                                                              pres_thresh=0.5,
                                                                              pvalue_thresh=0.05)))
  
  expect_equal(length(compSummaries), length(allGrpDdo))
  expect_true(all(names(compSummaries) %in% names(allGrpDdo)))
  
})

test_that("tests of concat function on group comparison summary DDOs", {
  
  data("exampleProcessedPeakData")
  exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Location")
  grpDdo1 <- divideByGroupComparisons(exampleProcessedPeakData, "all")
  grpDdo1 <- summarizeGroupComparisons(grpDdo1, summary_functions = "uniqueness_gtest",
                                  summary_function_params = list(uniqueness_gtest=
                                                                   list(pres_fn="prop",
                                                                        pres_thresh=0.5,
                                                                        pvalue_thresh=0.05)))
  
  exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Crop.Flora")
  grpDdo2 <- divideByGroupComparisons(exampleProcessedPeakData, "all")
  grpDdo2 <- summarizeGroupComparisons(grpDdo2, summary_functions = "uniqueness_gtest",
                       summary_function_params = list(uniqueness_gtest=
                                                        list(pres_fn="prop",
                                                             pres_thresh=0.5,
                                                             pvalue_thresh=0.05)))
  
  allGrpDdo <-concat(grpDdo1, grpDdo2)
  
  expect_equal(length(allGrpDdo), length(grpDdo1)+length(grpDdo2))
  expect_true(all(names(allGrpDdo) %in% c(names(grpDdo1), names(grpDdo2))))
  
  expect_true(all(class(allGrpDdo[[1]]) == class(grpDdo1[[1]])))
  
})

# library(trelliscope)
# vdbConn("/Users/d3l348/tmp/testVdb",autoYes=TRUE)
# makeDisplay(data=allGrpDdo,
#             name="test",
#             panelFn=panelFunctionGenerator("vanKrevelenPlot",
#                                            colorCName="uniqueness_gtest")
#             )
# view()
