## Tests of concat function

library(fticRanalysis)
context("concat function")

test_that("tests of concat function on group DDOs", {
  
  data("peakIcrProcessed")
  peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Location")
  grpDdo1 <- divideByGroup(peakIcrProcessed)
  
  peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Crop.Flora")
  grpDdo2 <- divideByGroup(peakIcrProcessed)
  
  allGrpDdo <-concat(grpDdo1, grpDdo2)

  expect_equal(length(allGrpDdo), length(grpDdo1)+length(grpDdo2))
  expect_true(all(getKeys(allGrpDdo) %in% c(getKeys(grpDdo1), getKeys(grpDdo2))))
  
  expect_true(all(class(allGrpDdo[[1]]$value) == class(grpDdo1[[1]]$value)))
  
  grpSummaries <- summarizeGroups(allGrpDdo, c("n_present", "prop_present"))
  
  expect_equal(length(grpSummaries), length(allGrpDdo))
  expect_true(all(getKeys(grpSummaries) %in% getKeys(allGrpDdo)))
  
})

test_that("tests of concat function on group summary DDOs", {
  
  data("peakIcrProcessed")
  peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Location")
  grpDdo1 <- divideByGroup(peakIcrProcessed)
  grpDdo1 <- summarizeGroups(grpDdo1, summary_functions = c("n_present", "prop_present"))
  
  peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Crop.Flora")
  grpDdo2 <- divideByGroup(peakIcrProcessed)
  grpDdo2 <- summarizeGroups(grpDdo2, summary_functions = c("n_present", "prop_present"))
  
  allGrpDdo <-concat(grpDdo1, grpDdo2)
  
  expect_equal(length(allGrpDdo), length(grpDdo1)+length(grpDdo2))
  expect_true(all(getKeys(allGrpDdo) %in% c(getKeys(grpDdo1), getKeys(grpDdo2))))
  
  expect_true(all(class(allGrpDdo[[1]]$value) == class(grpDdo1[[1]]$value)))
  
})

test_that("tests of concat function on group comparison DDOs", {
  
  data("peakIcrProcessed")
  peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Location")
  grpDdo1 <- divideByGroupComparisons(peakIcrProcessed, "all")
  
  peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Crop.Flora")
  grpDdo2 <- divideByGroupComparisons(peakIcrProcessed, "all")
  
  allGrpDdo <-concat(grpDdo1, grpDdo2)
  
  expect_equal(length(allGrpDdo), length(grpDdo1)+length(grpDdo2))
  expect_true(all(getKeys(allGrpDdo) %in% c(getKeys(grpDdo1), getKeys(grpDdo2))))
  
  expect_true(all(class(allGrpDdo[[1]]$value) == class(grpDdo1[[1]]$value)))
  
  compSummaries <- summarizeGroupComparisons(allGrpDdo, summary_functions = "uniqueness_gtest",
                                        summary_function_params = list(uniqueness_gtest=
                                                                         list(pres_fn="prop",
                                                                              pres_thresh=0.5,
                                                                              pvalue_thresh=0.05)))
  
  expect_equal(length(compSummaries), length(allGrpDdo))
  expect_true(all(getKeys(compSummaries) %in% getKeys(allGrpDdo)))
  
})

test_that("tests of concat function on group comparison summary DDOs", {
  
  data("peakIcrProcessed")
  peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Location")
  grpDdo1 <- divideByGroupComparisons(peakIcrProcessed, "all")
  grpDdo1 <- summarizeGroupComparisons(grpDdo1, summary_functions = "uniqueness_gtest",
                                  summary_function_params = list(uniqueness_gtest=
                                                                   list(pres_fn="prop",
                                                                        pres_thresh=0.5,
                                                                        pvalue_thresh=0.05)))
  
  peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Crop.Flora")
  grpDdo2 <- divideByGroupComparisons(peakIcrProcessed, "all")
  grpDdo2 <- summarizeGroupComparisons(grpDdo2, summary_functions = "uniqueness_gtest",
                       summary_function_params = list(uniqueness_gtest=
                                                        list(pres_fn="prop",
                                                             pres_thresh=0.5,
                                                             pvalue_thresh=0.05)))
  
  allGrpDdo <-concat(grpDdo1, grpDdo2)
  
  expect_equal(length(allGrpDdo), length(grpDdo1)+length(grpDdo2))
  expect_true(all(getKeys(allGrpDdo) %in% c(getKeys(grpDdo1), getKeys(grpDdo2))))
  
  expect_true(all(class(allGrpDdo[[1]]$value) == class(grpDdo1[[1]]$value)))
  
})

# library(trelliscope)
# vdbConn("/Users/d3l348/tmp/testVdb",autoYes=TRUE)
# makeDisplay(data=allGrpDdo,
#             name="test",
#             panelFn=panelFunctionGenerator("vanKrevelenPlot",
#                                            colorCName="uniqueness_gtest")
#             )
# view()
