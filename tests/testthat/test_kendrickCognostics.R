## Test edata_replace method

library(ftmsRanalysis)
library(datadr)
context("kendrickCognostics method")

test_that("Basic tests of kendrickCognostics method on samples", {

  data("exampleProcessedPeakData")
  
  bySample <- divideBySample(exampleProcessedPeakData)
  cogs1 <- kendrickCognostics("bs1")(bySample[[1]]$value)
  expect_true(inherits(cogs1, "list"))
  expect_true(length(cogs1) > 0)
  expect_true(!identical(names(cogs1), NULL))
  expect_true(!any(is.na(names(cogs1))))
  
  cnames <- c("num_peaks", "mean_kendrick_mass", "mean_kendrick_defect", grep("prop", names(cogs1), value=TRUE))
  expect_true(all(is.numeric(unlist(cogs1[cnames]))))
  
  all_cogs <- suppressMessages(drLapply(bySample, kendrickCognostics("bs2")))
  
})

test_that("Basic tests of kendrickCognostics method on groups", {
  
  data("exampleProcessedPeakData")
  
  byGroup <- divideByGroup(exampleProcessedPeakData)
  cogs1 <- kendrickCognostics("bs3")(byGroup[[1]]$value)
  expect_true(inherits(cogs1, "list"))
  expect_true(length(cogs1) > 0)
  expect_true(!identical(names(cogs1), NULL))
  expect_true(!any(is.na(names(cogs1))))
  
  cnames <- c("num_peaks", "mean_kendrick_mass", "mean_kendrick_defect", grep("prop", names(cogs1), value=TRUE))
  expect_true(all(is.numeric(unlist(cogs1[cnames]))))
  
  all_cogs <- suppressMessages(drLapply(byGroup, kendrickCognostics("bs2")))
  
})

test_that("Basic tests of kendrickCognostics method on group summaries", {
  
  data("exampleProcessedPeakData")
  
  grpSummaries <- suppressMessages(summarizeGroups(divideByGroup(exampleProcessedPeakData), summary_functions = "n_present"))
  cogs1 <- kendrickCognostics("bs2")(grpSummaries[[1]]$value)
  expect_true(inherits(cogs1, "list"))
  expect_true(length(cogs1) > 0)
  expect_true(!identical(names(cogs1), NULL))
  expect_true(!any(is.na(names(cogs1))))
  
  cnames <- c("num_peaks", "mean_kendrick_mass", "mean_kendrick_defect", grep("prop", names(cogs1), value=TRUE))
  expect_true(all(is.numeric(unlist(cogs1[cnames]))))
  
  all_cogs <- suppressMessages(drLapply(grpSummaries, kendrickCognostics("bs1")))
  
})

test_that("Basic tests of kendrickCognostics method on group comparisons", {
  
  data("exampleProcessedPeakData")
  
  grpComp <- suppressMessages(summarizeGroupComparisons(divideByGroupComparisons(exampleProcessedPeakData, comparisons="all"),
                                       summary_functions="uniqueness_gtest", 
                                       summary_function_params=list(
                                         uniqueness_gtest=list(pres_fn="nsamps", pres_thresh=2, pvalue_thresh=0.05)
                                       )))
  cogs1 <- kendrickCognostics("bs1", uniquenessColName = "uniqueness_gtest")(grpComp[[1]]$value)
  expect_true(inherits(cogs1, "list"))
  expect_true(length(cogs1) > 0)
  expect_true(!identical(names(cogs1), NULL))
  expect_true(!any(is.na(names(cogs1))))
  
  cnames <- c("num_peaks_common", "num_peaks_g1", "num_peaks_g2", grep("prop", names(cogs1), value=TRUE))
  expect_true(all(is.numeric(unlist(cogs1[cnames]))))
  
  all_cogs <- suppressMessages(drLapply(grpComp, kendrickCognostics("bs2", uniquenessColName = "uniqueness_gtest")))
  
  expect_error(kendrickCognostics("bs1", uniquenessColName = "uniqueness_gtest2")(grpComp[[1]]$value))
  
})


test_that("Tests of kendrickCognostics method that should throw errors", {
  expect_error(kendrickCognostics("bs1")(NULL))
  expect_error(kendrickCognostics("bs2")(iris))
  expect_error(kendrickCognostics("bs2")(iris))
  expect_error(kendrickCognostics("bs4")(bySample[[1]]$value))
})