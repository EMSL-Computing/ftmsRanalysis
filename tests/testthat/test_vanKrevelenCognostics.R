## Test edata_replace method

library(ftmsRanalysis)
library(datadr)
context("vanKrevelenCognostics method")

test_that("Basic tests of vanKrevelenCognostics method on samples", {

  data("exampleProcessedPeakData")
  
  bySample <- divideBySample(exampleProcessedPeakData)
  cogs1 <- vanKrevelenCognostics("bs1")(bySample[[1]]$value)
  expect_true(inherits(cogs1, "list"))
  expect_true(length(cogs1) > 0)
  expect_true(!identical(names(cogs1), NULL))
  expect_true(!any(is.na(names(cogs1))))
  
  cnames <- c("num_peaks", grep("prop", names(cogs1), value=TRUE))
  expect_true(all(is.numeric(unlist(cogs1[cnames]))))
  
  all_cogs <- suppressMessages(drLapply(bySample, vanKrevelenCognostics("bs2")))
  
})

test_that("Basic tests of vanKrevelenCognostics method on groups", {
  
  data("exampleProcessedPeakData")
  
  byGroup <- divideByGroup(exampleProcessedPeakData)
  cogs1 <- vanKrevelenCognostics("bs3")(byGroup[[1]]$value)
  expect_true(inherits(cogs1, "list"))
  expect_true(length(cogs1) > 0)
  expect_true(!identical(names(cogs1), NULL))
  expect_true(!any(is.na(names(cogs1))))
  
  cnames <- c("num_peaks", grep("prop", names(cogs1), value=TRUE))
  expect_true(all(is.numeric(unlist(cogs1[cnames]))))
  
  all_cogs <- suppressMessages(drLapply(byGroup, vanKrevelenCognostics("bs2")))
  
})

test_that("Basic tests of vanKrevelenCognostics method on group summaries", {
  
  data("exampleProcessedPeakData")
  
  grpSummaries <- suppressMessages(summarizeGroups(divideByGroup(exampleProcessedPeakData), summary_functions = "n_present"))
  cogs1 <- vanKrevelenCognostics("bs2")(grpSummaries[[1]]$value)
  expect_true(inherits(cogs1, "list"))
  expect_true(length(cogs1) > 0)
  expect_true(!identical(names(cogs1), NULL))
  expect_true(!any(is.na(names(cogs1))))
  
  cnames <- c("num_peaks", grep("prop", names(cogs1), value=TRUE))
  expect_true(all(is.numeric(unlist(cogs1[cnames]))))
  
  all_cogs <- suppressMessages(drLapply(grpSummaries, vanKrevelenCognostics("bs1")))
  
})

test_that("Basic tests of vanKrevelenCognostics method on group comparisons", {
  
  data("exampleProcessedPeakData")
  
  grpComp <- suppressMessages(summarizeGroupComparisons(divideByGroupComparisons(exampleProcessedPeakData, comparisons="all"),
                                       summary_functions="uniqueness_gtest", 
                                       summary_function_params=list(
                                         uniqueness_gtest=list(pres_fn="nsamps", pres_thresh=2, pvalue_thresh=0.05)
                                       )))
  cogs1 <- vanKrevelenCognostics("bs1", uniquenessColName = "uniqueness_gtest")(grpComp[[1]]$value)
  expect_true(inherits(cogs1, "list"))
  expect_true(length(cogs1) > 0)
  expect_true(!identical(names(cogs1), NULL))
  expect_true(!any(is.na(names(cogs1))))
  
  cnames <- c("num_peaks_common", "num_peaks_g1", "num_peaks_g2", grep("prop", names(cogs1), value=TRUE))
  expect_true(all(is.numeric(unlist(cogs1[cnames]))))
  
  all_cogs <- suppressMessages(drLapply(grpComp, vanKrevelenCognostics("bs2", uniquenessColName = "uniqueness_gtest")))
  
  expect_error(vanKrevelenCognostics("bs1", uniquenessColName = "uniqueness_gtest2")(grpComp[[1]]$value))
  
})


test_that("Tests of vanKrevelenCognostics method that should throw errors", {
  expect_error(vanKrevelenCognostics("bs1")(NULL))
  expect_error(vanKrevelenCognostics("bs2")(iris))
  expect_error(vanKrevelenCognostics("bs2")(iris))
  expect_error(vanKrevelenCognostics("bs4")(bySample[[1]]$value))
})