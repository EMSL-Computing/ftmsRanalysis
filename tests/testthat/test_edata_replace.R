## Test edata_replace method

library(ftmsRanalysis)
context("edata_replace method")

test_that("Basic tests of edata_replace method", {

  data("exampleProcessedPeakData")
  
  #transform 0 to NA
  peak2 <- suppressMessages(edata_replace(exampleProcessedPeakData, x = 0, y=NA))
  
  sample_cnames <- unique(as.character(dplyr::pull(exampleProcessedPeakData$f_data, getFDataColName(exampleProcessedPeakData))))
  init_zeroes <- sum(exampleProcessedPeakData$e_data[, sample_cnames] == 0, na.rm=TRUE)
  init_nas <- sum(is.na(exampleProcessedPeakData$e_data[, sample_cnames]))
  final_zeroes <- sum(peak2$e_data[, sample_cnames] == 0, na.rm=TRUE)
  final_nas <- sum(is.na(peak2$e_data[, sample_cnames]))
  
  expect_equal(final_zeroes, 0)
  expect_equal(final_nas, init_zeroes+init_nas)
  
  #transform NA to -1
  peak3 <- suppressMessages(edata_replace(peak2, x = NA, y=-1))
  
  init_neg_1 <- sum(peak2$e_data[, sample_cnames] == -1, na.rm=TRUE)
  init_nas <- sum(is.na(peak2$e_data[, sample_cnames]))
  final_neg_1 <- sum(peak3$e_data[, sample_cnames] == -1, na.rm=TRUE)
  final_nas <- sum(is.na(peak3$e_data[, sample_cnames]))
  
  expect_equal(final_nas, 0)
  expect_equal(final_neg_1, init_neg_1+init_nas)
  
})

test_that("Tests of edata_replace method that should throw errors", {
  expect_error(edata_replace("not a real ftmsData object", x=0, y=1))
})