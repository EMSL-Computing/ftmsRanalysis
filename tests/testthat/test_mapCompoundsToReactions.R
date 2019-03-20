library(ftmsRanalysis)
context("mapCompoundsToReactions function")

test_that("mapCompoundsToReactions works correctly mapping to MetaCyc", {
  data("exampleProcessedPeakData")

  expect_warning(compObj <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  suppressWarnings(rxnObj <- mapCompoundsToReactions(compObj))
  
  expect_true(inherits(rxnObj, "reactionData"))
  expect_true(all(c("e_data", "e_meta", "f_data") %in% names(rxnObj)))
  
  expect_true(nrow(rxnObj$e_data) < nrow(exampleProcessedPeakData$e_data))
  expect_true(identical(rxnObj$f_data, exampleProcessedPeakData$f_data))
  
  edata_cname <- getEDataColName(rxnObj)
  expect_true(all(rxnObj$e_meta[, edata_cname] %in% rxnObj$e_data[, edata_cname]))
  expect_true(all(rxnObj$e_data[, edata_cname] %in% rxnObj$e_meta[, edata_cname]))
  expect_true(edata_cname != getEDataColName(exampleProcessedPeakData))
  expect_true(sum(is.na(rxnObj$e_meta[, getReactionColName(rxnObj)])) == 0)
  expect_true(getDataScale(rxnObj) == "count")
  
  # check that all edata values are integer
  tmp_edata <- rxnObj$e_data[, -1]
  expect_true(all(data.frame(lapply(tmp_edata, as.integer)) == tmp_edata))
  
  # transform to presence/absence first, then make sure resulting object is the same as above
  suppressWarnings(compObj2 <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  compObj2 <- edata_transform(compObj2, "pres")
  suppressWarnings(rxnObj2 <- mapCompoundsToReactions(compObj2))
  
  expect_equal(rxnObj2$e_data[, -1], tmp_edata)
  
  # transform to log2 first, then make sure resulting object is the same as above
  suppressWarnings(compObj3 <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  compObj3 <- edata_transform(compObj3, "log2")
  suppressWarnings(rxnObj3 <- mapCompoundsToReactions(compObj3))
  
  expect_equal(rxnObj3$e_data[, -1], tmp_edata)
  
})

test_that("mapCompoundsToReactions fails correctly with bad parameters", {

  expect_error(tmp <- mapCompoundsToReactions(exampleProcessedPeakData), regexp = "must be an object of type 'compoundData'")
  expect_error(tmp <- mapCompoundsToReactions(iris), regexp = "must be an object of type 'compoundData'")
  
})