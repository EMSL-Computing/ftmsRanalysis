library(ftmsRanalysis)
context("mapCompoundsToReactions function")

test_that("mapCompoundsToReactions works correctly mapping to MetaCyc", {
  data("exampleProcessedPeakData")

  expect_warning(compIcrData <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  suppressWarnings(rxnIcrData <- mapCompoundsToReactions(compIcrData))
  
  expect_true(inherits(rxnIcrData, "reactionData"))
  expect_true(all(c("e_data", "e_meta", "f_data") %in% names(rxnIcrData)))
  
  expect_true(nrow(rxnIcrData$e_data) < nrow(exampleProcessedPeakData$e_data))
  expect_true(identical(rxnIcrData$f_data, exampleProcessedPeakData$f_data))
  
  edata_cname <- getEDataColName(rxnIcrData)
  expect_true(all(rxnIcrData$e_meta[, edata_cname] %in% rxnIcrData$e_data[, edata_cname]))
  expect_true(all(rxnIcrData$e_data[, edata_cname] %in% rxnIcrData$e_meta[, edata_cname]))
  expect_true(edata_cname != getEDataColName(exampleProcessedPeakData))
  expect_true(sum(is.na(rxnIcrData$e_meta[, getReactionColName(rxnIcrData)])) == 0)
  expect_true(getDataScale(rxnIcrData) == "count")
  
  # check that all edata values are integer
  tmp_edata <- rxnIcrData$e_data[, -1]
  expect_true(all(data.frame(lapply(tmp_edata, as.integer)) == tmp_edata))
  
  # transform to presence/absence first, then make sure resulting object is the same as above
  suppressWarnings(compIcrData2 <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  compIcrData2 <- edata_transform(compIcrData2, "pres")
  suppressWarnings(rxnIcrData2 <- mapCompoundsToReactions(compIcrData2))
  
  expect_equal(rxnIcrData2$e_data[, -1], tmp_edata)
  
  # transform to log2 first, then make sure resulting object is the same as above
  suppressWarnings(compIcrData3 <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  compIcrData3 <- edata_transform(compIcrData3, "log2")
  suppressWarnings(rxnIcrData3 <- mapCompoundsToReactions(compIcrData3))
  
  expect_equal(rxnIcrData3$e_data[, -1], tmp_edata)
  
})

test_that("mapCompoundsToReactions works correctly mapping to KEGG", {
  data("exampleProcessedPeakData")
  
  expect_warning(compIcrData <- mapPeaksToCompounds(exampleProcessedPeakData, db="KEGG"))
  suppressWarnings(rxnIcrData <- mapCompoundsToReactions(compIcrData))
  
  expect_true(inherits(rxnIcrData, "reactionData"))
  expect_true(all(c("e_data", "e_meta", "f_data") %in% names(rxnIcrData)))
  
  expect_true(nrow(rxnIcrData$e_data) < nrow(exampleProcessedPeakData$e_data))
  expect_true(identical(rxnIcrData$f_data, exampleProcessedPeakData$f_data))
  
  edata_cname <- getEDataColName(rxnIcrData)
  expect_true(all(rxnIcrData$e_meta[, edata_cname] %in% rxnIcrData$e_data[, edata_cname]))
  expect_true(all(rxnIcrData$e_data[, edata_cname] %in% rxnIcrData$e_meta[, edata_cname]))
  expect_true(edata_cname != getEDataColName(exampleProcessedPeakData))
  expect_true(sum(is.na(rxnIcrData$e_meta[, getReactionColName(rxnIcrData)])) == 0)
  expect_true(getDataScale(rxnIcrData) == "count")
  
  # check that all edata values are integer
  tmp_edata <- rxnIcrData$e_data[, -1]
  expect_true(all(data.frame(lapply(tmp_edata, as.integer)) == tmp_edata))
  
  # transform to presence/absence first, then make sure resulting object is the same as above
  suppressWarnings(compIcrData2 <- mapPeaksToCompounds(exampleProcessedPeakData, db="KEGG"))
  compIcrData2 <- edata_transform(compIcrData2, "pres")
  suppressWarnings(rxnIcrData2 <- mapCompoundsToReactions(compIcrData2))
  
  expect_equal(rxnIcrData2$e_data[, -1], tmp_edata)
  
  # transform to log2 first, then make sure resulting object is the same as above
  suppressWarnings(compIcrData3 <- mapPeaksToCompounds(exampleProcessedPeakData, db="KEGG"))
  compIcrData3 <- edata_transform(compIcrData3, "log2")
  suppressWarnings(rxnIcrData3 <- mapCompoundsToReactions(compIcrData3))
  
  expect_equal(rxnIcrData3$e_data[, -1], tmp_edata)
  
})

test_that("mapCompoundsToReactions fails correctly with bad parameters", {

  expect_error(tmp <- mapCompoundsToReactions(exampleProcessedPeakData), regexp = "must be an object of type 'compoundData'")
  expect_error(tmp <- mapCompoundsToReactions(iris), regexp = "must be an object of type 'compoundData'")
  
})