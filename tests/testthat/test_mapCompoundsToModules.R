library(fticRanalysis)
context("mapCompoundsToModules function")

test_that("mapCompoundsToModules works correctly mapping to MetaCyc", {
  data("peakIcrProcessed")
  
  suppressWarnings(compIcrData <- mapPeaksToCompounds(peakIcrProcessed, db="MetaCyc"))
  suppressWarnings(modIcrData <- mapCompoundsToModules(compIcrData))
  
  expect_true(inherits(modIcrData, "moduleIcrData"))
  expect_true(all(c("e_data", "e_meta", "f_data") %in% names(modIcrData)))
  
  expect_true(identical(modIcrData$f_data, peakIcrProcessed$f_data))
  
  edata_cname <- getEDataColName(modIcrData)
  expect_true(all(modIcrData$e_meta[, edata_cname] %in% modIcrData$e_data[, edata_cname]))
  expect_true(all(modIcrData$e_data[, edata_cname] %in% modIcrData$e_meta[, edata_cname]))
  expect_true(edata_cname != getEDataColName(peakIcrProcessed))
  
  expect_true(sum(is.na(modIcrData$e_meta[, getModuleColName(modIcrData)])) == 0)
  expect_true(sum(is.na(modIcrData$e_meta[, getModuleNodeColName(modIcrData)])) == 0)
  
  expect_true(is.na(getDataScale(modIcrData)))
  
  # check that all edata values are integer
  tmp_edata <- modIcrData$e_data[, -1]
  expect_true(all(data.frame(lapply(tmp_edata, as.integer)) == tmp_edata))
  
  # transform to presence/absence first, then make sure resulting object is the same as above
  suppressWarnings(compIcrData2 <- mapPeaksToCompounds(peakIcrProcessed, db="MetaCyc"))
  compIcrData2 <- edata_transform(compIcrData2, "pres")
  suppressWarnings(modIcrData2 <- mapCompoundsToModules(compIcrData2))
  
  expect_equal(modIcrData2$e_data[, -1], tmp_edata)
  
  # transform to log2 first, then make sure resulting object is the same as above
  suppressWarnings(compIcrData3 <- mapPeaksToCompounds(peakIcrProcessed, db="MetaCyc"))
  compIcrData3 <- edata_transform(compIcrData3, "log2")
  suppressWarnings(modIcrData3 <- mapCompoundsToModules(compIcrData3))
  
  expect_equal(modIcrData3$e_data[, -1], tmp_edata)
  
})


test_that("mapCompoundsToReactions fails correctly with bad parameters", {
  
  expect_error(tmp <- mapCompoundsToReactions(peakIcrProcessed), regexp = "must be an object of type compoundIcrData")
  expect_error(tmp <- mapCompoundsToReactions(iris), regexp = "must be an object of type compoundIcrData")
  
})