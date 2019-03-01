library(ftmsRanalysis)
context("mapCompoundsToModules function")

test_that("mapCompoundsToModules works correctly mapping to MetaCyc", {
  data("exampleProcessedPeakData")
  
  suppressWarnings(compObj <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  suppressWarnings(modObj <- mapCompoundsToModules(compObj))
  
  expect_true(inherits(modObj, "moduleData"))
  expect_true(all(c("e_data", "e_meta", "f_data") %in% names(modObj)))
  
  expect_true(identical(modObj$f_data, exampleProcessedPeakData$f_data))
  
  edata_cname <- getEDataColName(modObj)
  expect_true(all(modObj$e_meta[, edata_cname] %in% modObj$e_data[, edata_cname]))
  expect_true(all(modObj$e_data[, edata_cname] %in% modObj$e_meta[, edata_cname]))
  expect_true(edata_cname != getEDataColName(exampleProcessedPeakData))
  
  expect_true(sum(is.na(modObj$e_meta[, getModuleColName(modObj)])) == 0)
  expect_true(sum(is.na(modObj$e_meta[, getModuleNodeColName(modObj)])) == 0)
  
  expect_true(getDataScale(modObj) == "count")
  
  # check that all edata values are integer
  tmp_edata <- modObj$e_data[, -1]
  expect_true(all(data.frame(lapply(tmp_edata, as.integer)) == tmp_edata))
  
  # transform to presence/absence first, then make sure resulting object is the same as above
  suppressWarnings(compObj2 <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  compObj2 <- edata_transform(compObj2, "pres")
  suppressWarnings(modObj2 <- mapCompoundsToModules(compObj2))
  
  expect_equal(modObj2$e_data[, -1], tmp_edata)
  
  # transform to log2 first, then make sure resulting object is the same as above
  suppressWarnings(compObj3 <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  compObj3 <- edata_transform(compObj3, "log2")
  suppressWarnings(modObj3 <- mapCompoundsToModules(compObj3))
  
  expect_equal(modObj3$e_data[, -1], tmp_edata)
  
})

test_that("mapCompoundsToModules works correctly mapping to KEGG", {
  data("exampleProcessedPeakData")
  
  suppressWarnings(compObj <- mapPeaksToCompounds(exampleProcessedPeakData, db="KEGG"))
  suppressWarnings(modObj <- mapCompoundsToModules(compObj))
  
  expect_true(inherits(modObj, "moduleData"))
  expect_true(all(c("e_data", "e_meta", "f_data") %in% names(modObj)))
  
  expect_true(identical(modObj$f_data, exampleProcessedPeakData$f_data))
  
  edata_cname <- getEDataColName(modObj)
  expect_true(all(modObj$e_meta[, edata_cname] %in% modObj$e_data[, edata_cname]))
  expect_true(all(modObj$e_data[, edata_cname] %in% modObj$e_meta[, edata_cname]))
  expect_true(edata_cname != getEDataColName(exampleProcessedPeakData))
  
  expect_true(sum(is.na(modObj$e_meta[, getModuleColName(modObj)])) == 0)
  expect_true(sum(is.na(modObj$e_meta[, getModuleNodeColName(modObj)])) == 0)
  
  expect_true(getDataScale(modObj) == "count")
  
  # check that all edata values are integer
  tmp_edata <- modObj$e_data[, -1]
  expect_true(all(data.frame(lapply(tmp_edata, as.integer)) == tmp_edata))
  
  # transform to presence/absence first, then make sure resulting object is the same as above
  suppressWarnings(compObj2 <- mapPeaksToCompounds(exampleProcessedPeakData, db="KEGG"))
  compObj2 <- edata_transform(compObj2, "pres")
  suppressWarnings(modObj2 <- mapCompoundsToModules(compObj2))
  
  expect_equal(modObj2$e_data[, -1], tmp_edata)
  
  # transform to log2 first, then make sure resulting object is the same as above
  suppressWarnings(compObj3 <- mapPeaksToCompounds(exampleProcessedPeakData, db="KEGG"))
  compObj3 <- edata_transform(compObj3, "log2")
  suppressWarnings(modObj3 <- mapCompoundsToModules(compObj3))
  
  expect_equal(modObj3$e_data[, -1], tmp_edata)
  
})

test_that("mapCompoundsToModules fails correctly with bad parameters", {
  
  expect_error(tmp <- mapCompoundsToModules(exampleProcessedPeakData), regexp = "must be an object of type 'compoundData'")
  expect_error(tmp <- mapCompoundsToModules(iris), regexp = "must be an object of type 'compoundData'")
  
})