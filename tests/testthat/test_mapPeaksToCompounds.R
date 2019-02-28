## Test mapping peaks to compounds

library(ftmsRanalysis)
library(MetaCycData)
library(KeggData)
context("mapPeaksToCompounds function")

# convenience function to compare attributes between a peakData and compoundData
testCompareAttributes <- function(compObj, peakObj, excludeAttr=NA) {
  expect_true(all(compObj$f_data == peakObj$f_data))
#  expect_true(identical(compIcr$e_meta, peakIcr$e_meta))
  
  for (attr_name in setdiff(names(attributes(peakObj)), excludeAttr)) {
    if (!(attr_name %in% c("data_info", "cnames"))) {
      expect_identical(attr(compObj, attr_name), attr(peakObj, attr_name), info=sprintf("attr_name='%s'", attr_name))
    } else if (attr_name == "data_info") { #data_scale is different
      expect_identical(attr(compObj, attr_name)[names(attr(compObj, attr_name)) != "data_scale"], 
                       attr(peakObj, attr_name)[names(attr(peakObj, attr_name)) != "data_scale"], info="attr_name='data_info'")
    } else if (attr_name == "cnames") { #edata_cname should be different between peakIcr and compIcr
      cc <- setdiff(names(attr(peakObj, "cnames")), "edata_cname")
      expect_identical(attr(compObj, attr_name)[cc], 
                       attr(peakObj, attr_name)[cc], info="attr_name='cnames'")
    }
  }
}


test_that("mapPeaksToCompounds works correctly mapping to MetaCyc", {
  data("exampleProcessedPeakData")
  
  expect_warning(compIcrData <- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  
  expect_true(inherits(compIcrData, "compoundData"))
  expect_true(all(c("e_data", "e_meta", "f_data") %in% names(compIcrData)))
  
  testCompareAttributes(compIcrData, exampleProcessedPeakData, excludeAttr=c("class", "DB", "cnames"))
  peakCNames <- attr(exampleProcessedPeakData, "cnames")
  compCNames <- attr(exampleProcessedPeakData, "cnames")
  expect_true(identical(peakCNames, compCNames[names(peakCNames)]))

  expect_true(nrow(compIcrData$e_data) < nrow(exampleProcessedPeakData$e_data))
  expect_true(identical(compIcrData$f_data, exampleProcessedPeakData$f_data))
  
  edata_cname <- getEDataColName(compIcrData)
  expect_true(all(compIcrData$e_meta[, edata_cname] %in% compIcrData$e_data[, edata_cname]))
  expect_true(all(compIcrData$e_data[, edata_cname] %in% compIcrData$e_meta[, edata_cname]))
  expect_true(edata_cname != getEDataColName(exampleProcessedPeakData))
  expect_true(sum(is.na(compIcrData$e_meta[, getCompoundColName(compIcrData)])) == 0)
  
  expect_equal(getDataScale(compIcrData), getDataScale(exampleProcessedPeakData))
  
  # test using transformed data as input
  
  peakIcr2 <- edata_transform(exampleProcessedPeakData, "log2")
  expect_warning(compIcrData2 <- mapPeaksToCompounds(peakIcr2, db="MetaCyc"))
  expect_equal(getDataScale(compIcrData2), getDataScale(peakIcr2))
  
})

test_that("mapPeaksToCompounds works correctly mapping to Kegg", {
  data("exampleProcessedPeakData")
  
  expect_warning(compIcrData <- mapPeaksToCompounds(exampleProcessedPeakData, db="KEGG"))
  
  expect_true(inherits(compIcrData, "compoundData"))
  expect_true(all(c("e_data", "e_meta", "f_data") %in% names(compIcrData)))
  
  testCompareAttributes(compIcrData, exampleProcessedPeakData, excludeAttr=c("class", "DB", "cnames"))
  peakCNames <- attr(exampleProcessedPeakData, "cnames")
  compCNames <- attr(exampleProcessedPeakData, "cnames")
  expect_true(identical(peakCNames, compCNames[names(peakCNames)]))

  expect_true(nrow(compIcrData$e_data) < nrow(exampleProcessedPeakData$e_data))
  expect_true(identical(compIcrData$f_data, exampleProcessedPeakData$f_data))
  
  edata_cname <- getEDataColName(compIcrData)
  expect_true(all(compIcrData$e_meta[, edata_cname] %in% compIcrData$e_data[, edata_cname]))
  expect_true(all(compIcrData$e_data[, edata_cname] %in% compIcrData$e_meta[, edata_cname]))
  expect_true(edata_cname != getEDataColName(exampleProcessedPeakData))
  expect_true(sum(is.na(compIcrData$e_meta[, getCompoundColName(compIcrData)])) == 0)
  
  expect_equal(getDataScale(compIcrData), getDataScale(exampleProcessedPeakData))
  
  # test using transformed data as input
  
  peakIcr2 <- edata_transform(exampleProcessedPeakData, "log2")
  expect_warning(compIcrData2 <- mapPeaksToCompounds(peakIcr2, db="KEGG"))
  expect_equal(getDataScale(compIcrData2), getDataScale(peakIcr2))
  
})

test_that("mapPeaksToCompounds fails correctly with bad parameters", {
  data("exampleProcessedPeakData")
  
  expect_error(tmp <- mapPeaksToCompounds(exampleProcessedPeakData, db="invalid_db"), regexp = "db must be one of")
  
  expect_error(tmp <- mapPeaksToCompounds(iris), regexp = "must be an object of type peakData")
  
})
  
  


