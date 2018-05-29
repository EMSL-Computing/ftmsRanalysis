## Test mapping peaks to compounds

library(fticRanalysis)
library(MetaCycData)
context("mapPeaksToCompounds function")

# convenience function to compare attributes between a peakIcrData and compoundIcrData
testCompareAttributes <- function(compIcr, peakIcr, excludeAttr=NA) {
  expect_true(all(compIcr$f_data == peakIcr$f_data))
#  expect_true(identical(compIcr$e_meta, peakIcr$e_meta))
  
  for (attr_name in setdiff(names(attributes(peakIcr)), excludeAttr)) {
    if (!(attr_name %in% c("data_info", "cnames"))) {
      expect_identical(attr(compIcr, attr_name), attr(peakIcr, attr_name), info=sprintf("attr_name='%s'", attr_name))
    } else if (attr_name == "data_info") { #data_scale is different
      expect_identical(attr(compIcr, attr_name)[names(attr(compIcr, attr_name)) != "data_scale"], 
                       attr(peakIcr, attr_name)[names(attr(peakIcr, attr_name)) != "data_scale"], info="attr_name='data_info'")
    } else if (attr_name == "cnames") { #edata_cname should be different between peakIcr and compIcr
      cc <- setdiff(names(attr(peakIcr, "cnames")), "edata_cname")
      expect_identical(attr(compIcr, attr_name)[cc], 
                       attr(peakIcr, attr_name)[cc], info="attr_name='cnames'")
    }
  }
}


test_that("mapPeaksToCompounds works correctly mapping to MetaCyc", {
  data("peakIcrProcessed")
  
  expect_warning(compIcrData <- mapPeaksToCompounds(peakIcrProcessed, db="MetaCyc"))
  
  expect_true(inherits(compIcrData, "compoundIcrData"))
  expect_true(all(c("e_data", "e_meta", "f_data") %in% names(compIcrData)))
  
  testCompareAttributes(compIcrData, peakIcrProcessed, "class")
  expect_true(nrow(compIcrData$e_data) < nrow(peakIcrProcessed$e_data))
  expect_true(identical(compIcrData$f_data, peakIcrProcessed$f_data))
  
  edata_cname <- getEDataColName(compIcrData)
  expect_true(all(compIcrData$e_meta[, edata_cname] %in% compIcrData$e_data[, edata_cname]))
  expect_true(all(compIcrData$e_data[, edata_cname] %in% compIcrData$e_meta[, edata_cname]))
  expect_true(edata_cname != getEDataColName(peakIcrProcessed))
  expect_true(sum(is.na(compIcrData$e_meta[, getCompoundColName(compIcrData)])) == 0)
  
  expect_equal(getDataScale(compIcrData), getDataScale(peakIcrProcessed))
  
  # test using transformed data as input
  
  peakIcr2 <- edata_transform(peakIcrProcessed, "log2")
  expect_warning(compIcrData2 <- mapPeaksToCompounds(peakIcr2, db="MetaCyc"))
  expect_equal(getDataScale(compIcrData2), getDataScale(peakIcr2))
  
})

test_that("mapPeaksToCompounds fails correctly with bad parameters", {
  data("peakIcrProcessed")
  
  expect_error(tmp <- mapPeaksToCompounds(peakIcrProcessed, db="invalid_db"), regexp = "db must be one of")
  
  expect_error(tmp <- mapPeaksToCompounds(iris), regexp = "must be an object of type peakIcrData")
  
})
  
  


