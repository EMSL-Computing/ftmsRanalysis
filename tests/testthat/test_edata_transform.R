## edata_transform tests

library(fticRanalysis)
context("edata_transform function")

# valid data_scales 'log2', 'log10', 'log', 'pres', 'abundance'

# convenience function to compare attributes, sizes of e_data, f_data and e_meta, and values of f_data and e_meta
testCompareSizesAttributes <- function(newObj, originalObj, excludeAttr=NA) {
  expect_true(all(dim(newObj$e_data) == dim(originalObj$e_data)))
  expect_true(all(dim(newObj$f_data) == dim(originalObj$f_data)))
  expect_true(all(dim(newObj$e_meta) == dim(originalObj$e_meta)))
  expect_true(all(newObj$f_data == originalObj$f_data))
  expect_true(identical(newObj$e_meta, originalObj$e_meta))
  
  for (attr_name in setdiff(names(attributes(originalObj)), excludeAttr)) {
    if (attr_name != "data_info") {
      expect_identical(attr(newObj, attr_name), attr(originalObj, attr_name), info=sprintf("attr_name='%s'", attr_name))
    } else {
      expect_identical(attr(newObj, attr_name)[names(attr(newObj, attr_name)) != "data_scale"], 
                       attr(originalObj, attr_name)[names(attr(originalObj, attr_name)) != "data_scale"], info="attr_name='data_info'")
    }
  }
}


test_that("transforming from abundance to other data scales works correctly", {
  data("peakIcrData")
  sample_cols <- setdiff(colnames(peakIcrData$e_data), getEDataColName(peakIcrData))
  nonzero_inds <- which(peakIcrData$e_data[, sample_cols] != 0, arr.ind = TRUE)
  zero_inds <- which(peakIcrData$e_data[, sample_cols] == 0, arr.ind = TRUE)
  
  #log2
  p2 <- edata_transform(peakIcrData, "log2")
  expect_true(all(log2(peakIcrData$e_data[, sample_cols][nonzero_inds]) == p2$e_data[, sample_cols][nonzero_inds]))
  expect_true(all(is.na(p2$e_data[, sample_cols][zero_inds])))
  expect_equal(getDataScale(p2), "log2")
  testCompareSizesAttributes(p2, peakIcrData)
  
  #log10
  p3 <- edata_transform(peakIcrData, "log10")
  expect_true(all(log10(peakIcrData$e_data[, sample_cols][nonzero_inds]) == p3$e_data[, sample_cols][nonzero_inds]))
  expect_true(all(is.na(p3$e_data[, sample_cols][zero_inds])))
  expect_equal(getDataScale(p3), "log10")
  testCompareSizesAttributes(p3, peakIcrData)
  
  #log
  p4 <- edata_transform(peakIcrData, "log")
  expect_true(all(log(peakIcrData$e_data[, sample_cols][nonzero_inds]) == p4$e_data[, sample_cols][nonzero_inds]))
  expect_true(all(is.na(p4$e_data[, sample_cols][zero_inds])))
  expect_equal(getDataScale(p4), "log")
  testCompareSizesAttributes(p4, peakIcrData)
  
  #pres
  p5 <- edata_transform(peakIcrData, "pres")
  expect_true(all(p5$e_data[, sample_cols][nonzero_inds] == 1))
  expect_true(all(p5$e_data[, sample_cols][zero_inds] == 0))
  expect_equal(getDataScale(p5), "pres")
  testCompareSizesAttributes(p5, peakIcrData)
  
  #abundance : should throw an error
  expect_error(edata_transform(peakIcrData, "abundance"), regexp = "Data is already")
  
  #invalid data_scale value: should throw an error
  expect_error(edata_transform(peakIcrData, "this is not a valid data scale"), regexp = "not a valid option")
})

test_that("transforming from log2 to other data scales works correctly", {
  data("peakIcrData")
  peakIcrData <- edata_transform(peakIcrData, "log2")
  sample_cols <- setdiff(colnames(peakIcrData$e_data), getEDataColName(peakIcrData))
  non_na_inds <- which(peakIcrData$e_data[, sample_cols] != 0, arr.ind = TRUE)
  na_inds <- which(peakIcrData$e_data[, sample_cols] == 0, arr.ind = TRUE)
  
  #abundance
  p2 <- edata_transform(peakIcrData, "abundance")
  expect_true(all(2^(peakIcrData$e_data[, sample_cols][non_na_inds]) == p2$e_data[, sample_cols][non_na_inds]))
  expect_true(all(is.na(p2$e_data[, sample_cols][na_inds])))
  expect_equal(getDataScale(p2), "abundance")
  testCompareSizesAttributes(p2, peakIcrData)
  
  #log10
  p3 <- expect_warning(edata_transform(peakIcrData, "log10"))
  expect_true(all(log10(2^peakIcrData$e_data[, sample_cols][non_na_inds]) == p3$e_data[, sample_cols][non_na_inds]))
  expect_true(all(is.na(p3$e_data[, sample_cols][na_inds])))
  expect_equal(getDataScale(p3), "log10")
  testCompareSizesAttributes(p3, peakIcrData)
  
  #log
  p4 <- expect_warning(edata_transform(peakIcrData, "log"))
  expect_true(all(log(2^peakIcrData$e_data[, sample_cols][non_na_inds]) == p4$e_data[, sample_cols][non_na_inds]))
  expect_true(all(is.na(p4$e_data[, sample_cols][na_inds])))
  expect_equal(getDataScale(p4), "log")
  testCompareSizesAttributes(p4, peakIcrData)
  
  #pres
  p5 <- expect_warning(edata_transform(peakIcrData, "pres"))
  expect_true(all(p5$e_data[, sample_cols][non_na_inds] == 1))
  expect_true(all(p5$e_data[, sample_cols][na_inds] == 0))
  expect_equal(getDataScale(p5), "pres")
  testCompareSizesAttributes(p5, peakIcrData)
  
  #log2 : should throw an error
  expect_error(edata_transform(peakIcrData, "log2"), regexp = "Data is already")
})

test_that("transforming from log10 to other data scales works correctly", {
  data("peakIcrData")
  peakIcrData <- edata_transform(peakIcrData, "log10")
  sample_cols <- setdiff(colnames(peakIcrData$e_data), getEDataColName(peakIcrData))
  non_na_inds <- which(peakIcrData$e_data[, sample_cols] != 0, arr.ind = TRUE)
  na_inds <- which(peakIcrData$e_data[, sample_cols] == 0, arr.ind = TRUE)
  
  #abundance
  p2 <- edata_transform(peakIcrData, "abundance")
  expect_true(all(10^(peakIcrData$e_data[, sample_cols][non_na_inds]) == p2$e_data[, sample_cols][non_na_inds]))
  expect_true(all(is.na(p2$e_data[, sample_cols][na_inds])))
  expect_equal(getDataScale(p2), "abundance")
  testCompareSizesAttributes(p2, peakIcrData)
  
  #log2
  p3 <- expect_warning(edata_transform(peakIcrData, "log2"))
  expect_true(all(log2(10^peakIcrData$e_data[, sample_cols][non_na_inds]) == p3$e_data[, sample_cols][non_na_inds]))
  expect_true(all(is.na(p3$e_data[, sample_cols][na_inds])))
  expect_equal(getDataScale(p3), "log2")
  testCompareSizesAttributes(p3, peakIcrData)
  
  #log
  p4 <- expect_warning(edata_transform(peakIcrData, "log"))
  expect_true(all(log(10^peakIcrData$e_data[, sample_cols][non_na_inds]) == p4$e_data[, sample_cols][non_na_inds]))
  expect_true(all(is.na(p4$e_data[, sample_cols][na_inds])))
  expect_equal(getDataScale(p4), "log")
  testCompareSizesAttributes(p4, peakIcrData)
  
  #pres
  p5 <- expect_warning(edata_transform(peakIcrData, "pres"))
  expect_true(all(p5$e_data[, sample_cols][non_na_inds] == 1))
  expect_true(all(p5$e_data[, sample_cols][na_inds] == 0))
  expect_equal(getDataScale(p5), "pres")
  testCompareSizesAttributes(p5, peakIcrData)
  
  #log10 : should throw an error
  expect_error(edata_transform(peakIcrData, "log10"), regexp = "Data is already")
})

test_that("transforming from log to other data scales works correctly", {
  data("peakIcrData")
  peakIcrData <- edata_transform(peakIcrData, "log")
  sample_cols <- setdiff(colnames(peakIcrData$e_data), getEDataColName(peakIcrData))
  non_na_inds <- which(peakIcrData$e_data[, sample_cols] != 0, arr.ind = TRUE)
  na_inds <- which(peakIcrData$e_data[, sample_cols] == 0, arr.ind = TRUE)
  
  #abundance
  p2 <- edata_transform(peakIcrData, "abundance")
  expect_true(all(exp(peakIcrData$e_data[, sample_cols][non_na_inds]) == p2$e_data[, sample_cols][non_na_inds]))
  expect_true(all(is.na(p2$e_data[, sample_cols][na_inds])))
  expect_equal(getDataScale(p2), "abundance")
  testCompareSizesAttributes(p2, peakIcrData)
  
  #log2
  p3 <- expect_warning(edata_transform(peakIcrData, "log2"))
  expect_true(all(log2(exp(peakIcrData$e_data[, sample_cols][non_na_inds])) == p3$e_data[, sample_cols][non_na_inds]))
  expect_true(all(is.na(p3$e_data[, sample_cols][na_inds])))
  expect_equal(getDataScale(p3), "log2")
  testCompareSizesAttributes(p3, peakIcrData)
  
  #log
  p4 <- expect_warning(edata_transform(peakIcrData, "log10"))
  expect_true(all(log10(exp(peakIcrData$e_data[, sample_cols][non_na_inds])) == p4$e_data[, sample_cols][non_na_inds]))
  expect_true(all(is.na(p4$e_data[, sample_cols][na_inds])))
  expect_equal(getDataScale(p4), "log10")
  testCompareSizesAttributes(p4, peakIcrData)
  
  #pres
  p5 <- expect_warning(edata_transform(peakIcrData, "pres"))
  expect_true(all(p5$e_data[, sample_cols][non_na_inds] == 1))
  expect_true(all(p5$e_data[, sample_cols][na_inds] == 0))
  expect_equal(getDataScale(p5), "pres")
  testCompareSizesAttributes(p5, peakIcrData)
  
  #log : should throw an error
  expect_error(edata_transform(peakIcrData, "log"), regexp = "Data is already")
})
