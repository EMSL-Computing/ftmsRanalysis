## Tests on subset

library(fticRanalysis)
context("subset function")

# test edata and emeta sizes and attribute equality
testSubsetSizesAttributes <- function(subsetObj, originalObj) {
  expect_true(all(class(subsetObj) == class(originalObj)))
  
  expect_true(nrow(subsetObj$e_data) == nrow(originalObj$e_data))
  expect_true(all(dim(subsetObj$e_meta) == dim(originalObj$e_meta)))
  
  # compare attributes
  diffAttrNames <- "group_DF"  #attribute names that should not be the same in the subset
  for (attr_name in setdiff(names(attributes(originalObj)), diffAttrNames)) {
    expect_identical(attr(subsetObj, attr_name), attr(originalObj, attr_name), info=sprintf("attr_name='%s'", attr_name))
  }
  
}

test_that("test of subset.peakIcrData with one group", {
  data("peakIcrProcessed")
  
  grps <- "M_S"
  groupDF <- getGroupDF(peakIcrProcessed)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(peakIcrProcessed, groups=grps)
  
  testSubsetSizesAttributes(msSubset, peakIcrProcessed)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
})


test_that("test of subset.peakIcrData with two groups", {
  data("peakIcrProcessed")
  
  grps <- c("M_S", "W_S")
  groupDF <- getGroupDF(peakIcrProcessed)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(peakIcrProcessed, groups=grps)
  
  testSubsetSizesAttributes(msSubset, peakIcrProcessed)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
})

test_that("test of subset.peakIcrData with one sample", {
  data("peakIcrProcessed")
  
  groupDF <- getGroupDF(peakIcrProcessed)
  samples <- groupDF$SampleID[3]
  
  msSubset <- subset(peakIcrProcessed, samples=samples)
  
  testSubsetSizesAttributes(msSubset, peakIcrProcessed)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
})

test_that("test of subset.peakIcrData with two samples", {
  data("peakIcrProcessed")
  
  groupDF <- getGroupDF(peakIcrProcessed)
  samples <- groupDF$SampleID[3:4]
  
  msSubset <- subset(peakIcrProcessed, samples=samples)
  
  testSubsetSizesAttributes(msSubset, peakIcrProcessed)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
})