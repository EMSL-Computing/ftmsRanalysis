## Tests on subset

library(ftmsRanalysis)
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

test_that("test of subset.peakData with one group", {
  data("exampleProcessedPeakData")
  
  grps <- "M_S"
  groupDF <- getGroupDF(exampleProcessedPeakData)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(exampleProcessedPeakData, groups=grps)
  
  testSubsetSizesAttributes(msSubset, exampleProcessedPeakData)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
})


test_that("test of subset.peakData with two groups", {
  data("exampleProcessedPeakData")
  
  grps <- c("M_S", "W_S")
  groupDF <- getGroupDF(exampleProcessedPeakData)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(exampleProcessedPeakData, groups=grps)
  
  testSubsetSizesAttributes(msSubset, exampleProcessedPeakData)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
})

test_that("test of subset.peakData with one sample", {
  data("exampleProcessedPeakData")
  
  groupDF <- getGroupDF(exampleProcessedPeakData)
  samples <- groupDF$SampleID[3]
  
  msSubset <- subset(exampleProcessedPeakData, samples=samples)
  
  testSubsetSizesAttributes(msSubset, exampleProcessedPeakData)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
})

test_that("test of subset.peakData with two samples", {
  data("exampleProcessedPeakData")
  
  groupDF <- getGroupDF(exampleProcessedPeakData)
  samples <- groupDF$SampleID[3:4]
  
  msSubset <- subset(exampleProcessedPeakData, samples=samples)
  
  testSubsetSizesAttributes(msSubset, exampleProcessedPeakData)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
})

test_that("test of subset.compoundData with one group and then two samples", {
  data("exampleProcessedPeakData")
  suppressWarnings(compObj<- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  
  grps <- "M_S"
  groupDF <- getGroupDF(compObj)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(compObj, groups=grps)
  
  testSubsetSizesAttributes(msSubset, compObj)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
  samples <- groupDF$SampleID[3:4]
  
  gSubset <- subset(compObj, samples=samples)
  
  testSubsetSizesAttributes(gSubset, compObj)
  expect_true(all(samples %in% colnames(gSubset$e_data)))  
  expect_true(nrow(gSubset$f_data) == length(samples))
  expect_true(all(samples %in% gSubset$f_data$SampleID))
  
})

test_that("test of subset.reactionData with one group and then two samples", {
  data("exampleProcessedPeakData")
  suppressWarnings(compObj<- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  suppressWarnings(rxnObj <- mapCompoundsToReactions(compObj))
  
  grps <- "M_C"
  groupDF <- getGroupDF(rxnObj)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(rxnObj, groups=grps)
  
  testSubsetSizesAttributes(msSubset, rxnObj)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
  samples <- groupDF$SampleID[1:2]
  
  gSubset <- subset(rxnObj, samples=samples)
  
  testSubsetSizesAttributes(gSubset, rxnObj)
  expect_true(all(samples %in% colnames(gSubset$e_data)))  
  expect_true(nrow(gSubset$f_data) == length(samples))
  expect_true(all(samples %in% gSubset$f_data$SampleID))
  
})

test_that("test of subset.moduleData with one group and then two samples", {
  data("exampleProcessedPeakData")
  suppressWarnings(compObj<- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  suppressWarnings(moduleObj <- mapCompoundsToReactions(compObj))
  
  grps <- "W_C"
  groupDF <- getGroupDF(moduleObj)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(moduleObj, groups=grps)
  
  testSubsetSizesAttributes(msSubset, moduleObj)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
  samples <- groupDF$SampleID[1:2]
  
  gSubset <- subset(moduleObj, samples=samples)
  
  testSubsetSizesAttributes(gSubset, moduleObj)
  expect_true(all(samples %in% colnames(gSubset$e_data)))  
  expect_true(nrow(gSubset$f_data) == length(samples))
  expect_true(all(samples %in% gSubset$f_data$SampleID))
  
})

