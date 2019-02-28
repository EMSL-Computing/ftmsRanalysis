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
  suppressWarnings(compIcr<- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  
  grps <- "M_S"
  groupDF <- getGroupDF(compIcr)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(compIcr, groups=grps)
  
  testSubsetSizesAttributes(msSubset, compIcr)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
  samples <- groupDF$SampleID[3:4]
  
  gSubset <- subset(compIcr, samples=samples)
  
  testSubsetSizesAttributes(gSubset, compIcr)
  expect_true(all(samples %in% colnames(gSubset$e_data)))  
  expect_true(nrow(gSubset$f_data) == length(samples))
  expect_true(all(samples %in% gSubset$f_data$SampleID))
  
})

test_that("test of subset.reactionData with one group and then two samples", {
  data("exampleProcessedPeakData")
  suppressWarnings(compIcr<- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  suppressWarnings(rxnIcr <- mapCompoundsToReactions(compIcr))
  
  grps <- "M_C"
  groupDF <- getGroupDF(rxnIcr)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(rxnIcr, groups=grps)
  
  testSubsetSizesAttributes(msSubset, rxnIcr)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
  samples <- groupDF$SampleID[1:2]
  
  gSubset <- subset(rxnIcr, samples=samples)
  
  testSubsetSizesAttributes(gSubset, rxnIcr)
  expect_true(all(samples %in% colnames(gSubset$e_data)))  
  expect_true(nrow(gSubset$f_data) == length(samples))
  expect_true(all(samples %in% gSubset$f_data$SampleID))
  
})

test_that("test of subset.moduleData with one group and then two samples", {
  data("exampleProcessedPeakData")
  suppressWarnings(compIcr<- mapPeaksToCompounds(exampleProcessedPeakData, db="MetaCyc"))
  suppressWarnings(moduleIcr <- mapCompoundsToReactions(compIcr))
  
  grps <- "W_C"
  groupDF <- getGroupDF(moduleIcr)
  groupDF <- dplyr::filter(groupDF, Group %in% grps)
  samples <- groupDF$SampleID
  
  msSubset <- subset(moduleIcr, groups=grps)
  
  testSubsetSizesAttributes(msSubset, moduleIcr)
  expect_true(all(samples %in% colnames(msSubset$e_data)))  
  expect_true(nrow(msSubset$f_data) == length(samples))
  expect_true(all(samples %in% msSubset$f_data$SampleID))
  
  samples <- groupDF$SampleID[1:2]
  
  gSubset <- subset(moduleIcr, samples=samples)
  
  testSubsetSizesAttributes(gSubset, moduleIcr)
  expect_true(all(samples %in% colnames(gSubset$e_data)))  
  expect_true(nrow(gSubset$f_data) == length(samples))
  expect_true(all(samples %in% gSubset$f_data$SampleID))
  
})

