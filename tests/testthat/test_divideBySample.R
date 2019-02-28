context("divideBySample function")

testCompareAttributes <- function(newObj, originalObj, excludeAttr=NA) {
  for (attr_name in setdiff(names(attributes(originalObj)), excludeAttr)) {
    expect_identical(attr(newObj, attr_name), attr(originalObj, attr_name), info=sprintf("attr_name='%s'", attr_name))
  }
}

test_that("basic tests on divideBySample", {
  data("exampleProcessedPeakData")
  
  samples <- unique(exampleProcessedPeakData$f_data[, getFDataColName(exampleProcessedPeakData)])
  sampleDdo <- divideBySample(icrData = exampleProcessedPeakData)
  
  expect_equal(length(sampleDdo), length(samples))
  expect_true(inherits(sampleDdo, "ddo"))
  
  ## test one subset
  i <- 2
  val <- sampleDdo[[paste0("SampleID=", samples[i])]]$value
  
  testCompareAttributes(val, exampleProcessedPeakData, c("group_DF", "split"))
  
  expect_equal(nrow(getGroupDF(val)), 1)
  expect_true(samples[i] %in% colnames(val$e_data))
  expect_true(samples[i] %in% getGroupDF(val)[, getFDataColName(val)])
  expect_equal(ncol(val$e_data), 2)  
  
})
