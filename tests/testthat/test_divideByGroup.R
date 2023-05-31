## Tests on divideByGroup

library(ftmsRanalysis)
context("divideByGroup function")

testCompareAttributes <- function(newObj, originalObj, excludeAttr=NA) {
  for (attr_name in setdiff(names(attributes(originalObj)), excludeAttr)) {
    expect_identical(attr(newObj, attr_name), attr(originalObj, attr_name), info=sprintf("attr_name='%s'", attr_name))
  }
}

test_that("basic tests on divideByGroup", {
  data("exampleProcessedPeakData")
  
  groups <- unique(getGroupDF(exampleProcessedPeakData)$Group)
  
  groupDdo <- divideByGroup(ftmsObj = exampleProcessedPeakData)

  expect_equal(length(groupDdo), length(groups))
  expect_true(inherits(groupDdo, "list"))
  
  ## test one subset
  i <- 2
  grp_samples <- dplyr::filter(getGroupDF(exampleProcessedPeakData), Group==groups[i])[, getFDataColName(exampleProcessedPeakData)]
  val <- groupDdo[[paste0("Group=", groups[i])]]
  
  testCompareAttributes(val, exampleProcessedPeakData, c("group_DF", "split", "valence_DF"))
 
  expect_equal(nrow(getGroupDF(val)), length(grp_samples))
  expect_true(all(grp_samples %in% colnames(val$e_data)))
  expect_true(all(grp_samples %in% getGroupDF(val)[, getFDataColName(val)]))
  
})
  