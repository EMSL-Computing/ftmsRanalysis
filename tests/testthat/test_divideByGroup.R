## Tests on divideByGroup

library(fticRanalysis)
context("divideByGroup function")

testCompareAttributes <- function(newObj, originalObj, excludeAttr=NA) {
  for (attr_name in setdiff(names(attributes(originalObj)), excludeAttr)) {
    expect_identical(attr(newObj, attr_name), attr(originalObj, attr_name), info=sprintf("attr_name='%s'", attr_name))
  }
}

test_that("basic tests on divideByGroup", {
  data("exampleProcessedPeakData")
  
  groups <- unique(getGroupDF(exampleProcessedPeakData)$Group)
  
  groupDdo <- divideByGroup(icrData = exampleProcessedPeakData)

  expect_equal(length(groupDdo), length(groups))
  expect_true(inherits(groupDdo, "ddo"))
  
  ## test one subset
  i <- 2
  grp_samples <- dplyr::filter(getGroupDF(exampleProcessedPeakData), Group==groups[i])[, getFDataColName(exampleProcessedPeakData)]
  val <- groupDdo[[paste0("Group=", groups[i])]]$value
  
  testCompareAttributes(val, exampleProcessedPeakData, c("group_DF", "split"))
 
  expect_equal(nrow(getGroupDF(val)), length(grp_samples))
  expect_true(all(grp_samples %in% colnames(val$e_data)))
  expect_true(all(grp_samples %in% getGroupDF(val)[, getFDataColName(val)]))
  
})
  