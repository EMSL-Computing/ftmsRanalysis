## Basic summary methods

library(ftmsRanalysis)
context("summary method")

test_that("summary.ftmsData method", {

  # test summary without group_df
  data("examplePeakData")  
  summObj <- summary(examplePeakData)  
  expect_true(inherits(summObj, "ftmsDataSummary"))
  expect_true(inherits(summObj, "list"))
  expect_equal(object = length(summObj), expected = 3)  
  expect_true(all(c("Samples", "Molecules", "Percent_Missing") %in% names(summObj)))
  
  # test summary with group_df
  data("exampleProcessedPeakData")  
  summObj2 <- summary(exampleProcessedPeakData)  
  expect_true(inherits(summObj2, "ftmsDataSummary"))
  expect_true(inherits(summObj2, "list"))
  expect_equal(object = length(summObj2), expected = 4)  
  expect_true(all(c("Samples", "Molecules", "Percent_Missing", "Group_Sizes") %in% names(summObj2)))
  
  # test print function for summary object
  con <- textConnection(NULL, open="w")
  sink(file = con)
  print(summObj2)
  sink() #closes sink above
  close(con)
})