## Test edata_replace method

library(ftmsRanalysis)
context("group_designation method")

test_that("Basic tests of group_designation method", {

  data("examplePeakData")
  samp_cname <- getFDataColName(examplePeakData)
  
  # 1 main effect, 1 covariate
  peak2 <- group_designation(examplePeakData, main_effects="Location", covariates = "Crop.Flora")
  groupDF <- getGroupDF(peak2)
  expect_true(!is.null(groupDF))
  expect_true(inherits(groupDF, "data.frame"))
  expect_true(ncol(groupDF) == 2)
  expect_true(samp_cname %in% colnames(groupDF))
  expect_true("Group" %in% colnames(groupDF))
  
  # 2 main effects, 0 covariates
  peak3 <- group_designation(examplePeakData, main_effects=c("Location", "Crop.Flora"))
  groupDF <- getGroupDF(peak3)
  expect_true(!is.null(groupDF))
  expect_true(inherits(groupDF, "data.frame"))
  expect_true(ncol(groupDF) == 4)
  expect_true(samp_cname %in% colnames(groupDF))
  expect_true("Group" %in% colnames(groupDF))
  expect_true(all(c("Location", "Crop.Flora") %in% colnames(groupDF)))
  
  # Augment f_data with additional columns
  examplePeakData$f_data$Test_Condition1 <- c(1, rep(2, times=nrow(examplePeakData$f_data)-1))
  examplePeakData$f_data$Test_Condition2 <- c(rep("G1", times=nrow(examplePeakData$f_data)-1), NA)
  examplePeakData$f_data$Test_Condition3 <- rep(factor("G1", levels=c("G1", "G2")), times=nrow(examplePeakData$f_data))
  
  # main effect with a group of 1, which should be removed
  peak4 <- expect_warning(group_designation(examplePeakData, main_effects=c("Test_Condition1")))
  groupDF <- getGroupDF(peak4)
  removed_sample <- examplePeakData$f_data[1, samp_cname]
  expect_true(!(removed_sample %in% dplyr::pull(groupDF, samp_cname)))
  expect_true(!(removed_sample %in% colnames(peak4$e_data)))
 
  # main effect where 1 sample has a group assignment of NA, which should be removed
  peak5 <- group_designation(examplePeakData, main_effects=c("Test_Condition2"))
  groupDF <- getGroupDF(peak5)
  removed_sample <- examplePeakData$f_data[nrow(examplePeakData$f_data), samp_cname]
  expect_true(!(removed_sample %in% dplyr::pull(groupDF, samp_cname)))
  expect_true(!(removed_sample %in% colnames(peak5$e_data)))
  
  # 2 main effects, 0 covariates with var.name=TRUE
  peak6 <- group_designation(examplePeakData, main_effects=c("Location", "Crop.Flora"), var.name = TRUE)
  groupDF <- getGroupDF(peak6)
  expect_true(!is.null(groupDF))
  expect_true(inherits(groupDF, "data.frame"))
  expect_true(ncol(groupDF) == 4)
  expect_true(samp_cname %in% colnames(groupDF))
  expect_true("Group" %in% colnames(groupDF))
  expect_true(all(c("Location", "Crop.Flora") %in% colnames(groupDF)))
  # make sure main effects are used to construct group names
  expect_true(all(grepl("Location", dplyr::pull(groupDF, "Group"))))
  expect_true(all(grepl("Crop.Flora", dplyr::pull(groupDF, "Group"))))
  
})

test_that("Tests of group_designation method that should throw errors", {
  expect_error(group_designation("not a real ftmsData object", main_effects = "hello"))
  expect_error(group_designation(examplePeakData, main_effects="not a real column", covariates = "Crop.Flora"))
  expect_error(group_designation(examplePeakData, main_effects=NA))
  expect_error(group_designation(examplePeakData, main_effects=character(0)))
  expect_error(group_designation(examplePeakData, main_effects=c("Location", "Crop.Flora"), covariates = c("Location", "Crop.Flora", "Block")))
})