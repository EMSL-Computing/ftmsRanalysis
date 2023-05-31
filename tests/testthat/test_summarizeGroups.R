## Tests on summarizeGroups function

library(ftmsRanalysis)
context("summarizeGroups function")

test_that("basic tests of summarizeGroups function on one group", {
  data("exampleProcessedPeakData")
  grp_name <- "M_S"
  grp_subset <- subset(exampleProcessedPeakData, groups="M_S")
  
  grp_summary <- summarizeGroups(grp_subset, summary_functions=c("n_present", "prop_present"))
  
  expect_true(inherits(grp_summary, "peakData"))
  expect_true(inherits(grp_summary, "groupSummary"))
  
  new_cnames <- paste0(grp_name, "_", c("n_present", "prop_present"))
  expect_true(all(new_cnames %in% colnames(grp_summary$e_data)))
  
  expect_true(is.numeric(grp_summary$e_data$M_S_n_present))
  expect_true(is.numeric(grp_summary$e_data$M_S_prop_present))
  expect_true(nrow(grp_summary$e_data) == nrow(grp_subset$e_data))
  expect_true(all(dim(grp_summary$e_meta) == dim(grp_subset$e_meta)))
  expect_true(all(colnames(grp_summary$e_meta) %in% colnames(grp_subset$e_meta)))  
  
  expect_true(ncol(grp_summary$f_data) == 4)
  expect_true(all(new_cnames %in% grp_summary$f_data$Group_Summary_Column))
  
  expect_true(sum(grp_summary$f_data$Num_Samples) == nrow(getGroupDF(grp_subset))*2)

})
  
test_that("basic tests of summarizeGroups function on object with multiple groups", {
  data("exampleProcessedPeakData")
  grp_names <- unique(ftmsRanalysis:::getGroupDF(exampleProcessedPeakData)$Group)

  grp_summary <- summarizeGroups(exampleProcessedPeakData, summary_functions=c("n_present", "prop_present"))
  
  expect_true(inherits(grp_summary, "peakData"))
  expect_true(inherits(grp_summary, "groupSummary"))
  
  new_cnames <- c(paste0(grp_names, "_", "n_present"), paste0(grp_names, "_", "prop_present"))
  expect_true(all(new_cnames %in% colnames(grp_summary$e_data)))
  
  expect_true(is.numeric(grp_summary$e_data$M_C_n_present))
  expect_true(is.numeric(grp_summary$e_data$M_C_prop_present))
  expect_true(nrow(grp_summary$e_data) == nrow(exampleProcessedPeakData$e_data))
  expect_true(all(dim(grp_summary$e_meta) == dim(exampleProcessedPeakData$e_meta)))
  expect_true(all(colnames(grp_summary$e_meta) %in% colnames(exampleProcessedPeakData$e_meta)))  
  
  expect_true(ncol(grp_summary$f_data) == 4)
  expect_true(all(new_cnames %in% grp_summary$f_data$Group_Summary_Column))
  
  expect_true(sum(grp_summary$f_data$Num_Samples) == nrow(getGroupDF(exampleProcessedPeakData))*2)
  
})


test_that("test summarizeGroups function on a ddo", {
  data("exampleProcessedPeakData")
  peakByGroup <- divideByGroup(exampleProcessedPeakData)
  grpNames <- as.character(unique(getGroupDF(exampleProcessedPeakData)$Group))
  
  grpSummaryDdo <- summarizeGroups(peakByGroup, summary_functions=c("n_present", "prop_present"))
  
  expect_true(inherits(grpSummaryDdo, "list"))
  expect_true(length(grpSummaryDdo) == length(grpNames))
  expect_true(all(paste0("Group=", grpNames) %in% names(grpSummaryDdo)))
  
  grp2 <- grpSummaryDdo[[2]]
  expect_true(inherits(grp2, "peakData"))
  expect_true(inherits(grp2, "groupSummary"))
  
  expect_true(all(paste0(attr(grp2, "group_DF")$Group, "_", c("n_present", "prop_present")) %in% colnames(grp2$e_data)))
  expect_true(all(paste0(attr(grp2, "group_DF")$Group, "_", c("n_present", "prop_present")) %in% grp2$f_data$Group_Summary_Column))
})