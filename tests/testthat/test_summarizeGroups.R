## Tests on summarizeGroups function

library(fticRanalysis)
context("summarizeGroups function")

test_that("basic tests of summarizeGroups function", {
  data("peakIcrProcessed")
  grp_MS <- subset(peakIcrProcessed, groups="M_S")
  
  groupMSummary <- summarizeGroups(grp_MS, summary_functions=list(count="n_present", proportion="prop_present"))
  
  expect_true(inherits(groupMSummary, "peakIcrData"))
  expect_true(inherits(groupMSummary, "groupSummary"))
  
  expect_true(all(c("count", "proportion") %in% colnames(groupMSummary$e_data)))
  
  expect_true(is.numeric(groupMSummary$e_data$count))
  expect_true(is.numeric(groupMSummary$e_data$proportion))
  expect_true(nrow(groupMSummary$e_data) == nrow(grp_MS$e_data))
  expect_true(all(dim(groupMSummary$e_meta) == dim(grp_MS$e_meta)))
  expect_true(all(colnames(groupMSummary$e_meta) %in% colnames(grp_MS$e_meta)))  
  
  expect_true(ncol(groupMSummary$f_data) == 3)
  expect_true(all(c("count", "proportion") %in% groupMSummary$f_data$Group.Summary.Column))
  
  expect_true(all(groupMSummary$f_data$Num.Samples == nrow(getGroupDF(groupMSummary))))

})
  
test_that("test summarizeGroups function with default column names", {
  data("peakIcrProcessed")
  grp_MS <- subset(peakIcrProcessed, groups="M_S")
  
  groupMSummary2 <- summarizeGroups(grp_MS, summary_functions=list("n_present", "prop_present"))
  
  expect_true(all(c("n_present", "prop_present") %in% colnames(groupMSummary2$e_data)))
  expect_true(all(c("n_present", "prop_present") %in% groupMSummary2$f_data$Group.Summary.Column))

})


test_that("test summarizeGroups function with vector instead of list", {
  data("peakIcrProcessed")
  grp_MS <- subset(peakIcrProcessed, groups="M_S")
  
  groupMSummary3 <- summarizeGroups(grp_MS, summary_functions=c("n_present", "prop_present"))
  
  expect_true(all(c("n_present", "prop_present") %in% colnames(groupMSummary3$e_data)))
  expect_true(all(c("n_present", "prop_present") %in% groupMSummary3$f_data$Group.Summary.Column))
  
})


test_that("test summarizeGroups function on a ddo", {
  data("peakIcrProcessed")
  peakByGroup <- divideByGroup(peakIcrProcessed)
  grpNames <- as.character(unique(getGroupDF(peakIcrProcessed)$Group))
  
  grpSummaryDdo <- summarizeGroups(peakByGroup, summary_functions=c("n_present", "prop_present"))
  
  expect_true(inherits(grpSummaryDdo, "ddo"))
  expect_true(length(grpSummaryDdo) == length(grpNames))
  expect_true(all(paste0("Group=", grpNames) %in% unlist(getKeys(grpSummaryDdo))))
  
  grp2 <- grpSummaryDdo[[2]]$value
  expect_true(inherits(grp2, "peakIcrData"))
  expect_true(inherits(grp2, "groupSummary"))
  
  expect_true(all(c("n_present", "prop_present") %in% colnames(grp2$e_data)))
  expect_true(all(c("n_present", "prop_present") %in% grp2$f_data$Group.Summary.Column))
})