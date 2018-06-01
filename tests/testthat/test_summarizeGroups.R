## Tests on summarizeGroups function

library(fticRanalysis)
context("summarizeGroups function")

test_that("basic tests of summarizeGroups function on one group", {
  data("peakIcrProcessed")
  grp_name <- "M_S"
  grp_subset <- subset(peakIcrProcessed, groups="M_S")
  
  grp_summary <- summarizeGroups(grp_subset, summary_functions=list(count="n_present", proportion="prop_present"))
  
  expect_true(inherits(grp_summary, "peakIcrData"))
  expect_true(inherits(grp_summary, "groupSummary"))
  
  new_cnames <- paste0(grp_name, "_", c("count", "proportion"))
  expect_true(all(new_cnames %in% colnames(grp_summary$e_data)))
  
  expect_true(is.numeric(grp_summary$e_data$M_S_count))
  expect_true(is.numeric(grp_summary$e_data$M_S_proportion))
  expect_true(nrow(grp_summary$e_data) == nrow(grp_subset$e_data))
  expect_true(all(dim(grp_summary$e_meta) == dim(grp_subset$e_meta)))
  expect_true(all(colnames(grp_summary$e_meta) %in% colnames(grp_subset$e_meta)))  
  
  expect_true(ncol(grp_summary$f_data) == 4)
  expect_true(all(new_cnames %in% grp_summary$f_data$Group_Summary_Column))
  
  expect_true(sum(grp_summary$f_data$Num_Samples) == nrow(getGroupDF(grp_subset))*2)

})
  
test_that("basic tests of summarizeGroups function on object with multiple groups", {
  data("peakIcrProcessed")
  grp_names <- unique(fticRanalysis:::getGroupDF(peakIcrProcessed)$Group)

  grp_summary <- summarizeGroups(peakIcrProcessed, summary_functions=list(count="n_present", proportion="prop_present"))
  
  expect_true(inherits(grp_summary, "peakIcrData"))
  expect_true(inherits(grp_summary, "groupSummary"))
  
  new_cnames <- c(paste0(grp_names, "_", "count"), paste0(grp_names, "_", "proportion"))
  expect_true(all(new_cnames %in% colnames(grp_summary$e_data)))
  
  expect_true(is.numeric(grp_summary$e_data$M_C_count))
  expect_true(is.numeric(grp_summary$e_data$M_C_proportion))
  expect_true(nrow(grp_summary$e_data) == nrow(peakIcrProcessed$e_data))
  expect_true(all(dim(grp_summary$e_meta) == dim(peakIcrProcessed$e_meta)))
  expect_true(all(colnames(grp_summary$e_meta) %in% colnames(peakIcrProcessed$e_meta)))  
  
  expect_true(ncol(grp_summary$f_data) == 4)
  expect_true(all(new_cnames %in% grp_summary$f_data$Group_Summary_Column))
  
  expect_true(sum(grp_summary$f_data$Num_Samples) == nrow(getGroupDF(peakIcrProcessed))*2)
  
})

test_that("test summarizeGroups function with default column names", {
  data("peakIcrProcessed")
  grp_subset <- subset(peakIcrProcessed, groups="M_S")
  
  new_cnames <- paste0("M_S", "_", c("n_present", "prop_present"))
  grp_summary2 <- summarizeGroups(grp_subset, summary_functions=list("n_present", "prop_present"))
  
  expect_true(all(new_cnames %in% colnames(grp_summary2$e_data)))
  expect_true(all(new_cnames %in% grp_summary2$f_data$Group_Summary_Column))

})


test_that("test summarizeGroups function with vector instead of list", {
  data("peakIcrProcessed")
  grp_subset <- subset(peakIcrProcessed, groups="M_S")
  
  new_cnames <- paste0("M_S", "_", c("n_present", "prop_present"))
  grp_summary3 <- summarizeGroups(grp_subset, summary_functions=c("n_present", "prop_present"))
  
  expect_true(all(new_cnames %in% colnames(grp_summary3$e_data)))
  expect_true(all(new_cnames %in% grp_summary3$f_data$Group_Summary_Column))
  
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
  
  expect_true(all(paste0(getSplitVar(grp2, "Group"), "_", c("n_present", "prop_present")) %in% colnames(grp2$e_data)))
  expect_true(all(paste0(getSplitVar(grp2, "Group"), "_", c("n_present", "prop_present")) %in% grp2$f_data$Group_Summary_Column))
})