## comparisonMatrix tests

library(fticRanalysis)
context("comparisonMatrix function")


test_that("creating comparison matrix works correctly, when group_DF is absent", {
  data("peakIcrData")
  
  ngroups <- length(unique(peakIcrData$f_data$SampleID))
  max.pairs <- choose(ngroups, 2)
  
  # all comparisons
  all <- comparisonMatrix(icrObj = peakIcrData, comparisons = "all", control = NULL)
  expect_true(nrow(all) == 2)
  expect_true(ncol(all) == max.pairs)
  
  # one-factor changing comparisons
  expect_error(one <- comparisonMatrix(icrObj = peakIcrData, comparisons = "one-factor", control = NULL), 
               regexp="The 'one-factor' specification cannot be used without a group data frame")

  # specific comparisons
  spec <- comparisonMatrix(icrObj = peakIcrData, comparisons = list(c("EM0011_sample","EM0061_sample"),c("EW0113_sample","EW0163_sample")), control = NULL)
  expect_true(nrow(spec) == 2)
  expect_true(ncol(spec) == 2)
  
  # compare against control
  contr <- comparisonMatrix(icrObj = peakIcrData, comparisons = "control", control = "EM0011_sample")
  expect_true(nrow(contr) == 2)
  expect_true(ncol(contr) == ngroups - 1)
  
  # Trying to compare groups not found in dataset
  expect_error(comparisonMatrix(icrObj = peakIcrData, comparisons = list(c("EM0011sample","EM0061_sample"),c("EW0113_sample","EW0163_sample")), control = NULL), regexp = "not all groups specified in pairs parameter were found in the data")
  
  #invalid data_scale value: should throw an error
  expect_error(comparisonMatrix(icrObj = peakIcrData, comparisons = "mymethod", control = NULL), regexp = "check that comparisons argument is")
})


test_that("creating comparison matrix works correctly, when group_DF is present", {
  data("peakIcrProcessed")
  
  ngroups <- length(unique(attr(peakIcrProcessed, "group_DF")$Group))
  max.pairs <- choose(ngroups, 2)

  # all comparisons
  all <- comparisonMatrix(icrObj = peakIcrProcessed, comparisons = "all", control = NULL)
  expect_true(nrow(all) == 2)
  expect_true(ncol(all) == max.pairs)
  
  # one-factor changing comparisons
  one <- comparisonMatrix(icrObj = peakIcrProcessed, comparisons = "one-factor", control = NULL)
  expect_true(nrow(one) == 2)
  expect_true(ncol(one) == 4)
  
  # specific comparisons
  spec <- comparisonMatrix(icrObj = peakIcrProcessed, comparisons = list(c("M_S","W_S"),c("M_C","W_C")), control = NULL)
  expect_true(nrow(spec) == 2)
  expect_true(ncol(spec) == 2)
  
  # compare against control
  contr <- comparisonMatrix(icrObj = peakIcrProcessed, comparisons = "control", control = "M_C")
  expect_true(nrow(contr) == 2)
  expect_true(ncol(contr) == ngroups - 1)

  # Trying to compare groups not found in dataset
  expect_error(comparisonMatrix(icrObj = peakIcrProcessed, comparisons = list(c("M_S","W_S"),c("M_C","WC")), control = NULL), regexp = "not all groups specified in pairs parameter were found in the data")
  
  #invalid data_scale value: should throw an error
  expect_error(comparisonMatrix(icrObj = peakIcrProcessed, comparisons = "mymethod", control = NULL), regexp = "check that comparisons argument is")
})
