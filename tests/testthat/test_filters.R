## Basic functionality tests for peakIcrData objects

library(fticRanalysis)
context("filtering on peakIcrData objects")

test_that("mass filters work correctly on peakIcrData", {
  data("peakIcrData")
  
  filtData <- mass_filter(peakIcrData)
  expect_true(inherits(filtData, "massFilt"))
  expect_true(inherits(filtData, "data.frame"))
  expect_true(ncol(filtData) == 2)
  expect_true(all(c(getEDataColName(peakIcrData), getMassColName(peakIcrData)) %in% colnames(filtData)))
  
  peakIcrData2 <- applyFilt(filtData, peakIcrData, min_mass = 200, max_mass = 900)
  expect_true(inherits(peakIcrData2, "peakIcrData"))
  new.masses <- as.numeric(peakIcrData2$e_meta[, getMassColName(peakIcrData2)])
  expect_true(all(new.masses >= 200))
  expect_true(all(new.masses <= 900))
  
  expect_true(nrow(peakIcrData$e_data) >= nrow(peakIcrData2$e_data))
  
  expect_true(!is.null(attr(peakIcrData2, "filter")))
  expect_true(!is.null(attr(peakIcrData2, "filter")$massFilt))
  ## TODO do we want to test more things about the attribute here?
  
  # summary method
  filtSumm <- summary(filtData)
  expect_true(ncol(filtSumm) == 2)
  expect_true(inherits(filtSumm, "table"))
  expect_true(getMassColName(peakIcrData) %in% stringr::str_trim(colnames(filtSumm)))
  
  filtSumm2 <- summary(filtData, min_mass=200, max_mass=900)
  expect_true(length(filtSumm2) == 3)
  expect_true(inherits(filtSumm2, "list"))
  expect_true(all(c("Original_Num_Peaks", "Removed_Peaks", "Retained_Peaks") %in% names(filtSumm2)))
  
  # test some things that should fail  
  expect_error(tmp <- applyFilt(filtData, peakIcrData2, min_mass = 500, max_mass = 600))
  expect_error(tmp <- applyFilt(filtData, peakIcrData, min_mass = "hello", max_mass = 600))
  
})


test_that("molecule filters work correctly on peakIcrData", {
  data("peakIcrData")
  
  filtData <- molecule_filter(peakIcrData)
  expect_true(inherits(filtData, "moleculeFilt"))
  expect_true(inherits(filtData, "data.frame"))
  expect_true(ncol(filtData) == 2)
  expect_true(all(c(getEDataColName(peakIcrData), "Num_Observations") %in% colnames(filtData)))
  
  peakIcrData2 <- applyFilt(filtData, peakIcrData, min_num = 2)
  
  expect_true(inherits(peakIcrData2, "peakIcrData"))
  retainedPeaks <- as.vector(dplyr::filter(filtData, Num_Observations >=2)[, getEDataColName(peakIcrData2)])
  expect_true(all(retainedPeaks %in% peakIcrData2$e_data[, getEDataColName(peakIcrData2)]))
  
  expect_true(nrow(peakIcrData$e_data) >= nrow(peakIcrData2$e_data))
  
  expect_true(!is.null(attr(peakIcrData2, "filter")))
  expect_true(!is.null(attr(peakIcrData2, "filter")$moleculeFilt))
  ## TODO do we want to test more things about the attribute here?
  
  # summary method
  filtSumm <- summary(filtData)
  expect_true(ncol(filtSumm) == 2)
  expect_true(inherits(filtSumm, "table"))
  expect_true("Num_Observations" %in% stringr::str_trim(colnames(filtSumm)))
  
  filtSumm2 <- summary(filtData, min_num=2)
  expect_true(length(filtSumm2) == 3)
  expect_true(inherits(filtSumm2, "list"))
  expect_true(all(c("Original_Num_Peaks", "Removed_Peaks", "Retained_Peaks") %in% names(filtSumm2)))
  
  # test some things that should fail  
  expect_error(tmp <- applyFilt(filtData, peakIcrData, min_num=-1))
  expect_error(tmp <- applyFilt(filtData, peakIcrData, min_num="hello"))
  
})

test_that("formula filters work correctly on peakIcrData", {
  data("peakIcrData")
  
  filtData <- formula_filter(peakIcrData)
  expect_true(inherits(filtData, "formulaFilt"))
  expect_true(inherits(filtData, "data.frame"))
  expect_true(ncol(filtData) == 2)
  expect_true(all(c(getEDataColName(peakIcrData), "Formula_Assigned") %in% colnames(filtData)))
  
  ## Remove peaks WITHOUT formulas
  peakIcrData2 <- applyFilt(filtData, peakIcrData, remove = 'NoFormula')
  
  expect_true(inherits(peakIcrData2, "peakIcrData"))
  retainedPeaks <- as.vector(dplyr::filter(filtData, Formula_Assigned)[, getEDataColName(peakIcrData2)])
  expect_true(all(retainedPeaks %in% peakIcrData2$e_data[, getEDataColName(peakIcrData2)]))
  
  expect_true(nrow(peakIcrData$e_data) >= nrow(peakIcrData2$e_data))
  
  expect_true(!is.null(attr(peakIcrData2, "filter")))
  expect_true(!is.null(attr(peakIcrData2, "filter")$moleculeFilt))
  ## TODO do we want to test more things about the attribute here?
  
  # summary method
  filtSumm <- summary(filtData)
  expect_true(ncol(filtSumm) == 2)
  expect_true(inherits(filtSumm, "table"))
  expect_true("Formula_Assigned" %in% stringr::str_trim(colnames(filtSumm)))
  
  filtSumm2 <- summary(filtData, remove="NoFormula")
  expect_true(length(filtSumm2) == 3)
  expect_true(inherits(filtSumm2, "list"))
  expect_true(all(c("Original_Num_Peaks", "Removed_Peaks", "Retained_Peaks") %in% names(filtSumm2)))
  
  ## Remove peaks WITH formulas
  peakIcrData3 <- applyFilt(filtData, peakIcrData, remove = 'Formula')
  
  expect_true(inherits(peakIcrData3, "peakIcrData"))
  retainedPeaks <- as.vector(dplyr::filter(filtData, !Formula_Assigned)[, getEDataColName(peakIcrData3)])
  expect_true(all(retainedPeaks %in% peakIcrData3$e_data[, getEDataColName(peakIcrData3)]))
  
  expect_true(nrow(peakIcrData$e_data) >= nrow(peakIcrData3$e_data))
  
  expect_true(!is.null(attr(peakIcrData3, "filter")))
  expect_true(!is.null(attr(peakIcrData3, "filter")$moleculeFilt))
  ## TODO do we want to test more things about the attribute here?
  
  # test some things that should fail  
  expect_error(tmp <- applyFilt(filtData, peakIcrData, remove="invalid"))
  
})
