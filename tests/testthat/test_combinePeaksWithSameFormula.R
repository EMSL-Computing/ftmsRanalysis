## Tests on combinePeaksWithSameFormula function

library(fticRanalysis)
context("combinePeaksWithSameFormula function")

test_that("test of combinePeaksWithSameFormula function with abundance data", {
  data("exampleProcessedPeakData")
  
  icrData <- exampleProcessedPeakData
  icr2 <- combinePeaksWithSameFormula(icrData)
  
  expect_true(nrow(icr2$e_data) <= nrow(icrData$e_data))
  
  new_mf <- dplyr::pull(icr2$e_meta, getMFColName(icr2))
  new_mf <- new_mf[!is.na(new_mf)]
  expect_true(anyDuplicated(new_mf)==0)
  
  original_mf <- dplyr::pull(icrData$e_meta, getMFColName(icrData))
  dup_mf <- unique(original_mf[!is.na(original_mf) & duplicated(original_mf)])
  ind_not_dup_old <- !(original_mf %in% dup_mf)
  ind_not_dup_new <- !(dplyr::pull(icr2$e_meta, getMFColName(icr2)) %in% dup_mf)

  # all values from non-duplicated rows of e_data should be identical between old and new
  expect_true(identical(icrData$e_data[ind_not_dup_old, ], icr2$e_data[ind_not_dup_new, ]))
})

test_that("test of combinePeaksWithSameFormula function with log2 data", {
  data("exampleProcessedPeakData")
  
  icrData <- exampleProcessedPeakData
  icrData <- edata_transform(icrData, "log2")
  icr2 <- combinePeaksWithSameFormula(icrData)
  
  expect_true(nrow(icr2$e_data) <= nrow(icrData$e_data))
  expect_true(getDataScale(icr2) == "log2")
  
  new_mf <- dplyr::pull(icr2$e_meta, getMFColName(icr2))
  new_mf <- new_mf[!is.na(new_mf)]
  expect_true(anyDuplicated(new_mf)==0)
  
  original_mf <- dplyr::pull(icrData$e_meta, getMFColName(icrData))
  dup_mf <- unique(original_mf[!is.na(original_mf) & duplicated(original_mf)])
  ind_not_dup_old <- !(original_mf %in% dup_mf)
  ind_not_dup_new <- !(dplyr::pull(icr2$e_meta, getMFColName(icr2)) %in% dup_mf)
  
  # all values from non-duplicated rows of e_data should be identical between old and new
  expect_true(identical(icrData$e_data[ind_not_dup_old, ], icr2$e_data[ind_not_dup_new, ]))
  
})


test_that("test of combinePeaksWithSameFormula function with presence/absence data", {
  data("exampleProcessedPeakData")
  
  icrData <- exampleProcessedPeakData
  icrData <- edata_transform(icrData, "pres")
  icr2 <- combinePeaksWithSameFormula(icrData)
  
  expect_true(nrow(icr2$e_data) <= nrow(icrData$e_data))
  expect_true(getDataScale(icr2) == "pres")
  
  new_mf <- dplyr::pull(icr2$e_meta, getMFColName(icr2))
  new_mf <- new_mf[!is.na(new_mf)]
  expect_true(anyDuplicated(new_mf)==0)
  
  original_mf <- dplyr::pull(icrData$e_meta, getMFColName(icrData))
  dup_mf <- unique(original_mf[!is.na(original_mf) & duplicated(original_mf)])
  ind_not_dup_old <- !(original_mf %in% dup_mf)
  ind_not_dup_new <- !(dplyr::pull(icr2$e_meta, getMFColName(icr2)) %in% dup_mf)
  
  # all values from non-duplicated rows of e_data should be identical between old and new
  expect_true(identical(icrData$e_data[ind_not_dup_old, ], icr2$e_data[ind_not_dup_new, ]))
  
  # all edata values must be <= 1 to be presence/absence
  sample_colnames <- as.character(dplyr::pull(icr2$f_data, getFDataColName(icr2)))
  sample_colnames <- sample_colnames[sample_colnames %in% colnames(icr2$e_data)]
  expect_true(all(icr2$e_data[, sample_colnames] <= 1))
  
})

