## Tests on combinePeaksWithSameFormula function

library(ftmsRanalysis)
context("combinePeaksWithSameFormula function")

test_that("test of combinePeaksWithSameFormula function with abundance data", {
  data("exampleProcessedPeakData")
  
  ftmsObj <- exampleProcessedPeakData
  obj2 <- combinePeaksWithSameFormula(ftmsObj)
  
  expect_true(nrow(obj2$e_data) <= nrow(ftmsObj$e_data))
  
  new_mf <- dplyr::pull(obj2$e_meta, getMFColName(obj2))
  new_mf <- new_mf[!is.na(new_mf)]
  expect_true(anyDuplicated(new_mf)==0)
  
  original_mf <- dplyr::pull(ftmsObj$e_meta, getMFColName(ftmsObj))
  dup_mf <- unique(original_mf[!is.na(original_mf) & duplicated(original_mf)])
  ind_not_dup_old <- !(original_mf %in% dup_mf)
  ind_not_dup_new <- !(dplyr::pull(obj2$e_meta, getMFColName(obj2)) %in% dup_mf)

  # all values from non-duplicated rows of e_data should be identical between old and new
  expect_true(identical(ftmsObj$e_data[ind_not_dup_old, ], obj2$e_data[ind_not_dup_new, ]))
})

test_that("test of combinePeaksWithSameFormula function with log2 data", {
  data("exampleProcessedPeakData")
  
  ftmsObj <- exampleProcessedPeakData
  ftmsObj <- edata_transform(ftmsObj, "log2")
  obj2 <- combinePeaksWithSameFormula(ftmsObj)
  
  expect_true(nrow(obj2$e_data) <= nrow(ftmsObj$e_data))
  expect_true(getDataScale(obj2) == "log2")
  
  new_mf <- dplyr::pull(obj2$e_meta, getMFColName(obj2))
  new_mf <- new_mf[!is.na(new_mf)]
  expect_true(anyDuplicated(new_mf)==0)
  
  original_mf <- dplyr::pull(ftmsObj$e_meta, getMFColName(ftmsObj))
  dup_mf <- unique(original_mf[!is.na(original_mf) & duplicated(original_mf)])
  ind_not_dup_old <- !(original_mf %in% dup_mf)
  ind_not_dup_new <- !(dplyr::pull(obj2$e_meta, getMFColName(obj2)) %in% dup_mf)
  
  # all values from non-duplicated rows of e_data should be identical between old and new
  expect_true(all.equal(ftmsObj$e_data[ind_not_dup_old, ], obj2$e_data[ind_not_dup_new, ], check.attributes=FALSE))
  expect_true(all(colnames(ftmsObj$e_data[ind_not_dup_old, ]) == colnames(obj2$e_data[ind_not_dup_new, ])))
})


test_that("test of combinePeaksWithSameFormula function with presence/absence data", {
  data("exampleProcessedPeakData")
  
  ftmsObj <- exampleProcessedPeakData
  ftmsObj <- edata_transform(ftmsObj, "pres")
  obj2 <- combinePeaksWithSameFormula(ftmsObj)
  
  expect_true(nrow(obj2$e_data) <= nrow(ftmsObj$e_data))
  expect_true(getDataScale(obj2) == "pres")
  
  new_mf <- dplyr::pull(obj2$e_meta, getMFColName(obj2))
  new_mf <- new_mf[!is.na(new_mf)]
  expect_true(anyDuplicated(new_mf)==0)
  
  original_mf <- dplyr::pull(ftmsObj$e_meta, getMFColName(ftmsObj))
  dup_mf <- unique(original_mf[!is.na(original_mf) & duplicated(original_mf)])
  ind_not_dup_old <- !(original_mf %in% dup_mf)
  ind_not_dup_new <- !(dplyr::pull(obj2$e_meta, getMFColName(obj2)) %in% dup_mf)
  
  # all values from non-duplicated rows of e_data should be identical between old and new
  expect_true(identical(ftmsObj$e_data[ind_not_dup_old, ], obj2$e_data[ind_not_dup_new, ]))
  
  # all edata values must be <= 1 to be presence/absence
  sample_colnames <- as.character(dplyr::pull(obj2$f_data, getFDataColName(obj2)))
  sample_colnames <- sample_colnames[sample_colnames %in% colnames(obj2$e_data)]
  expect_true(all(obj2$e_data[, sample_colnames] <= 1))
  
})

