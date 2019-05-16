## Tests of assign_class function

library(ftmsRanalysis)
context("assign_class function")

test_result <- function(resObject, boundary_set, other_info="") {
  cname <- switch (boundary_set,
    bs1 = ftmsRanalysis:::getBS1ColName(resObject),
    bs2 = ftmsRanalysis:::getBS2ColName(resObject),
    bs3 = ftmsRanalysis:::getBS3ColName(resObject)
  )
#  cname <- ftmsRanalysis:::getBS1ColName(resObject)
  expect_true(!is.null(cname), info=sprintf("column name not set for %s (%s)", boundary_set, other_info))
  expect_true((cname %in% colnames(resObject$e_meta)), info=sprintf("column name not present in e_meta for %s (%s)", boundary_set, other_info))
  expect_true(is.character(dplyr::pull(resObject$e_meta, cname)), info=sprintf("class column is not character for %s (%s)", boundary_set, other_info))
}

test_boundary_set <- function(boundary_set) {
  peak2 <- assign_class(examplePeakData, boundary_set = boundary_set)
  test_result(peak2, boundary_set, "peakData without pre-calculated elemental ratios")
  
  peak3 <- assign_class(exampleProcessedPeakData, boundary_set = boundary_set, calc_ratios=TRUE)
  test_result(peak3, boundary_set, "peakData with pre-calculated elemental ratios and calc_rations=TRUE")
  
  peak4 <- assign_class(exampleProcessedPeakData, boundary_set = boundary_set, calc_ratios=FALSE)
  test_result(peak4, boundary_set, "peakData with pre-calculated elemental ratios")
  
  cmp <- suppressWarnings(suppressMessages(mapPeaksToCompounds(exampleProcessedPeakData)))
  cmp2 <- assign_class(cmp, boundary_set = boundary_set)
  test_result(cmp2, boundary_set, "compoundData with pre-calculated elemental ratios")
  
}

test_that("tests of assign_class function with bs1", {
  
  test_boundary_set("bs1")
  
})

test_that("tests of assign_class function with bs2", {
  
  test_boundary_set("bs2")
  
})

test_that("tests of assign_class function with bs3", {
  
  test_boundary_set("bs3")
  
})

test_that("tests of assign_class function that should fail", {
  
  expect_error(peak2 <- assign_class(examplePeakData, boundary_set = "hello"))
  expect_error(peak2 <- assign_class(examplePeakData, boundary_set = "bs1", calc_ratios = FALSE))
  
})
