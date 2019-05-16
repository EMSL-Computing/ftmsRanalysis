## Tests of getVanKrevelenCategoryBounds function

library(ftmsRanalysis)
context("getVanKrevelenCategoryBounds function")

test_result <- function(resObject, boundary_set) {
  boundary_set <- sprintf("boundary_set = '%s'", boundary_set)
  expect_true(inherits(resObject, "list"), info=boundary_set)
  expect_true(length(resObject) == 2, info=boundary_set)
  expect_identical(names(resObject), c("VKbounds", "VKlogic"), info=boundary_set)
  expect_true(inherits(resObject[[1]], "data.frame"), info=boundary_set)
  expect_true(inherits(resObject[[2]], "data.frame"), info=boundary_set)
  expect_true(all(unlist(lapply(resObject$VKbounds, is.numeric))), info=boundary_set)
}

test_that("tests of getVanKrevelenCategoryBounds function", {
  
  res1 <- getVanKrevelenCategoryBounds("bs1")
  test_result(res1, "bs1")
  
  res2 <- getVanKrevelenCategoryBounds("bs2")
  test_result(res2, "bs2")
  
  res3 <- getVanKrevelenCategoryBounds("bs3")
  test_result(res3, "bs3")
  
})
