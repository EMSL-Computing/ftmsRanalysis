## Tests of assign_elemental_composition method

library(ftmsRanalysis)
context("assign_elemental_composition method")

test_that("assign_elemental_composition method", {

  # test summary without group_df
  data("examplePeakData")  
  peak2 <- assign_elemental_composition(examplePeakData)
  
  cname <- getElCompColName(peak2)
  expect_true(!is.null(cname))
  expect_true(cname %in% colnames(peak2$e_meta))
  expect_true(is.character(dplyr::pull(peak2$e_meta, cname)))
  
  missing_mol_form <- sum(is.na(dplyr::pull(peak2$e_meta, ftmsRanalysis:::getMFColName(peak2))))
  missing_el_comp <- sum(is.na(dplyr::pull(peak2$e_meta, cname)))
  expect_equal(missing_el_comp, missing_mol_form, info="number of missing elemental composition values is not the same as the number of missing mol. formulae")
  
})