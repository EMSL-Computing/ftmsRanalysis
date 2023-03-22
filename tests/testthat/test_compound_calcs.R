## Tests of compound_calcs function

library(ftmsRanalysis)
context("compound_calcs function")

test_aroma_result <- function(peakObj) {
  aroma_cname <- ftmsRanalysis:::getAromaColName(peakObj)
  aroma_mod_cname <- ftmsRanalysis:::getModAromaColName(peakObj)
  expect_true(all(c(aroma_cname, aroma_mod_cname) %in% colnames(peakObj$e_meta)))
  expect_true(all(!is.null(c(aroma_cname, aroma_mod_cname))))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, aroma_cname)))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, aroma_mod_cname)))
  missing_mol_form <- sum(is.na(dplyr::pull(peakObj$e_meta, ftmsRanalysis:::getMFColName(peakObj))))
  missing_aroma <- sum(is.na(dplyr::pull(peakObj$e_meta, aroma_cname)))
  missing_aroma_mod <- sum(is.na(dplyr::pull(peakObj$e_meta, aroma_mod_cname)))
  expect_equal(missing_aroma, missing_mol_form, info="number of missing aroma values is not the same as the number of missing mol. formulae")
  expect_equal(missing_aroma_mod, missing_mol_form, info="number of missing aroma mod values is not the same as the number of missing mol. formulae")
}

test_dbe_result <- function(peakObj) {
  dbe_cname <- ftmsRanalysis:::getDBEColName(peakObj)
  dbeo_cname <- ftmsRanalysis:::getDBEoColName(peakObj)
  dbeai_cname <- ftmsRanalysis:::getDBEAIColName(peakObj)
  expect_true(all(c(dbe_cname, dbeo_cname, dbeai_cname) %in% colnames(peakObj$e_meta)))
  expect_true(all(!is.null(c(dbe_cname, dbeo_cname, dbeai_cname))))
  
  expect_true(all(sapply(dplyr::select(tidyselect::all_of(peakObj$e_meta), dbe_cname), is.numeric)))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, dbeo_cname)))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, dbeai_cname)))
  expect_true(!is.null(attr(peakObj, "valence_DF")) & all(rownames(attr(peakObj, "valence_DF")) %in% colnames(peakObj$e_meta))) # check non-null since all(NULL) = TRUE
  
  missing_mol_form <- sum(is.na(dplyr::pull(peakObj$e_meta, ftmsRanalysis:::getMFColName(peakObj))))
  missing_dbe <- sum(sapply(dplyr::select(tidyselect::all_of(peakObj$e_meta), dbe_cname), is.na))/length(dbe_cname) # divide by number of dbe columns for correct comparison
  missing_dbeo <- sum(is.na(dplyr::pull(peakObj$e_meta, dbeo_cname)))
  missing_dbeai <- sum(is.na(dplyr::pull(peakObj$e_meta, dbeai_cname)))
  
  expect_equal(missing_dbe, missing_mol_form, info="number of missing DBE values is not the same as the number of missing mol. formulae")
  expect_equal(missing_dbeo, missing_mol_form, info="number of missing DBE_O values is not the same as the number of missing mol. formulae")
  expect_equal(missing_dbeai, missing_mol_form, info="number of missing DBE_AI values is not the same as the number of missing mol. formulae")
}

test_gibbs_result <- function(peakObj) {
  gibbs_cname <- ftmsRanalysis:::getGibbsColName(peakObj)
  expect_true(!is.null(gibbs_cname))
  expect_true(all(c(gibbs_cname) %in% colnames(peakObj$e_meta)))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, gibbs_cname)))
  missing_mol_form <- sum(is.na(dplyr::pull(peakObj$e_meta, ftmsRanalysis:::getMFColName(peakObj))))
  missing_gibbs <- sum(is.na(dplyr::pull(peakObj$e_meta, gibbs_cname)))
  expect_equal(missing_gibbs, missing_mol_form, info="number of missing gibbs values is not the same as the number of missing mol. formulae")
}

test_kendrick_result <- function(peakObj) {
  kmass_cname <- ftmsRanalysis:::getKendrickMassColName(peakObj)
  kdefect_cname <- ftmsRanalysis:::getKendrickDefectColName(peakObj)
  
  expect_true(all(c(kmass_cname, kdefect_cname) %in% colnames(peakObj$e_meta)))
  expect_true(all(!is.null(c(kmass_cname, kdefect_cname))))
  
  expect_true(all(sapply(dplyr::select(peakObj$e_meta, kmass_cname), is.numeric)))
  expect_true(all(sapply(dplyr::select(peakObj$e_meta, kdefect_cname), is.numeric)))
  
  missing_kmass <- sum(sapply(dplyr::select(peakObj$e_meta, kmass_cname), is.na))
  missing_kdefect <- sum(sapply(dplyr::select(peakObj$e_meta, kdefect_cname), is.na))
  expect_equal(missing_kmass, 0)
  expect_equal(missing_kdefect, 0)
}

test_nosc_result <- function(peakObj) {
  nosc_cname <- ftmsRanalysis:::getNOSCColName(peakObj)
  expect_true(all(c(nosc_cname) %in% colnames(peakObj$e_meta)))
  expect_true(!is.null(nosc_cname))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, nosc_cname)))
  missing_mol_form <- sum(is.na(dplyr::pull(peakObj$e_meta, ftmsRanalysis:::getMFColName(peakObj))))
  missing_nosc <- sum(is.na(dplyr::pull(peakObj$e_meta, nosc_cname)))
  expect_equal(missing_nosc, missing_mol_form, info="number of missing nosc values is not the same as the number of missing mol. formulae")
}

test_elemental_ratios_result <- function(peakObj) {
  oc_cname <- ftmsRanalysis:::getRatioColName(peakObj, "O:C")
  hc_cname <- ftmsRanalysis:::getRatioColName(peakObj, "H:C")
  nc_cname <- ftmsRanalysis:::getRatioColName(peakObj, "N:C")
  pc_cname <- ftmsRanalysis:::getRatioColName(peakObj, "P:C")
  np_cname <- ftmsRanalysis:::getRatioColName(peakObj, "N:P")
  expect_true(all(c(oc_cname, hc_cname, nc_cname, pc_cname, np_cname) %in% colnames(peakObj$e_meta)))
  expect_true(all(!is.null(c(oc_cname, hc_cname, nc_cname, pc_cname, np_cname))))
  
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, oc_cname)))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, hc_cname)))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, nc_cname)))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, pc_cname)))
  expect_true(is.numeric(dplyr::pull(peakObj$e_meta, np_cname)))
  
  missing_mol_form <- sum(is.na(dplyr::pull(peakObj$e_meta, ftmsRanalysis:::getMFColName(peakObj))))
  missing_oc <- sum(is.na(dplyr::pull(peakObj$e_meta, oc_cname)))
  missing_hc <- sum(is.na(dplyr::pull(peakObj$e_meta, hc_cname)))
  missing_nc <- sum(is.na(dplyr::pull(peakObj$e_meta, nc_cname)))
  missing_pc <- sum(is.na(dplyr::pull(peakObj$e_meta, pc_cname)))
  missing_np <- sum(is.na(dplyr::pull(peakObj$e_meta, np_cname)))
  
  expect_true(missing_oc >= missing_mol_form, info="number of missing O:C values is not the same as the number of missing mol. formulae")
  expect_true(missing_hc >= missing_mol_form, info="number of missing H:C values is not the same as the number of missing mol. formulae")
  expect_true(missing_nc >= missing_mol_form, info="number of missing N:C values is not the same as the number of missing mol. formulae")
  expect_true(missing_pc >= missing_mol_form, info="number of missing P:C values is not the same as the number of missing mol. formulae")
  expect_true(missing_np >= missing_mol_form, info="number of missing N:P values is not the same as the number of missing mol. formulae")
}

test_that("tests of compound_calcs function", {
  
  opts <- c("calc_aroma", "calc_dbe", "calc_gibbs", "calc_kendrick", "calc_nosc", "calc_element_ratios")
  
  peak2 <- compound_calcs(examplePeakData, calc_fns=opts)
  test_aroma_result(peak2)
  test_dbe_result(peak2)
  test_gibbs_result(peak2)
  test_kendrick_result(peak2)
  test_nosc_result(peak2)
  test_elemental_ratios_result(peak2)
  
  
  peak3 <- compound_calcs(examplePeakData, calc_fns=opts[1:2])
  test_aroma_result(peak3)
  test_dbe_result(peak3)
  expect_error(test_gibbs_result(peak3))
  expect_error(test_kendrick_result(peak3))
  expect_error(test_nosc_result(peak3))
  expect_error(test_elemental_ratios_result(peak3))
  
  peak4 <- compound_calcs(examplePeakData, calc_fns=opts[4])
  test_kendrick_result(peak4)
  expect_error(test_aroma_result(peak4))
  expect_error(test_dbe_result(peak4))
  expect_error(test_gibbs_result(peak4))
  expect_error(test_nosc_result(peak4))
  expect_error(test_elemental_ratios_result(peak4))
  
  # test multiple kmass cnames and dbe cnames
  kendrick_args = list('base_compounds' = c('CH2', 'CO2', 'H2'))
  dbe_args = list('valences' = data.frame('C' = c(2, 3,3), 'H' = c(1, 1, 1), 'N' = c(3, 2, 3), 'O' = c(2,2,2), 'S' = c(2,3,4), 'P' = c(4,3,2)))
  calc_args = list('calc_kendrick' = kendrick_args, 'calc_dbe' = dbe_args)
  peak5 <- compound_calcs(examplePeakData, calc_fns=opts, calc_args = calc_args)
  test_aroma_result(peak5)
  test_dbe_result(peak5)
  test_gibbs_result(peak5)
  test_kendrick_result(peak5)
  test_nosc_result(peak5)
  test_elemental_ratios_result(peak5)
})

test_that("tests of compound_calcs that should throw errors", {
  expect_error(compound_calcs(examplePeakData, calc_fns="not a real thing"))
  expect_error(compound_calcs(examplePeakData, calc_fns=NULL))
  expect_error(compound_calcs(examplePeakData, calc_fns=c("calc_aroma", "not a real thing")))  
  expect_error(compound_calcs(examplePeakData, calc_fns=opts[-which(opts == 'calc_kendrick')], calc_args = list('calc_kendrick' = list('base_compounds' = c('CH2', 'CO2', 'H2')))))
})
