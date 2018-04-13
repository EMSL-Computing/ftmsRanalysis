## Basic functionality tests for peakIcrData objects

library(fticRanalysis)
context("peakIcrData construction")

test_that("peakIcrData objects are constructed correctly", {
  data("fticr12T_edata")
  data("fticr12T_fdata")
  data("fticr12T_emeta")
  
  #trace(assign_mf, browser)
  picr <- as.peakIcrData(edata, fdata, emeta, edata_cname="peak", fdata_cname="Sample.ID", mass_cname="peak",
                         c_cname="c.number", h_cname="h.number", o_cname="o.number",
                         n_cname="n.number", s_cname="s.number", p_cname="p.number",
                         isotopic_cname = "isotopic", isotopic_notation = "TRUE")
  
  expect_true(!is.null(picr$e_data))
  expect_true(!is.null(picr$e_meta))
  expect_true(!is.null(picr$f_data))
  expect_true(inherits(picr, "peakIcrData"))
  expect_true(inherits(picr, "icrData"))
  
  expect_equal(getEDataColName(picr), "peak")
  expect_equal(getFDataColName(picr), "Sample.ID")
  expect_equal(getMassColName(picr), "peak")
  expect_equal(getCarbonColName(picr), "c.number")
  expect_equal(getHydrogenColName(picr), "h.number")

  expect_equal(getOxygenColName(picr), "o.number")
  expect_equal(getNitrogenColName(picr), "n.number")
  expect_equal(getSulfurColName(picr), "s.number")
  expect_equal(getPhosphorusColName(picr), "p.number")
  expect_equal(getIsotopicColName(picr), "isotopic")

  expect_true(!is.null(getMFColName(picr)))
})

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

test_that("group designation works correctly on peakIcrData", {
  data("peakIcrData")
  
  peakIcrData2 <- group_designation(peakIcrData, c("Location", "Block"))
  expect_true(!is.null(fticRanalysis:::getGroupDF(peakIcrData2)))
  
  groupDF <- fticRanalysis:::getGroupDF(peakIcrData2)
  expect_true(all(c(getFDataColName(peakIcrData2), "Group", "Location", "Block") %in% colnames(groupDF)))
  expect_true(all(groupDF[, getFDataColName(peakIcrData2)] %in% colnames(peakIcrData2$e_data)))

  
  peakIcrData3 <- group_designation(peakIcrData, c("Location"))
  expect_true(!is.null(fticRanalysis:::getGroupDF(peakIcrData3)))
  
  groupDF3 <- fticRanalysis:::getGroupDF(peakIcrData3)
  expect_true(all(c(getFDataColName(peakIcrData3), "Group") %in% colnames(groupDF3)))
  expect_true(all(groupDF[, getFDataColName(peakIcrData3)] %in% colnames(peakIcrData3$e_data)))
  
  expect_true(length(unique(groupDF[, "Group"])) > 
                length(unique(groupDF3[, "Group"])))
  
})
