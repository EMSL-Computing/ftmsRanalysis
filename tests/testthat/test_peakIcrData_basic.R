## Basic functionality tests for peakIcrData objects

library(fticRanalysis)
context("peakIcrData construction")

test_that("peakIcrData objects are constructed correctly", {
  data("fticr12T_edata")
  data("fticr12T_fdata")
  data("fticr12T_emeta")
  
  #trace(assign_mf, browser)
  picr <- as.peakIcrData(fticr12T_edata, fticr12T_fdata, fticr12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                         c_cname="C", h_cname="H", o_cname="O",
                         n_cname="N", s_cname="S", p_cname="P",
                         isotopic_cname = "C13", isotopic_notation = "1")
  
  expect_true(!is.null(picr$e_data))
  expect_true(!is.null(picr$e_meta))
  expect_true(!is.null(picr$f_data))
  expect_true(inherits(picr, "peakIcrData"))
  expect_true(inherits(picr, "icrData"))
  
  expect_equal(getEDataColName(picr), "Mass")
  expect_equal(getFDataColName(picr), "SampleID")
  expect_equal(getMassColName(picr), "Mass")
  expect_equal(getCarbonColName(picr), "C")
  expect_equal(getHydrogenColName(picr), "H")

  expect_equal(getOxygenColName(picr), "O")
  expect_equal(getNitrogenColName(picr), "N")
  expect_equal(getSulfurColName(picr), "S")
  expect_equal(getPhosphorusColName(picr), "P")
  expect_equal(getIsotopicColName(picr), "C13")

  expect_true(!is.null(getMFColName(picr)))
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
