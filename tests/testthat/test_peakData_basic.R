## Basic functionality tests for peakData objects

library(ftmsRanalysis)
context("peakData construction")

test_that("peakData objects are constructed correctly", {
  data("ftms12T_edata")
  data("ftms12T_fdata")
  data("ftms12T_emeta")
  
  #trace(assign_mf, browser)
  peakObj <- as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                         c_cname="C", h_cname="H", o_cname="O",
                         n_cname="N", s_cname="S", p_cname="P",
                         isotopic_cname = "C13", isotopic_notation = "1")
  
  expect_true(!is.null(peakObj$e_data))
  expect_true(!is.null(peakObj$e_meta))
  expect_true(!is.null(peakObj$f_data))
  expect_true(inherits(peakObj, "peakData"))
  expect_true(inherits(peakObj, "ftmsData"))
  
  expect_equal(getEDataColName(peakObj), "Mass")
  expect_equal(getFDataColName(peakObj), "SampleID")
  expect_equal(getMassColName(peakObj), "Mass")
  expect_equal(getCarbonColName(peakObj), "C")
  expect_equal(getHydrogenColName(peakObj), "H")

  expect_equal(getOxygenColName(peakObj), "O")
  expect_equal(getNitrogenColName(peakObj), "N")
  expect_equal(getSulfurColName(peakObj), "S")
  expect_equal(getPhosphorusColName(peakObj), "P")
  expect_equal(getIsotopicColName(peakObj), "C13")

  expect_true(!is.null(getMFColName(peakObj)))
})

test_that("group designation works correctly on peakData", {
  data("examplePeakData")
  
  peakObj2 <- group_designation(examplePeakData, c("Location", "Block"))
  expect_true(!is.null(ftmsRanalysis:::getGroupDF(peakObj2)))
  
  groupDF <- ftmsRanalysis:::getGroupDF(peakObj2)
  expect_true(all(c(getFDataColName(peakObj2), "Group", "Location", "Block") %in% colnames(groupDF)))
  expect_true(all(groupDF[, getFDataColName(peakObj2)] %in% colnames(peakObj2$e_data)))

  
  peakObj3 <- group_designation(examplePeakData, c("Location"))
  expect_true(!is.null(ftmsRanalysis:::getGroupDF(peakObj3)))
  
  groupDF3 <- ftmsRanalysis:::getGroupDF(peakObj3)
  expect_true(all(c(getFDataColName(peakObj3), "Group") %in% colnames(groupDF3)))
  expect_true(all(groupDF[, getFDataColName(peakObj3)] %in% colnames(peakObj3$e_data)))
  
  expect_true(length(unique(groupDF[, "Group"])) > 
                length(unique(groupDF3[, "Group"])))
  
})
