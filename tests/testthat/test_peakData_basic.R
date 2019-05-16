## Basic functionality tests for peakData objects

library(ftmsRanalysis)
context("peakData construction")

test_that("peakData objects are constructed correctly with elemental columns", {
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

test_that("peakData objects are constructed correctly with molecular formulae", {
  data("examplePeakData")
  
  emeta <- dplyr::select(examplePeakData$e_meta, -c(O, N, S, P))

  #trace(assign_mf, browser)
  peakObj <- as.peakData(examplePeakData$e_data, examplePeakData$f_data, emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                         mf_cname="MolForm", isotopic_cname = "C13", isotopic_notation = "1")
  
  expect_true(!is.null(peakObj$e_data))
  expect_true(!is.null(peakObj$e_meta))
  expect_true(!is.null(peakObj$f_data))
  expect_true(inherits(peakObj, "peakData"))
  expect_true(inherits(peakObj, "ftmsData"))
  
  expect_equal(getEDataColName(peakObj), "Mass")
  expect_equal(getFDataColName(peakObj), "SampleID")
  expect_equal(getMassColName(peakObj), "Mass")
  
  expect_true(!is.null(getCarbonColName(peakObj)))
  expect_true(!is.null(getHydrogenColName(peakObj)))
  expect_true(!is.null(getOxygenColName(peakObj)))
  expect_true(!is.null(getNitrogenColName(peakObj)))
  expect_true(!is.null(getSulfurColName(peakObj)))
  expect_true(!is.null(getPhosphorusColName(peakObj)))
  
  expect_true(getCarbonColName(peakObj) %in% colnames(peakObj$e_meta))
  expect_true(getHydrogenColName(peakObj) %in% colnames(peakObj$e_meta))
  expect_true(getOxygenColName(peakObj) %in% colnames(peakObj$e_meta))
  expect_true(getNitrogenColName(peakObj) %in% colnames(peakObj$e_meta))
  expect_true(getSulfurColName(peakObj) %in% colnames(peakObj$e_meta))
  expect_true(getPhosphorusColName(peakObj) %in% colnames(peakObj$e_meta))
  
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

test_that("peak data construction works without optional elemental columns", {
  data("ftms12T_edata")
  data("ftms12T_fdata")
  data("ftms12T_emeta")
  
  emeta2 <- dplyr::select(ftms12T_emeta, -c(O, N, S, P))
  peakObj <- as.peakData(ftms12T_edata, ftms12T_fdata, emeta2, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                         c_cname="C", h_cname="H")
  
  expect_true(!is.null(getOxygenColName(peakObj)))
  expect_true(!is.null(getNitrogenColName(peakObj)))
  expect_true(!is.null(getSulfurColName(peakObj)))
  expect_true(!is.null(getPhosphorusColName(peakObj)))
  
  expect_true(getOxygenColName(peakObj) %in% colnames(peakObj$e_meta))
  expect_true(getNitrogenColName(peakObj) %in% colnames(peakObj$e_meta))
  expect_true(getSulfurColName(peakObj) %in% colnames(peakObj$e_meta))
  expect_true(getPhosphorusColName(peakObj) %in% colnames(peakObj$e_meta))
  
  expect_true(all(peakObj$e_meta[, getOxygenColName(peakObj)] == 0))
  expect_true(all(peakObj$e_meta[, getNitrogenColName(peakObj)] == 0))
  expect_true(all(peakObj$e_meta[, getSulfurColName(peakObj)] == 0))
  expect_true(all(peakObj$e_meta[, getPhosphorusColName(peakObj)] == 0))

  expect_true(!all(is.na(peakObj$e_meta[, getMFColName(peakObj)])))
})

test_that("peak data construction calls that should cause errors", {
  
# this section repeatedly calls as.peakData with different invalid parameter values
  
  data("ftms12T_edata")
  data("ftms12T_fdata")
  data("ftms12T_emeta")
  
  expect_error(as.peakData("hello world", ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                         c_cname="C", h_cname="H", o_cname="O",
                         n_cname="N", s_cname="S", p_cname="P",
                         isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, "invalid param", ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, "nope", edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass35", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID99", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="MassABC",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="CJFJKD", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="HJKGLJD", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O(#I",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="NU(%#", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="SU(%#", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="PKJGFILS",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13KOD", isotopic_notation = "A"))
  
  # optional parameters
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", extraction_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", o2c_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", h2c_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", kmass_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", nosc_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", gfe_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", aroma_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", modaroma_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", dbe_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", dbeo_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", dbeai_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", elcomp_cname="hello"))
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1", instrument_type=1))
  # expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
  #                          c_cname="C", h_cname="H", o_cname="O",
  #                          n_cname="N", s_cname="S", p_cname="P",
  #                          isotopic_cname = "C13", isotopic_notation = "1", data_scale="hello"))

  
  ## Change the 3 data frames to have bad data
  emeta2 <- ftms12T_emeta[1:20, ]
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, emeta2, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  
  # if extra rows are found in emeta that are not in edata, they're removed from emeta with a warning not an error
  edata2 <- ftms12T_edata[1:20, ]
  expect_warning(as.peakData(edata2, ftms12T_fdata, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  

  fdata2 <- ftms12T_fdata[1:5,]
  expect_error(as.peakData(ftms12T_edata, fdata2, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  
  emeta3 <- dplyr::select(ftms12T_emeta, -Mass)
  expect_error(as.peakData(ftms12T_edata, ftms12T_fdata, emeta3, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  
  fdata3 <- dplyr::select(ftms12T_fdata, -SampleID)
  expect_error(as.peakData(ftms12T_edata, fdata3, ftms12T_emeta, edata_cname="Mass", fdata_cname="SampleID", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  expect_error(as.peakData(ftms12T_edata, fdata3, ftms12T_emeta, edata_cname="Mass", fdata_cname="Location", mass_cname="Mass",
                           c_cname="C", h_cname="H", o_cname="O",
                           n_cname="N", s_cname="S", p_cname="P",
                           isotopic_cname = "C13", isotopic_notation = "1"))
  
  
})
