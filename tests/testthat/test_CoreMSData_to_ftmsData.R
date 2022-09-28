library(ftmsRanalysis)

data("exampleCoreMSData")

test_that("errors thrown when given incorrect input", {
  expect_error(CoreMSData_to_ftmsData(exampleCoreMSData$monoiso_data), "cmsObj must be of the class 'CoreMSData'")
  expect_error(CoreMSData_to_ftmsData(exampleCoreMSData), "cmsObj contains either duplicate m/z values or duplicate molecular formulas within a sample. The function `unique_mf_assignment` must be used before converting `CoreMSData` object to `ftmsData` object.")
})

ftmsObj <- exampleCoreMSData %>% 
  applyFilt(filter_object = conf_filter(exampleCoreMSData),
            msObj = .,
            min_conf = 0.5) %>% 
  unique_mf_assignment(cmsObj = .,
                       method = "confidence") %>% 
  CoreMSData_to_ftmsData()

test_that("e_data and e_meta have correct structure", {
  filenames <- exampleCoreMSData$monoiso_data[[attr(exampleCoreMSData, "cnames")$file_cname]] 
  expect_true(all(names(ftmsObj$e_data) %in% c("Mass", filenames)))
  
  emeta_cols <- c("Mass", "C", "H", "O", "N", "S", "P", "Calibrated m/z", "Calculated m/z", "Heteroatom Class", "Ion Type", "MolForm")
  expect_true(all(names(ftmsObj$e_meta) %in% emeta_cols))
  
  expect_equal(nrow(ftmsObj$e_data), nrow(ftmsObj$e_meta))
})