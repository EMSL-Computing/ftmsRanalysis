library(ftmsRanalysis)

test_that("errors thrown when given incorrect input",{
  data("exampleCoreMSData")
  expect_error(unique_mf_assingment(exampleCoreMSData$monoiso_data, method = "confidence"), "cmsObj must be of the class 'CoreMSData'")
  expect_error(unique_mf_assingment(exampleCoreMSData, method = "max_conf"), "method must be 'confidence', 'peak_intensity', or 'prevalence'")
})

test_that("errors thrown when tied filter parameters", {
  data("exampleCoreMSData")
  expect_error(unique_mf_assingment(exampleCoreMSData, method = "confidence"), 
               "Data set contains tied confidence scores. Apply a confidence filter at a threshold of at least 0.16.")
  expect_error(unique_mf_assingment(exampleCoreMSData, method = "peak_intensity"))
  expect_error(unique_mf_assingment(exampleCoreMSData, method = "prevalence"),
               "Data set contains tied most prevalent formulas. Apply a confidence filter at a threshold of at least 0.52.")
})


test_that("output CoreMSData object correctly formatted", {
  data("exampleCoreMSData")
  cmsDat_filt <- applyFilt(filter_object = conf_filter(exampleCoreMSData),
                           msObj = exampleCoreMSData,
                           min_conf = 0.52)
  unq_conf <- unique_mf_assingment(cmsDat_filt, method = "confidence")
  unq_height <- unique_mf_assingment(cmsDat_filt, method = "peak_intensity")
  unq_prev <- unique_mf_assingment(cmsDat_filt, method = "prevalence")
  
  # output should be CoreMSData object, list of 2 dataframes
  expect_s3_class(unq_conf, "CoreMSData")
  expect_s3_class(unq_conf, "list")
  expect_s3_class(unq_height, "CoreMSData")
  expect_s3_class(unq_height, "list")
  expect_s3_class(unq_prev, "CoreMSData")
  expect_s3_class(unq_prev, "list")
  
  # all three methods should give same number of peaks
  expect_identical(nrow(unq_conf$monoiso_data), nrow(unq_height$monoiso_data), nrow(unq_prev$monoiso_data))
  expect_identical(nrow(unq_conf$iso_data), nrow(unq_height$iso_data), nrow(unq_prev$iso_data))
})