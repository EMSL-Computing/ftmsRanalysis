library(ftmsRanalysis)

data("exampleCoreMSData")

test_that("errors thrown when given incorrect input",{
  expect_error(unique_mf_assingment(exampleCoreMSData$monoiso_data, method = "confidence"), "cmsObj must be of the class 'CoreMSData'")
  expect_error(unique_mf_assingment(exampleCoreMSData, method = "max_conf"), "method must be 'confidence', 'peak_intensity', or 'prevalence'")
})

test_that("errors thrown when tied filter parameters", {
  expect_error(unique_mf_assingment(exampleCoreMSData, method = "confidence"))
  expect_error(unique_mf_assingment(exampleCoreMSData, method = "peak_intensity"))
  expect_error(unique_mf_assingment(exampleCoreMSData, method = "prevalence"))
})

# Confidence filter
cmsDat_filt <- applyFilt(filter_object = conf_filter(exampleCoreMSData),
                         msObj = exampleCoreMSData,
                         min_conf = 0.52)

# Example output using each method
unq_conf <- unique_mf_assingment(cmsDat_filt, method = "confidence")
unq_height <- unique_mf_assingment(cmsDat_filt, method = "peak_intensity")
unq_prev <- unique_mf_assingment(cmsDat_filt, method = "prevalence")

test_that("output CoreMSData object correctly formatted", {

  # output should be CoreMSData object, list of 2 dataframes
  expect_s3_class(unq_conf, "CoreMSData")
  expect_s3_class(unq_conf, "list")
  expect_s3_class(unq_height, "CoreMSData")
  expect_s3_class(unq_height, "list")
  expect_s3_class(unq_prev, "CoreMSData")
  expect_s3_class(unq_prev, "list")
})

test_that("output of all three methods has same number of peaks", {
  expect_identical(nrow(unq_conf$monoiso_data), nrow(unq_height$monoiso_data), nrow(unq_prev$monoiso_data))
  expect_identical(nrow(unq_conf$iso_data), nrow(unq_height$iso_data), nrow(unq_prev$iso_data))
})

test_that("output contains no duplicate m/z values within a sample", {
  dup_conf <- unq_conf$monoiso_data %>% 
    dplyr::group_by(Filename, `m/z`) %>% 
    dplyr::filter(dplyr::n() > 1)
  dup_height <- unq_height$monoiso_data %>% 
    dplyr::group_by(Filename, `m/z`) %>% 
    dplyr::filter(dplyr::n() > 1)
  dup_prev <- unq_prev$monoiso_data %>% 
    dplyr::group_by(Filename, `m/z`) %>% 
    dplyr::filter(dplyr::n() > 1)
  
  expect_equal(nrow(dup_conf), 0)
  expect_equal(nrow(dup_height), 0)
  expect_equal(nrow(dup_prev), 0)
})

test_that("output contains no duplicate molecular formulas within a sample", {
  dup_conf <- unq_conf$monoiso_data %>% 
    dplyr::group_by(Filename, `Molecular Formula`) %>% 
    dplyr::filter(dplyr::n() > 1)
  dup_height <- unq_height$monoiso_data %>% 
    dplyr::group_by(Filename, `Molecular Formula`) %>% 
    dplyr::filter(dplyr::n() > 1)
  dup_prev <- unq_prev$monoiso_data %>% 
    dplyr::group_by(Filename, `Molecular Formula`) %>% 
    dplyr::filter(dplyr::n() > 1)
  
  expect_equal(nrow(dup_conf), 0)
  expect_equal(nrow(dup_height), 0)
  expect_equal(nrow(dup_prev), 0)
})
