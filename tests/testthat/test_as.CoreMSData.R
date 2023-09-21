library(ftmsRanalysis)

test_that("CoreMSData object correctly constructed", {
  
  # error thrown when object passed to as.CoreMSData() is not of class 'CoreMSrbind'
  data_frame <- readr::read_csv("example_data1.csv")
  expect_error(as.CoreMSData(data_frame), "all_data must be of the class 'CoreMSrbind'")
  
  mult_file_mixX1 <- c("example_data1.csv",
                       "example_data2.csv",
                       "example_data3.csv",
                       "example_data_noX1.csv")
  all_data <- read_CoreMS_data(mult_file_mixX1)
  
  # error thrown when column names not found in all_data
  expect_error(as.CoreMSData(all_data, obs_mass_cname = "Mass"),
               "Observed mass column Mass not found in all_data")
  expect_error(as.CoreMSData(all_data, calc_mass_cname = "Mass"),
               "Calculated mass column Mass not found in all_data")
  expect_error(as.CoreMSData(all_data, pheight_cname = "Height"),
               "Peak height/intensity column Height not found in all_data")
  expect_error(as.CoreMSData(all_data, error_cname = "error"),
               "Mass error column error not found in all_data")
  expect_error(as.CoreMSData(all_data, conf_cname = "score"),
               "Confidence score column score not found in all_data")
  expect_error(as.CoreMSData(all_data, file_cname = "name"),
               "Filename column name not found in all_data")
  expect_error(as.CoreMSData(all_data, mf_cname = "formula"),
               "Molecular Formula column formula not found in all_data")
  expect_error(as.CoreMSData(all_data, iso_cols = c(1,2,3)), 
               "iso_cols must be a character vector")
  
  cmsObj <- as.CoreMSData(all_data = all_data,
                          obs_mass_cname = "m/z",
                          calc_mass_cname = "Calculated m/z",
                          pheight_cname = "Peak Height",
                          error_cname = "Mass Error (ppm)",
                          conf_cname = "Confidence Score",
                          file_cname = "Filename",
                          mf_cname = "Molecular Formula",
                          iso_cols = "13C")
  
  
  expect_equal(attr(cmsObj, "cnames")$obs_mass_cname, "m/z")
  expect_equal(attr(cmsObj, "cnames")$calc_mass_cname, "Calculated m/z")
  expect_equal(attr(cmsObj, "cnames")$pheight_cname, "Peak Height")
  expect_equal(attr(cmsObj, "cnames")$error_cname, "Mass Error (ppm)")
  expect_equal(attr(cmsObj, "cnames")$conf_cname, "Confidence Score")
  expect_equal(attr(cmsObj, "cnames")$file_cname, "Filename")
  expect_equal(attr(cmsObj, "cnames")$mf_cname, "Molecular Formula")
  expect_equal(attr(cmsObj, "cnames")$iso_cols, "13C")
  
  # correct structure of CoreMSData object
  expect_s3_class(cmsObj, "CoreMSData")
  expect_s3_class(cmsObj, "list")
  expect_true(length(cmsObj) == 2)
  expect_s3_class(cmsObj$monoiso_data, "data.frame")
  expect_s3_class(cmsObj$iso_data, "data.frame")
  
  # monoisotopic and isotopic peaks correctly sorted
  expect_false(all(stringr::str_detect(cmsObj$monoiso_data$`Molecular Formula`, pattern = "13C|18O|15N|34S")))
  expect_true(all(stringr::str_detect(cmsObj$iso_data$`Molecular Formula`, pattern = "13C|18O|15N|34S")))
})