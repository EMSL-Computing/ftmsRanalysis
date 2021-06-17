library(CoreMS2FREDA)

test_that("error thrown when input is not character vector", {
  things <- c(3, 7, 9)
  expect_error(read_CoreMS_data(things), "list_of_files must be of the class 'chr'")
})

test_that("read_CoreMS_data output has correct format", {
  
  mult_file_mixX1 <- c("example_data1.csv",
                       "example_data2.csv",
                       "example_data3.csv",
                       "example_data_noX1.csv")
  single_file_X1 <- "example_data1.csv"
  single_file_noX1 <- "example_data_noX1.csv"
  
  # check that error thrown when force_rbind = FALSE
  expect_error(read_CoreMS_data(mult_file_mixX1), "one or more columns not present in all files")
  
  mult_data <- read_CoreMS_data(mult_file_mixX1, force_rbind = TRUE)
  expect_s3_class(mult_data, "CoreMSrbind")
  expect_false("X1" %in% names(mult_data))
  
  sing_data_X1 <- read_CoreMS_data(single_file_X1)
  expect_s3_class(sing_data_X1, "CoreMSrbind")
  expect_false("X1" %in% names(sing_data_X1))
  
  sing_data_noX1 <- read_CoreMS_data(single_file_noX1)
  expect_s3_class(sing_data_noX1, "CoreMSrbind")
  expect_false("X1" %in% names(sing_data_noX1))
  
})


test_that("CoreMSData object correctly constructed with required columns", {
  
  # error thrown when object passed to as.CoreMSData() is not of class 'CoreMSrbind'
  data_frame <- readr::read_csv("example_data1.csv")
  expect_error(as.CoreMSData(data_frame), "all_data must be of the class 'CoreMSrbind'")
  
  mult_file_mixX1 <- c("example_data1.csv",
                       "example_data2.csv",
                       "example_data3.csv",
                       "example_data_noX1.csv")
  all_data <- read_CoreMS_data(mult_file_mixX1, force_rbind = TRUE)
  
  # error thrown when column names not found in all_data
  expect_error(as.CoreMSData(all_data, mass_cname = "Mass"),
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
  expect_error(as.CoreMSData(all_data, c13_cname = "carbon"), 
               "Carbon column carbon not found in all_data")
  expect_error(as.CoreMSData(all_data, o18_cname = "oxygen"), 
               "Oxygen column oxygen not found in all_data")
  expect_error(as.CoreMSData(all_data, n15_cname = "carbon"), 
               "Nitrogen column carbon not found in all_data")
  expect_error(as.CoreMSData(all_data, s34_cname = "carbon"), 
               "Sulfur column carbon not found in all_data")
  

  cmsObj <- as.CoreMSData(all_data = all_data,
                          mass_cname = "Calculated m/z",
                          pheight_cname = "Peak Height",
                          error_cname = "Mass Error (ppm)",
                          conf_cname = "Confidence Score",
                          file_cname = "Filename",
                          mf_cname = "Molecular Formula",
                          c13_cname = "13C")
  
  expect_s3_class(cmsObj, "CoreMSData")
  
  expect_equal(attr(cmsObj, "cnames")$mass_cname, "Calculated m/z")
  expect_equal(attr(cmsObj, "cnames")$pheight_cname, "Peak Height")
  expect_equal(attr(cmsObj, "cnames")$error_cname, "Mass Error (ppm)")
  expect_equal(attr(cmsObj, "cnames")$conf_cname, "Confidence Score")
  expect_equal(attr(cmsObj, "cnames")$file_cname, "Filename")
  expect_equal(attr(cmsObj, "cnames")$mf_cname, "Molecular Formula")
  expect_equal(attr(cmsObj, "cnames")$c13_cname, "13C")
  
  expect_true(is.null(attr(cmsObj, "cnames")$s34_cname))
  expect_true(is.null(attr(cmsObj, "cnames")$o18_cname))
  expect_true(is.null(attr(cmsObj, "cnames")$n15_cname))
  
})

