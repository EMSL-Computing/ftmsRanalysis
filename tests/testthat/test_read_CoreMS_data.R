library(ftmsRanalysis)

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
  #expect_error(read_CoreMS_data(mult_file_mixX1), "one or more columns not present in all files")
  
  mult_data <- read_CoreMS_data(mult_file_mixX1)
  expect_s3_class(mult_data, "CoreMSrbind")
  expect_false("X1" %in% names(mult_data))
  
  sing_data_X1 <- read_CoreMS_data(single_file_X1)
  expect_s3_class(sing_data_X1, "CoreMSrbind")
  expect_false("X1" %in% names(sing_data_X1))
  
  sing_data_noX1 <- read_CoreMS_data(single_file_noX1)
  expect_s3_class(sing_data_noX1, "CoreMSrbind")
  expect_false("X1" %in% names(sing_data_noX1))
  
})

