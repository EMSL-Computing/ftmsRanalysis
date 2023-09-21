library(ftmsRanalysis)

ex_files <- c("example_data1.csv",
              "example_data2.csv",
              "example_data3.csv",
              "example_data_noX1.csv")
single_file_X1 <- "example_data1.csv"
single_file_noX1 <- "example_data_noX1.csv"

test_that("error thrown when input is not character vector", {
  things <- c(3, 7, 9, 1)
  expect_error(read_CoreMS_data(things), "list_of_files must be of the class 'chr'")
  
  expect_error(read_CoreMS_data(ex_files, sample_names = things), "sample_names must be of the class 'chr'")
})

test_that("error thrown when length of sample_names not equal to length of list_of_files", {
  expect_error(read_CoreMS_data(ex_files, sample_names = c("Sample1", "Sample2")),
               "length of sample_names must be equal to length of list_of_files")
})

test_that("read_CoreMS_data output has correct format", {
  
  mult_data <- read_CoreMS_data(ex_files)
  expect_s3_class(mult_data, "CoreMSrbind")
  expect_false("X1" %in% names(mult_data))
  expect_false("..1" %in% names(mult_data))
  
  sing_data_X1 <- read_CoreMS_data(single_file_X1)
  expect_s3_class(sing_data_X1, "CoreMSrbind")
  expect_false("X1" %in% names(sing_data_X1))
  
  sing_data_noX1 <- read_CoreMS_data(single_file_noX1)
  expect_s3_class(sing_data_noX1, "CoreMSrbind")
  expect_false("X1" %in% names(sing_data_noX1))
  
})

test_that("sample_names correctly applied", {
  samps <- c("Sample1", "Sample2", "Sample3", "Sample4")
  mult_data <- read_CoreMS_data(ex_files, sample_names = samps)
  
  expect_true(all(unique(mult_data$Filename) %in% samps))
})

