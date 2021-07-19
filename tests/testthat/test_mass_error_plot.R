library(ftmsRanalysis)

test_that("error thrown when cmsObj not of class 'CoreMSData'", {
  data_frame <- readr::read_csv("example_data1.csv")
  expect_error(mass_error_plot(data_frame),
               "cmsObj must be of the class 'CoreMSData'")
})

# create CoreMSData object with single input file
file_list <- "example_data1.csv"
single_file_data <- read_CoreMS_data(file_list)
single_file_data <- as.CoreMSData(single_file_data)

test_that("invalid input for title argument throws error", {
  expect_error(mass_error_plot(single_file_data, title = 43),
               "title must be single character string")
  expect_error(mass_error_plot(single_file_data, title = c("truck", "green")),
               "title must be single character string")
  expect_error(mass_error_plot(single_file_data, title = -3.4),
               "title must be single character string")
})

test_that("invalid input for xlabel argument throws error", {
  expect_error(mass_error_plot(single_file_data, xlabel = 43),
               "xlabel must be single character string")
  expect_error(mass_error_plot(single_file_data, xlabel = c("truck", "green")),
               "xlabel must be single character string")
  expect_error(mass_error_plot(single_file_data, xlabel = -3.4),
               "xlabel must be single character string")
})

test_that("invalid input for ylabel argument throws error", {
  expect_error(mass_error_plot(single_file_data, ylabel = 43),
               "ylabel must be single character string")
  expect_error(mass_error_plot(single_file_data, ylabel = c("truck", "green")),
               "ylabel must be single character string")
  expect_error(mass_error_plot(single_file_data, ylabel = -3.4),
               "ylabel must be single character string")
})

test_that("invalid input for xrange argument throws error", {
  expect_error(mass_error_plot(all_data, xrange = 40),
               "xrange must be numeric vector with length 2")
  expect_error(mass_error_plot(all_data, xrange = c("one", "two")),
               "xrange must be numeric vector with length 2")
})

test_that("invalid input for yrange argument throws error", {
  expect_error(mass_error_plot(all_data, yrange = 40),
               "yrange must be numeric vector with length 2")
  expect_error(mass_error_plot(all_data, yrange = c("one", "two")),
               "yrange must be numeric vector with length 2")
})

# create CoreMSData object with multiple input files
file_list <- c("example_data1.csv", "example_data_noX1.csv")
mult_file_data <- read_CoreMS_data(file_list, force_rbind = TRUE)
mult_file_data <- as.CoreMSData(mult_file_data)

test_that("multiple-file cmsObj gives hex plot", {

  mass_error_plot(mult_file_data)

})
