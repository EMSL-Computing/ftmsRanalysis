library(ftmsRanalysis)

data("exampleCoreMSData")

test_that("error thrown when cmsObj not of class 'CoreMSData'", {
  expect_error(mass_error_plot(exampleCoreMSData$monoiso_data),
               "cmsObj must be of the class 'CoreMSData'")
})

test_that("invalid input for title argument throws error", {
  expect_error(mass_error_plot(exampleCoreMSData, title = 43),
               "title must be single character string")
  expect_error(mass_error_plot(exampleCoreMSData, title = c("truck", "green")),
               "title must be single character string")
  expect_error(mass_error_plot(exampleCoreMSData, title = -3.4),
               "title must be single character string")
})

test_that("invalid input for xlabel argument throws error", {
  expect_error(mass_error_plot(exampleCoreMSData, xlabel = 43),
               "xlabel must be single character string")
  expect_error(mass_error_plot(exampleCoreMSData, xlabel = c("truck", "green")),
               "xlabel must be single character string")
  expect_error(mass_error_plot(exampleCoreMSData, xlabel = -3.4),
               "xlabel must be single character string")
})

test_that("invalid input for ylabel argument throws error", {
  expect_error(mass_error_plot(exampleCoreMSData, ylabel = 43),
               "ylabel must be single character string")
  expect_error(mass_error_plot(exampleCoreMSData, ylabel = c("truck", "green")),
               "ylabel must be single character string")
  expect_error(mass_error_plot(exampleCoreMSData, ylabel = -3.4),
               "ylabel must be single character string")
})

test_that("invalid input for xrange argument throws error", {
  expect_error(mass_error_plot(exampleCoreMSData, xrange = 40),
               "xrange must be numeric vector with length 2")
  expect_error(mass_error_plot(exampleCoreMSData, xrange = c("one", "two")),
               "xrange must be numeric vector with length 2")
})

test_that("invalid input for yrange argument throws error", {
  expect_error(mass_error_plot(exampleCoreMSData, yrange = 40),
               "yrange must be numeric vector with length 2")
  expect_error(mass_error_plot(exampleCoreMSData, yrange = c("one", "two")),
               "yrange must be numeric vector with length 2")
})

