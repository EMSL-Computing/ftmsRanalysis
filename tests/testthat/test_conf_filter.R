library(ftmsRanalysis)

test_that("confidence filter works correctly on CoreMSData", {
  data("exampleCoreMSData")
  
  filtData <- conf_filter(exampleCoreMSData)
  expect_true(inherits(filtData, "confFilt"))
  expect_true(inherits(filtData, "data.frame"))
  expect_true(ncol(filtData) == 5)
  
  cmsObj_filt <- applyFilt.confFilt(filtData, exampleCoreMSData, min_conf = 0.5)
  expect_true(inherits(cmsObj_filt, "CoreMSData"))
  expect_true(inherits(cmsObj_filt, "data.frame"))
  new_confs <- cmsObj_filt[,attr(cmsObj_filt, "cnames")$conf_cname]
  expect_true(all(new_confs >= 0.5))
  
  expect_true(!is.null(attr(cmsObj_filt, "filters")))
  expect_true(!is.null(attr(cmsObj_filt, "filters")$confFilt))
  
  
})