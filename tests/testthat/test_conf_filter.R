library(ftmsRanalysis)

test_that("confidence filter works correctly on CoreMSData", {
  files <- list.files(pattern = "example")
  cmsDat <- read_CoreMS_data(files)
  cmsDat <- as.CoreMSData(cmsDat, 
                          c13_cname = "13C",
                          o18_cname = "18O")
  
  filtData <- conf_filter(cmsDat)
  expect_true(inherits(filtData, "confFilt"))
  expect_true(inherits(filtData, "list"))
  expect_true(ncol(filtData$iso_data) == 5)
  expect_true(ncol(filtData$monoiso_data) == 5)
  
  cmsObj_filt <- applyFilt(filtData, cmsDat, min_conf = 0.5)
  expect_true(inherits(cmsObj_filt, "CoreMSData"))
  expect_true(inherits(cmsObj_filt, "list"))
  new_confs <- rbind(cmsObj_filt$monoiso_data[,attr(cmsObj_filt, "cnames")$conf_cname], 
                 cmsObj_filt$iso_data[,attr(cmsObj_filt, "cnames")$conf_cname])
  expect_true(all(new_confs >= 0.5))
  
  expect_true(!is.null(attr(cmsObj_filt, "filters")))
  expect_true(!is.null(attr(cmsObj_filt, "filters")$confFilt))
  
  
})