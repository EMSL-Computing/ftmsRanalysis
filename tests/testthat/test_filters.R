## Basic functionality tests for peakIcrData objects

library(fticRanalysis)
context("filtering on peakIcrData objects")

test_that("mass filters work correctly on peakIcrData", {
  data("peakIcrData")
  
  filtData <- mass_filter(peakIcrData)
  expect_true(inherits(filtData, "massFilt"))
  expect_true(inherits(filtData, "data.frame"))
  expect_true(ncol(filtData) == 2)
  expect_true(all(c(getEDataColName(peakIcrData), getMassColName(peakIcrData)) %in% colnames(filtData)))
  
  peakIcrData2 <- applyFilt(filtData, peakIcrData, min_mass = 200, max_mass = 900)
  expect_true(inherits(peakIcrData2, "peakIcrData"))
  new.masses <- as.numeric(peakIcrData2$e_meta[, getMassColName(peakIcrData2)])
  expect_true(all(new.masses >= 200))
  expect_true(all(new.masses <= 900))
  
  expect_true(nrow(peakIcrData$e_data) >= nrow(peakIcrData2$e_data))
  
  expect_true(!is.null(attr(peakIcrData2, "filter")))
  expect_true(!is.null(attr(peakIcrData2, "filter")$massFilt))
  ## TODO do we want to test more things about the attribute here?
  
  # summary method
  filtSumm <- summary(filtData)
  expect_true(inherits(filtSumm, "summaryDefault"))
  expect_true(is.numeric(filtSumm))
  
  filtSumm2 <- summary(filtData, min_mass=200, max_mass=900)
  expect_true(inherits(filtSumm2, "summaryDefault"))
  expect_true(is.numeric(filtSumm2))

  filtSumm3 <- summary(filtData, min_mass=200)
  expect_true(inherits(filtSumm3, "summaryDefault"))
  expect_true(is.numeric(filtSumm3))
  
  filtSumm4 <- summary(filtData, max_mass=900)
  expect_true(inherits(filtSumm4, "summaryDefault"))
  expect_true(is.numeric(filtSumm4))
  
  # test some things that should fail  
  expect_error(tmp <- applyFilt(filtData, peakIcrData2, min_mass = 500, max_mass = 600))
  expect_error(tmp <- applyFilt(filtData, peakIcrData, min_mass = "hello", max_mass = 600))
  
})


test_that("molecule filters work correctly on peakIcrData", {
  data("peakIcrData")
  
  filtData <- molecule_filter(peakIcrData)
  expect_true(inherits(filtData, "moleculeFilt"))
  expect_true(inherits(filtData, "data.frame"))
  expect_true(ncol(filtData) == 2)
  expect_true(all(c(getEDataColName(peakIcrData), "Num_Observations") %in% colnames(filtData)))
  
  peakIcrData2 <- applyFilt(filtData, peakIcrData, min_num = 2)
  
  expect_true(inherits(peakIcrData2, "peakIcrData"))
  retainedPeaks <- as.vector(dplyr::filter(filtData, Num_Observations >=2)[, getEDataColName(peakIcrData2)])
  expect_true(all(retainedPeaks %in% peakIcrData2$e_data[, getEDataColName(peakIcrData2)]))
  
  expect_true(nrow(peakIcrData$e_data) >= nrow(peakIcrData2$e_data))
  
  expect_true(!is.null(attr(peakIcrData2, "filter")))
  expect_true(!is.null(attr(peakIcrData2, "filter")$moleculeFilt))
  ## TODO do we want to test more things about the attribute here?
  
  # summary method
  filtSumm <- summary(filtData)
  expect_true(inherits(filtSumm, "summaryDefault"))

  filtSumm2 <- summary(filtData, min_num=2)
  expect_true(inherits(filtSumm2, "summaryDefault"))
  
  # test some things that should fail  
  expect_error(tmp <- applyFilt(filtData, peakIcrData, min_num=-1))
  expect_error(tmp <- applyFilt(filtData, peakIcrData, min_num="hello"))
  
})

test_that("formula filters work correctly on peakIcrData", {
  data("peakIcrData")
  
  filtData <- formula_filter(peakIcrData)
  expect_true(inherits(filtData, "formulaFilt"))
  expect_true(inherits(filtData, "data.frame"))
  expect_true(ncol(filtData) == 2)
  expect_true(all(c(getEDataColName(peakIcrData), "Formula_Assigned") %in% colnames(filtData)))
  
  ## Remove peaks WITHOUT formulas
  peakIcrData2 <- applyFilt(filtData, peakIcrData, remove = 'NoFormula')
  
  expect_true(inherits(peakIcrData2, "peakIcrData"))
  retainedPeaks <- as.vector(dplyr::filter(filtData, Formula_Assigned)[, getEDataColName(peakIcrData2)])
  expect_true(all(retainedPeaks %in% peakIcrData2$e_data[, getEDataColName(peakIcrData2)]))
  
  expect_true(nrow(peakIcrData$e_data) >= nrow(peakIcrData2$e_data))
  
  expect_true(!is.null(attr(peakIcrData2, "filter")))
  expect_true(!is.null(attr(peakIcrData2, "filter")$formulaFilt))
  ## TODO do we want to test more things about the attribute here?
  
  # summary method
  filtSumm <- summary(filtData)
  expect_true(inherits(filtSumm, "summaryDefault"))

  filtSumm2 <- summary(filtData, remove="NoFormula")
  expect_true(inherits(filtSumm2, "summaryDefault"))

  filtSumm3 <- summary(filtData, remove="Formula")
  expect_true(inherits(filtSumm3, "summaryDefault"))

  ## Remove peaks WITH formulas
  peakIcrData3 <- applyFilt(filtData, peakIcrData, remove = 'Formula')
  
  expect_true(inherits(peakIcrData3, "peakIcrData"))
  retainedPeaks <- as.vector(dplyr::filter(filtData, !Formula_Assigned)[, getEDataColName(peakIcrData3)])
  expect_true(all(retainedPeaks %in% peakIcrData3$e_data[, getEDataColName(peakIcrData3)]))
  
  expect_true(nrow(peakIcrData$e_data) >= nrow(peakIcrData3$e_data))
  
  expect_true(!is.null(attr(peakIcrData3, "filter")))
  expect_true(!is.null(attr(peakIcrData3, "filter")$formulaFilt))
  ## TODO do we want to test more things about the attribute here?
  
  # test some things that should fail  
  expect_error(tmp <- applyFilt(filtData, peakIcrData, remove="invalid"))
  
})

test_that("emeta filters work correctly on peakIcrData", {
  data("peakIcrData")
  
  # select 3 random numeric columns and 1 categorical (fixed as MolForm) to test.  
  # I excluded C13 since currently it only has 1 level.
  cols <- sapply(peakIcrData$e_meta[,which(!(colnames(peakIcrData$e_meta) %in% c(getEDataColName(peakIcrData), "C13")))], is.numeric) %>% 
    which() %>% 
    sample(3) %>% 
    names() %>% c("MolForm")
  
  filterlist <- lapply(cols, function(colname){
    emeta_filter(peakIcrData, colname)
  })
  
  expect_true(all(sapply(filterlist, inherits, what = c("emetaFilt", "data.frame"))))
  expect_true(all(sapply(filterlist, ncol) == 2))
  expect_true(all(sapply(1:4, function(i){
                        class(filterlist[[i]]$emeta_value) == class(peakIcrData$e_meta[,cols[i]])
                        })),
              info = "type mismatch between icrdata column and filter column")
  
  ### test numeric ###
  
  # max/min for each of the three numeric filters
  maxlist <- sapply(cols[1:3], function(col){
    max(peakIcrData$e_meta[col], na.rm = TRUE)
  })
  
  minlist <- sapply(cols[1:3], function(col){
    min(peakIcrData$e_meta[col], na.rm = TRUE)
  })
  
  for(i in 1:3){
    filtered_obj <- applyFilt(filterlist[[i]], peakIcrData, minlist[[i]], maxlist[[i]])
    
    # max/min values in the filter should not affect e_meta
    expect_true(all(filtered_obj$e_meta == peakIcrData$e_meta, na.rm = TRUE))
    
    # numeric range that is a subset of the range of the column
    newmax = runif(1, median(peakIcrData$e_meta[,cols[i]], na.rm = TRUE), maxlist[[i]])
    newmin = runif(1, minlist[[i]], median(peakIcrData$e_meta[,cols[i]], na.rm = TRUE))
    
    filtered_obj <- applyFilt(filterlist[[i]], peakIcrData, newmin, newmax)
    
    expect_equal(nrow(filtered_obj$e_meta), 
                 nrow(filterlist[[i]] %>% dplyr::filter(emeta_value <= newmax, emeta_value >= newmin)))
    
    filtSumm <- summary(filterlist[[i]])
    expect_true(inherits(filtSumm, "summaryDefault"))
    
    filtSumm2 <- summary(filterlist[[i]], min_val = newmin, max_val=newmax)
    expect_true(inherits(filtSumm2, "summaryDefault"))
    
    filtSumm3 <- summary(filterlist[[i]], min_val = newmin)
    expect_true(inherits(filtSumm3, "summaryDefault"))
    
    filtSumm4 <- summary(filterlist[[i]], max_val=newmax)
    expect_true(inherits(filtSumm4, "summaryDefault"))
    
    expect_true(!is.null(attr(filtered_obj, "filter")))
    expect_true(!is.null(attr(filtered_obj, "filter")[[paste0("emetaFilt_", cols[i])]]))
    
    # test some things that should fail
    expect_error(tmp <- applyFilt(filterlist[[i]], peakIcrData, min_val = maxlist[[i]], max_val = minlist[[i]]))
    expect_error(tmp <- applyFilt(filterlist[[i]], peakIcrData, min_val = minlist[[i]] - 100, max_val = minlist[[i]] - 0.1^10))
  }
  
  ### test categorical ###
  
  cats = filterlist[[4]]$emeta_value %>% unique() %>% setdiff(NA)
  
  filtered_obj <- applyFilt(filterlist[[4]], peakIcrData, cats = cats, na.rm = FALSE)
  
  # returns same object if all NON-NA levels specified and na.rm = FALSE
  expect_true(all(filtered_obj$e_meta == peakIcrData$e_meta, na.rm = TRUE))
  
  # subset of categories
 
  cats <- sample(cats, ceiling(length(cats)/2))  
  filtered_obj <- applyFilt(filterlist[[4]], peakIcrData, cats = cats, na.rm = FALSE)
  
  expect_equal(nrow(filtered_obj$e_meta), 
               nrow(filterlist[[4]] %>% dplyr::filter(emeta_value %in% c(cats, NA))))
  
  filtSumm <- summary(filterlist[[4]])
  expect_true(inherits(filtSumm, "summaryDefault"))

  filtSumm2 <- summary(filterlist[[4]], cats=cats)
  expect_true(inherits(filtSumm2, "summaryDefault"))
  
  expect_true(!is.null(attr(filtered_obj, "filter")))
  expect_true(!is.null(attr(filtered_obj, "filter")$emetaFilt_MolForm))

  # test some things that should fail
  expect_error(tmp <- applyFilt(filterlist[[4]], peakIcrData, cats = "_31234___RIDICULOUS-^^^--***#$#% CHAR ARG"))
  
})
