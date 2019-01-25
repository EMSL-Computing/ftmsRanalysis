#' Combine rows of icrData with the same mass formula
#' 
#' If multiple peaks are mapped to the same mass formula it can cause problems 
#' or inconsistencies when mapping data to a biological database. The
#' \code{combinePeaksWithSameFormula} combines rows of \code{icrData$e_data}
#' that map to the same mass formula:
#' * If data is abundance: columns are summed
#' * If data is log scale: data are converted to abundance, then columns are summed and re-transformed to the original scale
#' * If data is presence/absence: 'or' function is applied to each column
#' 
#' It is recommended to call the \code{combinePeaksWithSameFormula} function
#' before using \code{mapPeaksToCompounds}.
#'
#' @param icrData data of type \code{peakIcrData}
#'
#' @return \code{peakIcrData} object where now two rows of \code{e_data} map to the same mass formula
#' @export
#'
#' @examples
#' new_data <- combinePeaksWithSameFormula(peakIcrProcessed)
combinePeaksWithSameFormula <- function(icrData) {
  if (!inherits(icrData, "peakIcrData") ) {
    stop("icrData must be an object of type peakIcrData")
  }
  if (is.null(getMFColName(icrData))) {
    stop("mass formula column name is not set")
  }
  if (is.null(getMassColName(icrData))) {
    stop("mass column name is not set")
  }
  validScales <- c('log2', 'log10', 'log', 'pres', 'abundance')
  dataScale <- getDataScale(icrData)
  if (!(dataScale %in% validScales)) {
    stop(sprintf("the data scale of icrData must one of: (%s)", paste(validScales, collapse=", ")))
  }
  
  # determine which rows of e_data need to be combined
  if (inherits(icrData, "peakIcrData")) {
    mf_col <- getMFColName(icrData)
    massMFComb <- dplyr::select_(icrData$e_meta, getEDataColName(icrData), getMassColName(icrData), getMFColName(icrData)) %>%
      dplyr::rename_(Mass=getMassColName(icrData), MF=getMFColName(icrData)) %>%
      dplyr::filter(!is.na(MF))
    if (anyDuplicated(massMFComb$MF) > 0) { # identify dups
      tmp <- table(massMFComb$MF)
      dupMF <- names(tmp)[tmp>1]
      massMFComb <- massMFComb %>%
        dplyr::filter(MF %in% dupMF)
      message(sprintf("%d mass formulas with duplicate values found", length(dupMF)))
    } else {
      message("No duplicates found")
      return(icrData)
    }
    if (getMassColName(icrData) != getEDataColName(icrData)) {
      massMFComb <- dplyr::select(massMFComb, -Mass)
    }
    
  }
  
  # transform data scale back to abundance for summing rows
  if (dataScale != "pres" & dataScale != "abundance") {
    icrData <- edata_transform(icrData, "abundance")
  }
  
  sample_cols <- as.character(dplyr::pull(icrData$f_data, getFDataColName(icrData)))
  
  edata <- suppressWarnings(
    icrData$e_data %>%
    dplyr::mutate(index=1:nrow(icrData$e_data)) %>%
    dplyr::right_join(massMFComb, by=getEDataColName(icrData)) %>%
    dplyr::group_by(MF) 
  )
  
  # get first ID and index for each MF
  massMFFinal <- edata %>%
    dplyr::rename_(ID=getEDataColName(icrData)) %>%
    dplyr::summarize(ID=first(ID), index=min(index))
  colnames(massMFFinal)[2] <- getEDataColName(icrData)
  
  # get IDs to remove
  idsToRemove <- setdiff(dplyr::pull(massMFComb, getEDataColName(icrData)), 
                         dplyr::pull(massMFFinal, 2))
  
  # sum rows for each set of MF  
  edata <- suppressWarnings(
    edata %>%
    dplyr::summarize_at(sample_cols, sum) %>%
    dplyr::full_join(massMFFinal, by="MF")
  )
  row_ids <- edata$index
  edata <- dplyr::select(edata, -c(index, MF))

  # order columns same as original
  edata <- as.data.frame(edata[, c(getEDataColName(icrData), sample_cols)])
  
  new_edata <- icrData$e_data
  new_edata[row_ids, ] <- edata
  
  # remove IDs with duplicate mass formulas
  ind <- dplyr::pull(new_edata, getEDataColName(icrData)) %in% idsToRemove
  new_edata <- new_edata[!ind, ]  
  
  icrData$e_data <- new_edata

  # rescale data if necessary
  if (dataScale != "pres" & dataScale != "abundance") {
    icrData <- edata_transform(icrData, dataScale)
  } else if (dataScale == "pres") {
    # row sums may be > 1 so transform back to 0/1
    if(length(sample_cols) > 1){
      new_edata <- apply(icrData$e_data[, sample_cols], 1:2, function(x) ifelse(!is.na(x) & x > 0, 1, 0))
    }else{
      new_edata <- sapply(edata[, sample_cols], function(x) ifelse(!is.na(x) & x > 0, 1, 0))
    }
    new_edata <- data.frame(dplyr::pull(icrData$e_data, getEDataColName(icrData)), new_edata)
    colnames(new_edata)[1] <- getEDataColName(icrData)
    icrData$e_data <- new_edata
  }
  
  # find rows to remove in e_meta
  ind <- dplyr::pull(icrData$e_meta, getEDataColName(icrData)) %in% idsToRemove
  icrData$e_meta <- icrData$e_meta[!ind, ]  
  
  return(icrData)
}

