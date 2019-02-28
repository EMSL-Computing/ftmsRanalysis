#' Combine rows of an ftmsData object with the same mass formula
#' 
#' If multiple peaks are mapped to the same mass formula it can cause problems 
#' or inconsistencies when mapping data to a biological database. The
#' \code{combinePeaksWithSameFormula} combines rows of \code{ftmsObj$e_data}
#' that map to the same mass formula:
#' * If data is abundance: values are summed for each column
#' * If data is log scale: data are converted to abundance, then values are summed for each columns and re-transformed to the original scale
#' * If data is presence/absence: 'or' function is applied to each column (if at least one peak is observed, the peak is denoted as observed)
#' 
#' It is recommended to call the \code{combinePeaksWithSameFormula} function
#' before using \code{mapPeaksToCompounds}.
#'
#' @param ftmsObj data of type \code{peakData}
#'
#' @return \code{peakData} object where now two rows of \code{e_data} map to the same mass formula
#' @export
#'
#' @examples
#' new_data <- combinePeaksWithSameFormula(exampleProcessedPeakData)
combinePeaksWithSameFormula <- function(ftmsObj) {
  if (!inherits(ftmsObj, "peakData") ) {
    stop("ftmsObj must be an object of type 'peakData'")
  }
  if (is.null(getMFColName(ftmsObj))) {
    stop("mass formula column name is not set")
  }
  if (is.null(getMassColName(ftmsObj))) {
    stop("mass column name is not set")
  }
  validScales <- c('log2', 'log10', 'log', 'pres', 'abundance')
  dataScale <- getDataScale(ftmsObj)
  if (!(dataScale %in% validScales)) {
    stop(sprintf("the data scale of ftmsObj must one of: (%s)", paste(validScales, collapse=", ")))
  }
  
  # determine which rows of e_data need to be combined
  if (inherits(ftmsObj, "peakData")) {
    mf_col <- getMFColName(ftmsObj)
    massMFComb <- dplyr::select_(ftmsObj$e_meta, getEDataColName(ftmsObj), getMassColName(ftmsObj), getMFColName(ftmsObj)) %>%
      dplyr::rename_(Mass=getMassColName(ftmsObj), MF=getMFColName(ftmsObj)) %>%
      dplyr::filter(!is.na(MF))
    if (anyDuplicated(massMFComb$MF) > 0) { # identify dups
      tmp <- table(massMFComb$MF)
      dupMF <- names(tmp)[tmp>1]
      massMFComb <- massMFComb %>%
        dplyr::filter(MF %in% dupMF)
      message(sprintf("%d mass formulas with duplicate values found", length(dupMF)))
    } else {
      message("No duplicates found")
      return(ftmsObj)
    }
    if (getMassColName(ftmsObj) != getEDataColName(ftmsObj)) {
      massMFComb <- dplyr::select(massMFComb, -Mass)
    }
    
  }
  
  # transform data scale back to abundance for summing rows
  if (dataScale != "pres" & dataScale != "abundance") {
    ftmsObj <- edata_transform(ftmsObj, "abundance")
  }
  
  sample_cols <- as.character(dplyr::pull(ftmsObj$f_data, getFDataColName(ftmsObj)))
  
  edata <- suppressWarnings(
    ftmsObj$e_data %>%
    dplyr::mutate(index=1:nrow(ftmsObj$e_data)) %>%
    dplyr::right_join(massMFComb, by=getEDataColName(ftmsObj)) %>%
    dplyr::group_by(MF) 
  )
  
  # get first ID and index for each MF
  massMFFinal <- edata %>%
    dplyr::rename_(ID=getEDataColName(ftmsObj)) %>%
    dplyr::summarize(ID=dplyr::first(ID), index=min(index))
  colnames(massMFFinal)[2] <- getEDataColName(ftmsObj)
  
  # get IDs to remove
  idsToRemove <- setdiff(dplyr::pull(massMFComb, getEDataColName(ftmsObj)), 
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
  edata <- as.data.frame(edata[, c(getEDataColName(ftmsObj), sample_cols)])
  
  new_edata <- ftmsObj$e_data
  new_edata[row_ids, ] <- edata
  
  # remove IDs with duplicate mass formulas
  ind <- dplyr::pull(new_edata, getEDataColName(ftmsObj)) %in% idsToRemove
  new_edata <- new_edata[!ind, ]  
  
  ftmsObj$e_data <- new_edata

  # rescale data if necessary
  if (dataScale != "pres" & dataScale != "abundance") {
    ftmsObj <- edata_transform(ftmsObj, dataScale)
  } else if (dataScale == "pres") {
    # row sums may be > 1 so transform back to 0/1
    if(length(sample_cols) > 1){
      new_edata <- apply(ftmsObj$e_data[, sample_cols], 1:2, function(x) ifelse(!is.na(x) & x > 0, 1, 0))
    }else{
      new_edata <- sapply(edata[, sample_cols], function(x) ifelse(!is.na(x) & x > 0, 1, 0))
    }
    new_edata <- data.frame(dplyr::pull(ftmsObj$e_data, getEDataColName(ftmsObj)), new_edata)
    colnames(new_edata)[1] <- getEDataColName(ftmsObj)
    ftmsObj$e_data <- new_edata
  }
  
  # find rows to remove in e_meta
  ind <- dplyr::pull(ftmsObj$e_meta, getEDataColName(ftmsObj)) %in% idsToRemove
  ftmsObj$e_meta <- ftmsObj$e_meta[!ind, ]  
  
  return(ftmsObj)
}

