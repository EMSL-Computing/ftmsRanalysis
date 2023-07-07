#' Calculate Elemental Ratios
#' 
#' Calculates the ratios of counts of specified element/isotope pairs.
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}. e_meta must be present.
#' @param ratios a matrix of size 2xN where N is the number of ratios to calculate. Defaults to calculating O:C, H:C, N:C, P:C, and N:P. As a matrix: 
#'
#' | O | H | N | P | N |
#' |---|---|---|---|---|
#' | C | C | C | C | P |
#' 
#' 
#' @return an object of the same class as \code{ftmsData} with columns in \code{e_meta} giving elemental ratios
#' @author Lisa Bramer
#' @md


calc_element_ratios <- function(ftmsObj, ratios=NULL){
  
  # check that e_meta is not NULL #
  if(is.null(ftmsObj$e_meta)) stop("e_meta in ftmsData is NULL and must be present to use this function")
  
  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # check that ftmsObj doesn't already have cnames specified for ratios in e_meta #
  if(!is.null(getOCRatioColName(ftmsObj)) | !is.null(getHCRatioColName(ftmsObj))) message("o2c_cname and/or h2c_cname were already specified and will be overwritten")
  
  if (is.null(ratios)) {
    ratio_mat <- matrix(data = c("O","C","H","C","N","C","P","C","N","P"), nrow = 2)
  } else {
    if(!is.matrix(ratios)) stop("ratios must be of type matrix")
    if(dim(ratios)[1] != 2) stop("ratios matrix must be of size 2xN where N is number of ratios to be calculated. Must be 2 rows.")
    ratio_mat <- ratios
  }
  
  # pull e_meta out of ftmsObj #
  temp = ftmsObj$e_meta
  
  # Iterate through matrix columns and calculate ratios
  for (i in 1:ncol(ratio_mat)) {
    ratio_col_name <- paste0(ratio_mat[1,i], "to", ratio_mat[2,i], "_ratio")
    temp[[ratio_col_name]] <- temp[,getElementColName(ftmsObj, ratio_mat[1,i])] / temp[,getElementColName(ftmsObj, ratio_mat[2,i])]
    # Check if value is null and assign NA if so
    #temp[ratio_col_name][which(is.null(ratio_col_name))] <- NA
  }
  
  # reinsert temp into ftmsObj #
  ftmsObj$e_meta = temp
  
  # assign column names #
  for (i in 1:ncol(ratio_mat)) {
    ratio_pair <- paste0(ratio_mat[1,i], ":", ratio_mat[2,i])
    ratio_col_name <- paste0(ratio_mat[1,i], "to", ratio_mat[2,i], "_ratio")
    ftmsObj = setRatioColName(ftmsObj, ratio_pair, ratio_col_name)
  }
  
  return(ftmsObj)
}