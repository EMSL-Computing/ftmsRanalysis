#' Create e\_meta filter object
#'
#' This function returns an emetaFilt object for use with \code{\link{applyFilt}} to filter a 'peakData' or 'compoundData' object based on a column in e\_meta
#' 
#' @param ftmsObj an object of the class 'peakData' or 'compoundData'
#' @param cname character string giving name of column in \code{e_meta} which filter should be based upon.
#'  
#' @return Object of class 'emetaFilt' that contains the molecule identifier and the e\_meta column of interest. This can be used in conjunction with \code{\link{applyFilt}} to filter peaks outside of a specified range of values.
#'
#'
#' @author Lisa Bramer
#' @examples 
#' data("exampleProcessedPeakData")
#' # filter peaks based on Oxygen to Carbon ratio #
#' filter_object1 = emeta_filter(exampleProcessedPeakData, cname = "OtoC_ratio")
#' # filter to keep peaks with O:C ratio greater than or equal to 0.5 #
#' filtered_data = applyFilt(filter_object1, exampleProcessedPeakData, min_val = 0.5)
#' # filter to keep peaks with O:C ratio greater than or equal to 0.5  and less than or equal to 1 #
#' filtered_data = applyFilt(filter_object1, exampleProcessedPeakData, min_val = 0.5, max_val = 1)
#' 
#' # filter peaks based on molecular formula #
#' filter_object2 = emeta_filter(exampleProcessedPeakData, cname = "MolForm")
#' # filter to keep peaks with molecular formula of C12H14O12
#' filtered_data = applyFilt(filter_object2, exampleProcessedPeakData, cats = "C12H14O12")
#' # filter to keep peaks with molecular formula of C12H14O12 and keep NA MolForm peaks
#' filtered_data = applyFilt(filter_object2, exampleProcessedPeakData, cats = "C12H14O12", na.rm = FALSE)

#' @export

emeta_filter <- function(ftmsObj, cname){
  
  # check that ftmsObj is of appropriate class #
  if(!inherits(ftmsObj,"peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be of class 'peakData' or 'compoundData'")
  
  # check that cname is a valid column name in emeta #
  if(!(cname %in% names(ftmsObj$e_meta))) stop(paste(cname, " is not a column in e_meta", sep = ""))
  
  # pull edata column name #
  edata_id = getEDataColName(ftmsObj)
  
  # pull emeta column #
  emeta_val = ftmsObj$e_meta[,cname]
  
  # output #
  output <- data.frame(ftmsObj$e_meta[, edata_id], emeta_val, stringsAsFactors = FALSE)
  names(output) <- c(edata_id, "emeta_value")
  
  # reorder to match e_data:
  ind <- match(ftmsObj$e_data[, edata_id], output[, edata_id])
  output <- output[ind, ]
  
  orig_class <- class(output)
  class(output) <- c("emetaFilt", orig_class)
  
  # store an attribute that tells whether the value column in quantitative or categorical #
  attr(output, "type") = ifelse(class(ftmsObj$e_meta[,cname]) %in% c("character","factor"), "categorical", "quantitative")
  attr(output, "cname") = cname
  return(output)
}
