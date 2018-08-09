#' Create e\_meta filter object
#'
#' This function returns an emetaFilt object for use with \code{\link{applyFilt}} to filter a 'peakIcrData' or 'compoundIcrData' object based on a column in e\_meta
#' 
#' @param icrData an object of the class 'peakIcrData' or 'compoundIcrData'
#' @param cname character string giving name of column in \code{e_meta} which filter should be based upon.
#'  
#' @return Object of class 'emetaFilt' that contains the molecule identifier and the e\_meta column of interest. This can be used in conjunction with \code{\link{applyFilt}} to filter peaks outside of a specified range of values.
#'
#'
#' @author Lisa Bramer
#' @examples 
#' data("peakIcrProcessed")
#' # filter peaks based on Oxygen to Carbon ratio #
#' filter_object1 = emeta_filter(peakIcrProcessed, cname = "OtoC_ratio")
#' # filter to keep peaks with O:C ratio greater than or equal to 0.5 #
#' filtered_data = applyFilt(filter_object1, peakIcrProcessed, min_val = 0.5)
#' # filter to keep peaks with O:C ratio greater than or equal to 0.5  and less than or equal to 1 #
#' filtered_data = applyFilt(filter_object1, peakIcrProcessed, min_val = 0.5, max_val = 1)
#' 
#' # filter peaks based on molecular formula #
#' filter_object2 = emeta_filter(peakIcrProcessed, cname = "MolForm")
#' # filter to keep peaks with molecular formula of C12H14O12
#' filtered_data = applyFilt(filter_object2, peakIcrProcessed, cats = "C12H14O12")
#' # filter to keep peaks with molecular formula of C12H14O12 and keep NA MolForm peaks
#' filtered_data = applyFilt(filter_object2, peakIcrProcessed, cats = "C12H14O12", na.rm = F)

#' @export

emeta_filter <- function(icrData, cname){
  
  # check that icrData is of appropriate class #
  if(!inherits(icrData,"peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be of class 'peakIcrData' or 'compoundIcrData'")
  
  # check that cname is a valid column name in emeta #
  if(!(cname %in% names(icrData$e_meta))) stop(paste(cname, " is not a column in e_meta", sep = ""))
  
  # pull edata column name #
  edata_id = getEDataColName(icrData)
  
  # pull emeta column #
  emeta_val = icrData$e_meta[,cname]
  
  # output #
  output <- data.frame(icrData$e_meta[, edata_id], emeta_val)
  names(output) <- c(edata_id, "emeta_value")
  
  # reorder to match e_data:
  ind <- match(icrData$e_data[, edata_id], output[, edata_id])
  output <- output[ind, ]
  
  orig_class <- class(output)
  class(output) <- c("emetaFilt", orig_class)
  
  # store an attribute that tells whether the value column in quantitative or categorical #
  attr(output, "type") = ifelse(class(icrData$e_meta[,cname]) %in% c("character","factor"), "categorical", "quantitative")
  attr(output, "cname") = cname
  return(output)
}