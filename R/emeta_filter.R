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
#'
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