#' Formula filter object
#'
#' This function returns a formulaeFilt object for use with \code{\link{applyFilt}}
#' 
#' @param icrData an object of the class 'peakIcrData' or 'compoundIcrData'
#'
#' @return Object of class 'formulaFilt' that contains the molecule identifier and whether a formula could be assigned or not. This can be used in conjunction with \code{\link{applyFilt}} to filter peaks without a formula assigned.
#'
#'
#' @author Lisa Bramer
#'
#' @export

formula_filter <- function(icrData){
  
  # check that icrData is of appropriate class #
  if(!inherits(icrData,"peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be of class 'peakIcrData' or 'compoundIcrData'")
  
  edata_id = getEDataColName(icrData)
  
  # determine whether a peak has a formula assigned or not #
  form_assigned = !is.na(icrData$e_meta[,getMFColName(icrData)])
  
  # output #
  output <- data.frame(icrData$e_data[, edata_id], form_assigned)
  names(output) <- c(edata_id, "Formula_Assigned")
  
  orig_class <- class(output)
  class(output) <- c("formulaFilt", orig_class)
  
  return(output)
}