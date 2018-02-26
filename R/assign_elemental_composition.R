#' Assign elemental composition to each peak/mass
#' 
#' Assigns an elemental composition class to each peak/mass, where possible, based which of: Carbon, Hydrogen, Oxygen, Nitrogen, Sulfur, and Phosporus are present in the peak/mass formula
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#' 
#' @return an object of the same class as \code{icrData} with a column added in \code{e_meta} giving the class information for each peak/compound, when possible
#' 
#' @author Lisa Bramer
#' 
#' @export

assign_elemental_composition <- function(icrData){
  
  # check icrData #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  # pull e_meta #
  temp = icrData$e_meta
  
  # substitute any digits with nothing #
  temp$ElComposition = gsub(pattern = "[[:digit:]]", replacement = "", x = temp[,getMFColName(icrData)])
  
  # sub e_meta back into data object #
  icrData$e_meta = temp
  
  # assign elcomp cname #
  icrData = setElCompColName(icrData, "ElComposition")
  
  return(icrData)
  
}