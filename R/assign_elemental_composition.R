#' Assign elemental composition to each peak/mass
#' 
#' Assigns an elemental composition class to each peak/mass, where possible, based which of: Carbon, Hydrogen, Oxygen, Nitrogen, Sulfur, and Phosporus are present in the peak/mass formula
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}.
#' 
#' @return an object of the same class as \code{ftmsObj} with a column added in \code{e_meta} giving the class information for each peak/compound, when possible
#' 
#' @author Lisa Bramer
#' 
#' @export

assign_elemental_composition <- function(ftmsObj){
  
  # check ftmsObj #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # pull e_meta #
  temp = ftmsObj$e_meta
  
  # substitute any digits with nothing #
  temp$ElComposition = gsub(pattern = "[[:digit:]]", replacement = "", x = temp[,getMFColName(ftmsObj)])
  
  # sub e_meta back into data object #
  ftmsObj$e_meta = temp
  
  # assign elcomp cname #
  ftmsObj = setElCompColName(ftmsObj, "ElComposition")
  
  return(ftmsObj)
  
}