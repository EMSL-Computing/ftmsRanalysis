#' Filter Data Based on Mass Range
#' 
#' Subset data based on specified range of mass values 
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}.
#' 
#' @details Use this in conjunction with \code{\link{applyFilt}} to retain peaks which have a mass between \code{min_mass} and \code{max_mass} (inclusive) and filter all other peaks from the data.
#' 
#' @return an object of class 'massFilt' 
#' 
#' @author Lisa Bramer
#' @export
#'
#' @examples
#' data("examplePeakData")
#' filter_obj <- mass_filter(examplePeakData)
#' peakDataFiltered <- applyFilt(filter_obj, examplePeakData, min_mass=200, max_mass=900)
mass_filter <- function(ftmsObj){
  

  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")

  # pull edata and mass cnames #
  edata_id = getEDataColName(ftmsObj)
  mass_id = getMassColName(ftmsObj)
  
  # pull two columns from e_meta #
  res = data.frame(ID = ftmsObj$e_meta[,edata_id], ftmsObj$e_meta[,mass_id])
  names(res)[2] = mass_id
  
  # make sure mass column is numeric #
  res[,mass_id] = as.numeric(as.character(res[,mass_id]))
  
  class(res) = c("massFilt", "data.frame")

  return(res)
}

#######################################################################################
# Internal only functions for applying mass filters to MetaCyc compounds
# Need to transform mass slightly (- 1.007276) before filtering to be comparable with ftmsData peaks

metacyc_mass_filter <- function(compounds=MetaCycData::mc_compounds, min_mass, max_mass) {
  # check that compounds is not NULL #
  if(is.null(compounds)) stop("compounds is NULL and must be present to use this function")
  
  # check that all the cnames are character strings #
  if(class(min_mass) != "numeric") stop("min_mass must be numeric")
  if(class(max_mass) != "numeric") stop("max_mass must be numeric")
  
  cname <- "MONOISOTOPIC-MW" # "MOLECULAR-WEIGHT"
  
  ind.na <- is.na(compounds[, cname])
  ind <- compounds[, cname] - 1.007276 >= min_mass & compounds[, cname] - 1.007276 <= max_mass
  res <- compounds[!ind.na & ind, ]
  return(res)
}
