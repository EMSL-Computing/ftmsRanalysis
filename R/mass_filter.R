#' Filter Data Based on Mass Range
#' 
#' Subset data based on specified range of mass values 
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#' 
#' @details Use this in conjunction with \code{\link{applyFilt}} to retain peaks which have a mass between \code{min_mass} and \code{max_mass} (inclusive) and filter all other peaks from the data.
#' 
#' @return an object of class 'massFilt' 
#' 
#' @author Lisa Bramer
#' @export
#'
#' @examples
#' filter_obj <- mass_filter(peakIcrData)
#' peakIcrDataFiltered <- applyFilt(filter_obj, min_mass=200, max_mass=900)
mass_filter <- function(icrData){
  

  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  

  # pull edata and mass cnames #
  edata_id = getEDataColName(icrData)
  mass_id = getMassColName(icrData)
  
  # pull two columns from e_meta #
  res = data.frame(ID = icrData$e_meta[,edata_id], icrData$e_meta[,mass_id])
  names(res)[2] = mass_id
  
  # make sure mass column is numeric #
  res[,mass_id] = as.numeric(as.character(res[,mass_id]))
  
  class(res) = c("massFilt", "data.frame")

  return(res)
}

#######################################################################################
# Internal only functions for applying mass filters to KEGG and MetaCyc compounds
# Need to transform mass slightly (- 1.007276) before filtering to be comparable with icrData peaks

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

kegg_mass_filter <- function(compounds=KeggData::kegg_compounds, min_mass, max_mass) {
  # check that compounds is not NULL 
  if(is.null(compounds)) stop("compounds is NULL and must be present to use this function")
  
  # check that all the cnames are character strings #
  if(class(min_mass) != "numeric") stop("min_mass must be numeric")
  if(class(max_mass) != "numeric") stop("max_mass must be numeric")
  
  cname <- "EXACT_MASS"
  
  ind.na <- is.na(compounds[, cname])
  ind <- compounds[, cname] - 1.007276 >= min_mass & compounds[, cname] - 1.007276 <= max_mass
  res <- compounds[!ind.na & ind, ]
  return(res)
}

