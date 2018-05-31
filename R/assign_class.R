#' Assign compound class to each peak/mass
#' 
#' Assigns a compound class to each peak/mass, where possible, based Oxygen:Carbon ratio and Hydrogen:Carbon ratio and a chosen boundary set.
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#' @param boundary_set character vector specifying which boundary set to use when determining class. Valid options are currently "bs1" and "bs2" and defaults to "bs1". 
#' @param calc_ratios logical argument, if elemental ratios needed for class assignment are not already calculated, should they be added to the data? Defaults to TRUE.
#' 
#' @details Information about the upper and lower bounds used in each boundary set can be obtained using \code{\link{getVanKrevelenCategories}} or a graphical representation can be obtained using \code{\link{plot_vkBoundarySet}} in conjunction with \code{\link{getVanKrevelenCategories}}.
#' 
#' @return an object of the same class as \code{icrData} with a column added in \code{e_meta} giving the class information for each peak/compound, when possible
#' 
#' @author Lisa Bramer
#' 
#' @export

assign_class <- function(icrData, boundary_set = "bs1", calc_ratios = TRUE){
  
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  # check that boundary_set is valid argument #
  if(!(boundary_set %in% c("bs1", "bs2"))) stop("Invalid option provided for boundary_set argument.")
  
  # check that calc_vankrev is logical #
  if(class(calc_ratios) != "logical") stop("calc_ratios must be of class 'logical'")
  
  # check that O:C and H:C are non-NULL #
  # if they are NULL check calc_vankrev argument and act accordingly #
  if(is.null(getOCRatioColName(icrData)) | is.null(getHCRatioColName(icrData))){
    if(calc_ratios == TRUE){
      icrData = compound_calcs(icrData, "calc_element_ratios")
    }else{
      stop("Elemental ratio data (e.g. O:C) not found in data and calc_ratios = FALSE. Set calc_ratios = TRUE or ensure ratios are present and specified in your data")
    }
  }
  
  # get boundary set data.frame #
  bound_df = getVanKrevelenCategoryBounds(boundary_set)
  bound_match = bound_df[-which(rownames(bound_df) == "Other"),]

  getclass <- function(x, y){
    temp = rownames(bound_match)[which(bound_match$OC.low <= x &bound_match$OC.high >= x & bound_match$HC.low <= y & bound_match$HC.high >= y)]
    if(length(temp) > 0){
      paste(temp, collapse = ";")
    }else{
      "Other"
    }
  }
  
  # get e_meta #
  temp = icrData$e_meta
  
  # get the compound classes #
  comp_class = mapply(x = temp[,getOCRatioColName(icrData)], y = temp[,getHCRatioColName(icrData)], getclass)
  
  # check rows where no formula is assigned #
  noform = which(is.na(temp[,getMFColName(icrData)]))
  
  # if these exist, then set their compound class to NA #
  if(length(noform) > 0){
    comp_class[noform] = NA
  }
  
  # add the column to e_meta #
  temp$Class = comp_class
  
  # reassign temp back to e_meta in icrData #
  icrData$e_meta = temp
  
  # set the classcname #
  icrData = setClassColName(icrData, "Class")
  
  # return icrData #
  return(icrData)
}