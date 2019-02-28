#' Calculate Kendrick Mass and Defect
#' 
#' Calculates the Kendrick mass and Kendrick defect needed for Kendrick plots
#' 
#' @param icrData an object of class 'peakData' or 'compoundIcrData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}. e_meta must be present.
#'
#' @return an object of the same class as \code{icrData} with columns in \code{e_meta} giving Kendrick mass and defects
#' 
#' @author Lisa Bramer
#'

calc_kendrick <- function(icrData){
  

  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakData' or 'compoundIcrData'")
  
  # check that icrData doesn't already have cnames specified for ratios in e_meta #
  if(!is.null(getKendrickDefectColName(icrData)) | !is.null(getKendrickMassColName(icrData))) message("mass_cname and/or defect_cname were already specified and will be overwritten")
  
  mass_cname = getMassColName(icrData)
  
  # check that all the cnames are character strings #
  if(class(mass_cname) != "character") stop("mass_cname must be a character string")
 

  
  # pull e_meta out of icrData #
  temp = icrData$e_meta
  
  # calculate kendrick mass #
  temp$kmass = as.numeric(as.character(temp[,mass_cname]))*(14/14.01565)
  
  # calculate kendrick defect #
  temp$kdefect = ceiling(temp$kmass) - temp$kmass
  
  # reinsert temp into icrData #
  icrData$e_meta = temp
  
  # assign column names #
  res1 = setKendrickMassColName(icrData, "kmass")
  res2 = setKendrickDefectColName(res1, "kdefect")
  
  return(res2)
}