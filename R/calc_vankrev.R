#' Calculate Features for Van Krevelen Plots
#' 
#' Calculates the Oxygen to Carbon and Hydrogen to Carbon ratios needed for Van Krevelen plots
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}. e_meta must be present.
#'
#' @return an object of the same class as \code{icrData} with columns in \code{e_meta} giving Oxygen to Carbon and Hydrogen to Carbon ratios
#' @author Lisa Bramer
#' 


calc_vankrev <- function(icrData){
  
  # check that e_meta is not NULL #
  if(is.null(icrData$e_meta)) stop("e_meta in icrData is NULL and must be present to use this function")
  
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  # check that icrData doesn't already have cnames specified for ratios in e_meta #
  if(!is.null(getOCRatioColName(icrData)) | !is.null(getHCRatioColName(icrData))) message("o2c_cname and/or h2c_cname were already specified and will be overwritten")
  
  c_cname = getCarbonColName(icrData)
  h_cname = getHydrogenColName(icrData)
  o_cname = getOxygenColName(icrData)
  
  # check that all the cnames are character strings #
  if(class(c_cname) != "character") stop("c_cname must be a character string")
  if(class(h_cname) != "character") stop("h_cname must be a character string")
  if(class(o_cname) != "character") stop("o_cname must be a character string")

  # pull e_meta out of icrData #
  temp = icrData$e_meta

  # calculate o:c ratio #
  temp$OtoC_ratio = temp[,o_cname]/temp[,c_cname]
  
  # calculate h:c ratio #
  temp$HtoC_ratio = temp[,h_cname]/temp[,c_cname]
  
  if(length(which(is.na(temp[,getMFColName(icrData)]))) > 0){
    temp$OtoC_ratio[which(is.na(temp[,getMFColName(icrData)]))] = NA
    temp$HtoC_ratio[which(is.na(temp[,getMFColName(icrData)]))] = NA
  }
  
  # reinsert temp into icrData #
  icrData$e_meta = temp
  
  # assign column names #
  res1 = setOCRatioColName(icrData, "OtoC_ratio")
  res2 = setHCRatioColName(res1, "HtoC_ratio")
  
  return(res2)
  }