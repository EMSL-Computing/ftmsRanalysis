#' Calculate Elemental Ratios
#' 
#' Calculates the Oxygen:Carbon, Hydrogen:Carbon, Nitrogen:Carbon, Phosphorus:Carbon, and Nitrogen:Phosphorus ratios
#' 
#' @param icrData an object of class 'peakData' or 'compoundIcrData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}. e_meta must be present.
#'
#' @return an object of the same class as \code{icrData} with columns in \code{e_meta} giving elemental ratios
#' @author Lisa Bramer
#' 


calc_element_ratios <- function(icrData){
  
  # check that e_meta is not NULL #
  if(is.null(icrData$e_meta)) stop("e_meta in icrData is NULL and must be present to use this function")
  
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakData' or 'compoundIcrData'")
  
  # check that icrData doesn't already have cnames specified for ratios in e_meta #
  if(!is.null(getOCRatioColName(icrData)) | !is.null(getHCRatioColName(icrData))) message("o2c_cname and/or h2c_cname were already specified and will be overwritten")
  
  c_cname = getCarbonColName(icrData)
  h_cname = getHydrogenColName(icrData)
  o_cname = getOxygenColName(icrData)
  n_cname = getNitrogenColName(icrData)
  s_cname = getSulfurColName(icrData)
  p_cname = getPhosphorusColName(icrData)
  
  # pull e_meta out of icrData #
  temp = icrData$e_meta

  # calculate o:c ratio #
  temp$OtoC_ratio = temp[,o_cname]/temp[,c_cname]
  
  # calculate h:c ratio #
  temp$HtoC_ratio = temp[,h_cname]/temp[,c_cname]
  
  # calculate n:c ratio #
  temp$NtoC_ratio = temp[,n_cname]/temp[,c_cname]
  
  # calculate p:c ratio #
  temp$PtoC_ratio = temp[,p_cname]/temp[,c_cname]
  
  # calculate n:p ratio #
  temp$NtoP_ratio = temp[,n_cname]/temp[,p_cname]
  
  temp$NtoP_ratio[which(temp[,p_cname] == 0)] = NA
  
  if(length(which(is.na(temp[,getMFColName(icrData)]))) > 0){
    temp$OtoC_ratio[which(is.na(temp[,getMFColName(icrData)]))] = NA
    temp$HtoC_ratio[which(is.na(temp[,getMFColName(icrData)]))] = NA
    temp$NtoC_ratio[which(is.na(temp[,getMFColName(icrData)]))] = NA
    temp$PtoC_ratio[which(is.na(temp[,getMFColName(icrData)]))] = NA
    temp$NtoP_ratio[which(is.na(temp[,getMFColName(icrData)]))] = NA
  }
  
  # reinsert temp into icrData #
  icrData$e_meta = temp
  
  # assign column names #
  res1 = setOCRatioColName(icrData, "OtoC_ratio")
  res2 = setHCRatioColName(res1, "HtoC_ratio")
  res3 = setNCRatioColName(res2, "NtoC_ratio")
  res4 = setPCRatioColName(res3, "PtoC_ratio")
  res5 = setNPRatioColName(res4, "NtoP_ratio")
  
  return(res5)
  }