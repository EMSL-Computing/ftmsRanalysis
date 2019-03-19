#' Calculate Elemental Ratios
#' 
#' Calculates the Oxygen:Carbon, Hydrogen:Carbon, Nitrogen:Carbon, Phosphorus:Carbon, and Nitrogen:Phosphorus ratios
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}. e_meta must be present.
#'
#' @return an object of the same class as \code{ftmsData} with columns in \code{e_meta} giving elemental ratios
#' @author Lisa Bramer
#' 


calc_element_ratios <- function(ftmsObj){
  
  # check that e_meta is not NULL #
  if(is.null(ftmsObj$e_meta)) stop("e_meta in ftmsData is NULL and must be present to use this function")
  
  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # check that ftmsObj doesn't already have cnames specified for ratios in e_meta #
  if(!is.null(getOCRatioColName(ftmsObj)) | !is.null(getHCRatioColName(ftmsObj))) message("o2c_cname and/or h2c_cname were already specified and will be overwritten")
  
  c_cname = getCarbonColName(ftmsObj)
  h_cname = getHydrogenColName(ftmsObj)
  o_cname = getOxygenColName(ftmsObj)
  n_cname = getNitrogenColName(ftmsObj)
  s_cname = getSulfurColName(ftmsObj)
  p_cname = getPhosphorusColName(ftmsObj)
  
  # pull e_meta out of ftmsObj #
  temp = ftmsObj$e_meta

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
  
  if(length(which(is.na(temp[,getMFColName(ftmsObj)]))) > 0){
    temp$OtoC_ratio[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
    temp$HtoC_ratio[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
    temp$NtoC_ratio[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
    temp$PtoC_ratio[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
    temp$NtoP_ratio[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
  }
  
  # reinsert temp into ftmsObj #
  ftmsObj$e_meta = temp
  
  # assign column names #
  res1 = setOCRatioColName(ftmsObj, "OtoC_ratio")
  res2 = setHCRatioColName(res1, "HtoC_ratio")
  res3 = setNCRatioColName(res2, "NtoC_ratio")
  res4 = setPCRatioColName(res3, "PtoC_ratio")
  res5 = setNPRatioColName(res4, "NtoP_ratio")
  
  return(res5)
  }