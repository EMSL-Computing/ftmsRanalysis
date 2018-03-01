#' Calculate DBE and DBE-O Values
#' 
#' Calculate double bond equivalent (DBE) and double bond equivalent minus Oxygen (DBE-O) values for peaks where empirical formula is available
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#' 
#' @details DBE \eqn{= 1 + C - O - S - 0.5(N + P + H)}{= 1 + C - O - S - 0.5*(N + P + H)} and DBE-0 \eqn{= 1 + C - O - S - 0.5(N + P + H) - O}{= 1 + C - O - S - 0.5*(N + P + H) - O}
#' 
#' @references Koch, B. P., & Dittmar, T. (2006). From mass to structure: an aromaticity index for high‐resolution mass data of natural organic matter. Rapid communications in mass spectrometry, 20(5), 926-932. Errata: Koch, B. P., & Dittmar, T. (2016). From mass to structure: an aromaticity index for high-resolution mass data of natural organic matter. Rapid communications in mass spectrometery, 30(1), 250. DOI: 10.1002/rcm.7433
#' 
#' @return an object of the same class as \code{icrData} with a column in \code{e\_meta} giving DBE and DBE-O values
#'  
#' @author Lisa Bramer, Allison Thompson
#' 

calc_dbe <- function(icrData){
  
  
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  # pull e_meta out of icrData #
  temp = icrData$e_meta
  
  temp$DBE = 1 + temp[,getCarbonColName(icrData)] - temp[,getOxygenColName(icrData)] - temp[,getSulfurColName(icrData)] - 
    0.5*(temp[,getNitrogenColName(icrData)] + temp[,getPhosphorusColName(icrData)] + temp[,getHydrogenColName(icrData)])
  temp$DBE_O = temp$DBE - temp[,getOxygenColName(icrData)]
  
  # Old formula, from original paper
  #temp$DBE = 1 + 0.5*(2*temp[,getCarbonColName(icrData)] - temp[,getHydrogenColName(icrData)] + temp[,getNitrogenColName(icrData)] + temp[,getPhosphorusColName(icrData)])
  
  if(length(which(is.na(temp[,getMFColName(icrData)]))) > 0){
    temp$DBE[which(is.na(temp[,getMFColName(icrData)]))] = NA
    temp$DBE_O[which(is.na(temp[,getMFColName(icrData)]))] = NA
  }

  icrData$e_meta = temp
  
  setDBEColName(icrData, "DBE")
  setDBEoColName(icrData, "DBE_O")
  
  return(icrData)
}