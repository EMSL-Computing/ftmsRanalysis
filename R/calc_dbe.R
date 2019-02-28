#' Calculate DBE and DBE-O Values
#' 
#' Calculate double bond equivalent (DBE) and double bond equivalent minus Oxygen (DBE-O) values for peaks where empirical formula is available
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}.
#' 
#' @details DBE \eqn{= 1 + C - O - S - 0.5(N + P + H)}{= 1 + C - O - S - 0.5*(N + P + H)} and DBE-0 \eqn{= 1 + C - O - S - 0.5(N + P + H) - O}{= 1 + C - O - S - 0.5*(N + P + H) - O}
#' 
#' @references Koch, B. P., & Dittmar, T. (2006). From mass to structure: an aromaticity index for high‐resolution mass data of natural organic matter. Rapid communications in mass spectrometry, 20(5), 926-932. 
#' @references Errata: Koch, B. P., & Dittmar, T. (2016). From mass to structure: an aromaticity index for high-resolution mass data of natural organic matter. Rapid communications in mass spectrometery, 30(1), 250. DOI: 10.1002/rcm.7433
#' 
#' @return an object of the same class as \code{ftmsObj} with a column in \code{e\_meta} giving DBE, DBE-O, and DBE_AI values
#'  
#' @author Lisa Bramer, Allison Thompson
#' 

calc_dbe <- function(ftmsObj){
  
  
  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # pull e_meta out of ftmsObj #
  temp = ftmsObj$e_meta
  
  temp$DBE = 1 + 0.5*(2*temp[,getCarbonColName(ftmsObj)] - temp[,getHydrogenColName(ftmsObj)]+ temp[,getNitrogenColName(ftmsObj)] + temp[,getPhosphorusColName(ftmsObj)])
  
  temp$DBE_O = temp$DBE - temp[,getOxygenColName(ftmsObj)]
  
  temp$DBE_AI = 1 + temp[,getCarbonColName(ftmsObj)] - temp[,getOxygenColName(ftmsObj)] - temp[,getSulfurColName(ftmsObj)] - 
    0.5*(temp[,getNitrogenColName(ftmsObj)] + temp[,getPhosphorusColName(ftmsObj)] + temp[,getHydrogenColName(ftmsObj)])
  

  if(length(which(is.na(temp[,getMFColName(ftmsObj)]))) > 0){
    temp$DBE[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
    temp$DBE_O[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
    temp$DBE_AI[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
  }

  ftmsObj$e_meta = temp
  
  ftmsObj = setDBEColName(ftmsObj, "DBE")
  ftmsObj = setDBEoColName(ftmsObj, "DBE_O")
  ftmsObj = setDBEAIColName(ftmsObj, "DBE_AI")
  
  return(ftmsObj)
}