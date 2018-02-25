#' Calculate Aromaticity and Modified Aromaticity Values
#' 
#' Calculate aromaticity and modified aromaticity index values for peaks where empirical formula is available
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#' 
#' @details AI \eqn{= \frac{1 + C - O - S - 0.5H}{C - O - S - N - P}}{= (1 + C - O - S - 0.5* H)/(C - O - S - N - P)} and AI_MOD \eqn{= \frac{1 + C - 0.5O - S - 0.5H}{C - 0.5*O - S - N - P}}{= (1 + C - 0.5*O - S - 0.5*H)/(C - 0.5*O - S - N - P)}. When the numerator or denomenator is <=0, AI and AI_MOD = 0.
#' When AI > 0.5 a compound is considered not aromatic, 
#' 0.5 < = AI <0.67 a compound is aromatic, and when AI >= 0.67 a compound is a condensed aromatic
#' 
#' @references Koch, B. P., & Dittmar, T. (2006). From mass to structure: an aromaticity index for high‐resolution mass data of natural organic matter. Rapid communications in mass spectrometry, 20(5), 926-932.
#' 
#' @return an object of the same class as \code{icrData} with a column in \code{e\_meta} giving aromaticity and modified aromaticity values
#'  
#' @author Lisa Bramer
#' 

calc_aroma <- function(icrData){
  
  
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  # pull e_meta out of icrData #
  temp = icrData$e_meta
  
  ai_num = (1 + temp[,getCarbonColName(icrData)] - temp[,getOxygenColName(icrData)] - temp[,getSulfurColName(icrData)] - 0.5*temp[,getHydrogenColName(icrData)])
  ai_denom = (temp[,getCarbonColName(icrData)] - temp[,getOxygenColName(icrData)] - temp[,getSulfurColName(icrData)] - temp[,getNitrogenColName(icrData)] - temp[,getPhosphorusColName(icrData)])
  
  # pull out automatic AI <= 0 cases #
  ai_ids = which(ai_num <= 0 | ai_denom <= 0)

  aimod_num = (1 + temp[,getCarbonColName(icrData)] - 0.5*temp[,getOxygenColName(icrData)] - temp[,getSulfurColName(icrData)] - 0.5*temp[,getHydrogenColName(icrData)])
  aimod_denom = (temp[,getCarbonColName(icrData)] - 0.5*temp[,getOxygenColName(icrData)] - temp[,getSulfurColName(icrData)] - temp[,getNitrogenColName(icrData)] - temp[,getPhosphorusColName(icrData)])
  
  # pull out automatic AI = 0 cases #
  aimod_ids = which(aimod_num <= 0 | aimod_denom <= 0)
  
  temp$AI = ai_num/ai_denom
  if(length(ai_ids) > 0){temp$AI[ai_ids] = 0}

  temp$AI_Mod = aimod_num/aimod_denom
  if(length(aimod_ids) > 0){temp$AI_Mod[aimod_ids] = 0}
  
  if(length(which(is.na(temp[,getMFColName(icrData)]))) > 0){
  temp$AI[which(is.na(temp[,getMFColName(icrData)]))] = NA
  temp$AI_Mod[which(is.na(temp[,getMFColName(icrData)]))] = NA
  }
  
  icrData$e_meta = temp
  
  setAromaColName(icrData, "AI")
  setModAromaColName(icrData, "AI_Mod")
  
  return(icrData)
}