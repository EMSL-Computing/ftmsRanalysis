#' Calculate Nominal Oxidation State of Carbon (NOSC) Values
#' 
#' Calculate NOSC values for peaks where empirical formula is available
#' 
#' @param icrData an object of class 'peakData' or 'compoundIcrData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}. 
#' 
#' @details NOSC \eqn{= -(\frac{4C + H - 3N - 2O + 5P - 2S}{C}) + 4}{= -((4*C + H - 3*N - 2*O + 5*P - 2*S)/(C)) + 4}
#' 
#' @return an object of the same class as \code{icrData} with a column in \code{e\_meta} giving NOSC values
#'  
#' @author Lisa Bramer
#'


calc_nosc <- function(icrData){
  

  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakData' or 'compoundIcrData'")
  
    # pull e_meta out of icrData #
    temp = icrData$e_meta
    
    temp$NOSC = 4 - ((4*temp[,getCarbonColName(icrData)] + temp[,getHydrogenColName(icrData)] - 3*temp[,getNitrogenColName(icrData)] - 2*temp[,getOxygenColName(icrData)] + 5*temp[,getPhosphorusColName(icrData)] - 2*temp[,getSulfurColName(icrData)])/temp[,getCarbonColName(icrData)])
    
    if(length(which(is.na(temp[,getMFColName(icrData)]))) > 0){
      temp$NOSC[which(is.na(temp[,getMFColName(icrData)]))] = NA
    }
    
    icrData$e_meta = temp
    
    icrData = setNOSCColName(icrData, "NOSC")
    return(icrData)
}