#' Calculate Nominal Oxidation State of Carbon (NOSC) Values
#' 
#' Calculate NOSC values for peaks where empirical formula is available
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}. 
#' 
#' @details NOSC \eqn{= -(\frac{4C + H - 3N - 2O + 5P - 2S}{C}) + 4}{= -((4*C + H - 3*N - 2*O + 5*P - 2*S)/(C)) + 4}
#' 
#' @return an object of the same class as \code{ftmsData} with a column in \code{e\_meta} giving NOSC values
#'  
#' @author Lisa Bramer
#'


calc_nosc <- function(ftmsObj){
  

  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
    # pull e_meta out of ftmsObj #
    temp = ftmsObj$e_meta
    
    temp$NOSC = 4 - ((4*temp[,getCarbonColName(ftmsObj)] + temp[,getHydrogenColName(ftmsObj)] - 3*temp[,getNitrogenColName(ftmsObj)] - 2*temp[,getOxygenColName(ftmsObj)] + 5*temp[,getPhosphorusColName(ftmsObj)] - 2*temp[,getSulfurColName(ftmsObj)])/temp[,getCarbonColName(ftmsObj)])
    
    if(length(which(is.na(temp[,getMFColName(ftmsObj)]))) > 0){
      temp$NOSC[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
    }
    
    ftmsObj$e_meta = temp
    
    ftmsObj = setNOSCColName(ftmsObj, "NOSC")
    return(ftmsObj)
}