
#' Calculate Cox Gibbs Free Energy
#' 
#' Calculate Cox Gibbs Free Energy for peaks where empirical formula is available 
#' 
#' @param icrData an object of class 'peakData' or 'compoundIcrData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}
#'
#' @details Cox Gibbs Free Energy  \eqn{= 60.3 - 28.5NOSC}{= 60.3 - 28.5*NOSC}, where NOSC \eqn{= -(\frac{4C + H - 3N - 2O + 5P - 2S}{C}) + 4$}{= -((4*C + H - 3*N - 2*O + 5*P - 2*S)/(C)) + 4}
#' 
#' @return an object of the same class as \code{icrData} with a column in \code{e_meta} giving Cox Gibbs Free Energy values
#' 
#' @references LaRowe and Van Cappellen, 2011, "Degradation of natural organic matter: A thermodynamic analysis". Geochimica et Cosmochimica Acta. 75.
#'  
#' @author Lisa Bramer
#'

calc_gibbs <- function(icrData){
  

  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakData' or 'compoundIcrData'")
  
  # pull e_meta out of icrData #
  temp = icrData$e_meta
  
  nosc = 4 - ((4*temp[,getCarbonColName(icrData)] + temp[,getHydrogenColName(icrData)] - 3*temp[,getNitrogenColName(icrData)] - 2*temp[,getOxygenColName(icrData)] + 5*temp[,getPhosphorusColName(icrData)] - 2*temp[,getSulfurColName(icrData)])/temp[,getCarbonColName(icrData)])
  temp$GFE = 60.3 - 28.5 * nosc
  
  if(length(which(is.na(temp[,getMFColName(icrData)]))) > 0){
    temp$GFE[which(is.na(temp[,getMFColName(icrData)]))] = NA
  }
  
  icrData$e_meta = temp
  
  icrData = setGibbsColName(icrData, "GFE")
  
  return(icrData)
}