
#' Calculate Cox Gibbs Free Energy
#' 
#' Calculate Cox Gibbs Free Energy for peaks where empirical formula is available 
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}
#'
#' @details Cox Gibbs Free Energy  \eqn{= 60.3 - 28.5NOSC}{= 60.3 - 28.5*NOSC}, where NOSC \eqn{= -(\frac{4C + H - 3N - 2O + 5P - 2S}{C}) + 4}{= -((4*C + H - 3*N - 2*O + 5*P - 2*S)/(C)) + 4}
#' 
#' @return an object of the same class as \code{ftmsObj} with a column in \code{e_meta} giving Cox Gibbs Free Energy values
#' 
#' @references LaRowe and Van Cappellen, 2011, "Degradation of natural organic matter: A thermodynamic analysis". Geochimica et Cosmochimica Acta. 75.
#'  
#' @author Lisa Bramer
#'

calc_gibbs <- function(ftmsObj){
  

  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # pull e_meta out of ftmsObj #
  temp = ftmsObj$e_meta
  
  nosc = 4 - ((4*temp[,getCarbonColName(ftmsObj)] + temp[,getHydrogenColName(ftmsObj)] - 3*temp[,getNitrogenColName(ftmsObj)] - 2*temp[,getOxygenColName(ftmsObj)] + 5*temp[,getPhosphorusColName(ftmsObj)] - 2*temp[,getSulfurColName(ftmsObj)])/temp[,getCarbonColName(ftmsObj)])
  temp$GFE = 60.3 - 28.5 * nosc
  
  if(length(which(is.na(temp[,getMFColName(ftmsObj)]))) > 0){
    temp$GFE[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
  }
  
  ftmsObj$e_meta = temp
  
  ftmsObj = setGibbsColName(ftmsObj, "GFE")
  
  return(ftmsObj)
}