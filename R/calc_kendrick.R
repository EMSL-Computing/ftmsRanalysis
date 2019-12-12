#' Calculate Kendrick Mass and Defect
#' 
#' Calculates the Kendrick mass and Kendrick defect needed for Kendrick plots
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}. e_meta must be present.
#' @param base character, one of 'CH2', 'CO2', 'H2', 'H20', or 'CHO', the family of compounds to be used in determining the Kendrick Mass.
#'
#' @return an object of the same class as \code{ftmsObj} with columns in \code{e_meta} giving Kendrick mass and defects
#' 
#' @details
#' \tabular{ll}{
#'  \tab Kendrick-mass = \eqn{(Observed-Mass)*(Nominal-Mass(base)/Exact-Mass(base))} \cr
#'  \tab Kendrick-defect = ceiling(Kendrick-mass) - Kendrick-mass
#' }
#' 
#' @references Hughey, C. A., Hendrickson, C. L., Rodgers, R. P., Marshall, A. G., & Qian, K. (2001). Kendrick mass defect spectrum: a compact visual analysis for ultrahigh-resolution broadband mass spectra. Analytical Chemistry, 73(19), 4676-4681.
#' 
#' @author Lisa Bramer
#'

calc_kendrick <- function(ftmsObj, base = 'CH2'){
  ### DA PLAN:  Calculate ratio = nominal mass/exact mass for each proposed molecule

  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # check that ftmsObj doesn't already have cnames specified for ratios in e_meta #
  if(!is.null(getKendrickDefectColName(ftmsObj)) | !is.null(getKendrickMassColName(ftmsObj))) message("mass_cname and/or defect_cname were already specified and will be overwritten")
  
  if(!(base %in% c('CH2', 'CO2', 'H2', 'H2O', 'CHO'))) stop("Base compound must be one of 'CH2', 'CO2', 'H2', 'H2O', or 'CHO'")
  
  mass_cname = getMassColName(ftmsObj)
  
  # check that all the cnames are character strings #
  if(class(mass_cname) != "character") stop("mass_cname must be a character string")
 
  # pull e_meta out of ftmsObj #
  temp = ftmsObj$e_meta
  
  # determine which ratio to use: CH2, CO2, H2, H20, CHO
  coef  = switch(base,
    'CH2' = 14/14.01565,
    'CO2' = 44/43.98983,
    'H2' = 2/2.015650,
    'H2O' = 18/18.010565,
    'CHO' = 29/29.00274
  )
  
  # calculate kendrick mass #
  temp$kmass = as.numeric(as.character(temp[,mass_cname]))*coef
  
  # calculate kendrick defect #
  temp$kdefect = ceiling(temp$kmass) - temp$kmass
  
  # reinsert temp into ftmsObj #
  ftmsObj$e_meta = temp
  
  # assign column names #
  res1 = setKendrickMassColName(ftmsObj, "kmass")
  res2 = setKendrickDefectColName(res1, "kdefect")
  
  return(res2)
}