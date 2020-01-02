#' Calculate Kendrick Mass and Defect
#' 
#' Calculates the Kendrick mass and Kendrick defect needed for Kendrick plots
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}. e_meta must be present.
#' @param base_compounds character vector, containing any of 'CH2', 'CO2', 'H2', 'H20', or 'CHO', the family of compounds to be used in determining the Kendrick Mass.
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

calc_kendrick <- function(ftmsObj, base_compounds = 'CH2'){

  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # check that ftmsObj doesn't already have cnames specified for ratios in e_meta #
  if(!is.null(getKendrickDefectColName(ftmsObj)) | !is.null(getKendrickMassColName(ftmsObj))) message("mass_cname and/or defect_cname were already specified and may be overwritten")
  
  # verify base input
  if(!all(base_compounds %in% c('CH2', 'CO2', 'H2', 'H2O', 'CHO'))) stop("Base compounds must be one of 'CH2', 'CO2', 'H2', 'H2O', or 'CHO'")
  
  mass_cname = getMassColName(ftmsObj)
  
  # check that all the cnames are character strings #
  if(class(mass_cname) != "character") stop("mass_cname must be a character string")
 
  # pull e_meta out of ftmsObj #
  temp = ftmsObj$e_meta
  
  # determine which ratio to use: CH2, CO2, H2, H20, CHO
  coefs = sapply(base_compounds, function(base){
                  switch(base,
                  'CH2' = 14/14.01565,
                  'CO2' = 44/43.98983,
                  'H2' = 2/2.015650,
                  'H2O' = 18/18.010565,
                  'CHO' = 29/29.00274
                  )}
                )
  
  # make new column names for e_meta
  mass_cols = sapply(base_compounds, function(base) paste0('kmass.', base))
  defect_cols = sapply(base_compounds, function(base) paste0('kdefect.', base))
  
  # calculate kendrick masses for each base...
  masses = as.numeric(as.character(temp[,mass_cname])) %*% t(coefs)
  # ... and store in new columns
  temp[mass_cols] = masses
  
  # calculate kendrick defect #
  temp[defect_cols] = ceiling(temp[mass_cols]) - temp[mass_cols]
  
  # reinsert temp into ftmsObj #
  ftmsObj$e_meta = temp
  
  # assign column names #
  res1 = setKendrickMassColName(ftmsObj, mass_cols)
  res2 = setKendrickDefectColName(res1, defect_cols)
  
  return(res2)
}