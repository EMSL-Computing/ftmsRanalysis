#' Wrapper function to calculate values associated with individual peaks/compounds
#' 
#' Calculate a variety of typical values for FT-MS data associated with each peak/compound where empirical formula is available
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}.
#' @param calc_fns a character string specifying which calculations to perform. Available options are: calc_aroma, calc_dbe, calc_gibbs, calc_kendrick, calc_nosc, and calc_vankrev.
#' @param calc_args a list with names corresponding to the available calc_fns.  Each element is a named sub-list of extra arguments for the specified function.  See examples for how to pass extra arguments.
#' 
#' @details The calculations are as follows for each of the `calc_fns`: 
#' 
#' \tabular{ll}{
#' calc_aroma \tab calculates aromaticity index (AI) and modified aromaticity index (AI_Mod) \cr
#'  \tab AI \eqn{= \frac{1 + C - O - S - 0.5(N + P + H)}{C - O - S - N - P}}{= (1 + C - O - S - 0.5*(N + P + H))/(C - O - S - N - P)} \cr 
#'  \tab AI_Mod \eqn{= \frac{1 + C - 0.5O - S - 0.5(N + P + H)}{C - 0.5*O - S - N - P}}{= (1 + C - 0.5*O - S - 0.5*(N + P + H))/(C - 0.5*O - S - N - P)} \cr
#' \tab \cr
#' calc_dbe \tab calculates double bond equivalent (DBE) and double bond equivalent minux Oxygent (DBE_O) \cr
#'  \tab DBE \eqn{= 1 + C - O - S - 0.5*(N + P + H)} \cr
#'  \tab DBE_0 \eqn{= 1 + C - O - S - 0.5(N + P + H) - O}{= 1 + C - O - S - 0.5*(N + P + H) - O} \cr
#' \tab \cr
#' calc_gibbs \tab calculates Cox Gibbs Free Energy (GFE) \cr
#'  \tab GFE = \eqn{= 60.3 - 28.5NOSC}{= 60.3 - 28.5*NOSC} \cr
#' \tab \cr
#' calc_kendrick \tab calculates Kendrick Mass and Kendrick Defect \cr
#'  \tab Kendrick-mass = \eqn{(Observed-Mass)*(Nominal-Mass(base)/Exact-Mass(base))} \cr
#'  \tab Kendrick-defect = ceiling(Kendrick-mass) - Kendrick-mass \cr
#' \tab \cr
#' calc_nosc \tab calculates nominal oxidation state of Carbon (NOSC) \cr
#'  \tab NOSC \eqn{= -(\frac{4C + H - 3N - 2O + 5P - 2S}{C}) + 4}{= -((4*C + H - 3*N - 2*O + 5*P - 2*S)/(C)) + 4} \cr
#'  \tab \cr
#' calc_element_ratios \tab calculates O:C, H:C, P:C, N:C, and N:P ratios \cr
#' \cr
#' }
#' 
#' @references Koch, B. P., & Dittmar, T. (2006). From mass to structure: an aromaticity index for high‐resolution mass data of natural organic matter. Rapid communications in mass spectrometry, 20(5), 926-932.
#' @references Errata: Koch, B. P., & Dittmar, T. (2016). From mass to structure: an aromaticity index for high-resolution mass data of natural organic matter. Rapid communications in mass spectrometery, 30(1), 250. DOI: 10.1002/rcm.7433
#' @references LaRowe and Van Cappellen, 2011, "Degradation of natural organic matter: A thermodynamic analysis". Geochimica et Cosmochimica Acta. 75.
#' @references Hughey, C. A., Hendrickson, C. L., Rodgers, R. P., Marshall, A. G., & Qian, K. (2001). Kendrick mass defect spectrum: a compact visual analysis for ultrahigh-resolution broadband mass spectra. Analytical Chemistry, 73(19), 4676-4681.
#' 
#' @return an object of the same class as \code{ftmsData} with columns in \code{e_meta} giving the newly calculated values
#'  
#' @author Kelly Stratton
#' 
#' @examples 
#' library(ftmsRanalysis)
#' 
#' # default arguments
#' peakdata_processed = compound_calcs(examplePeakData)
#' 
#' # only calculate aromaticity and kendrick mass/defect
#' peakdata_processed = compound_calcs(examplePeakData, calc_fns = c("calc_aroma", "calc_kendrick"))
#' 
#' # Specify extra arguments for calc_kendrick and calc_dbe.  
#' calc_args = list('calc_kendrick' = list('base_compounds' = c('CH2', 'CO2', 'H2')), 
#'                  'calc_dbe' = list('valences' = list('C'=5, 'H' = 4, 'S' = 5)))
#' 
#' peakdata_processed <- compound_calcs(examplePeakData, calc_args = calc_args)
#' 
#' @export

compound_calcs <- function(ftmsObj, calc_fns=c("calc_aroma", "calc_dbe", "calc_gibbs", "calc_kendrick", "calc_nosc", "calc_element_ratios"), calc_args = NULL){
  
  ## initial checks ##
  
  # check ftmsObj #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # checks for calc_fns #
  valid_fns <- c("calc_aroma", "calc_dbe", "calc_gibbs", "calc_kendrick", "calc_nosc", "calc_element_ratios")
  if(!inherits(calc_fns, "character")){stop("calc_fns must be a character vector")}
  if(!all(calc_fns %in% valid_fns)){stop("calc_fns must contain valid function names. See documentation for more information.")}
  if(length(calc_fns) < 1){stop("calc_fns must contain at least one valid function name")}
  
  # checks for calc_args #
  if(!is.null(calc_args) & is.null(names(calc_args))) stop("If specified, calc_args must be a named list with names matching any of the valid function names.  See documentation for valid function names.")
  if(!all(names(calc_args) %in% valid_fns)) stop("The names of calc_args must all be one of the valid function names.  See documentation for valid function names.")
  if(!all(names(calc_args) %in% calc_fns)){
    bad_names = names(calc_args)[which(!(names(calc_args) %in% calc_fns))]
    stop(sprintf("Extra arguments were specified for functions not named in calc_fns:  %s", paste(bad_names, collapse = ', ')))
  }

  ## end of initial checks ##

  for(i in 1:length(calc_fns)){
    # set f to the function that is named in the ith element of compound_calcs # 
    f <- get(as.character(calc_fns[i]), envir=asNamespace("ftmsRanalysis"), mode="function")
    
    # check for extra arguments
    if(!is.null(calc_args[[as.character(calc_fns[i])]])){
      ftmsObj <- do.call(f, c(list(ftmsObj = ftmsObj), calc_args[[as.character(calc_fns[i])]]))
    }
    else ftmsObj <- f(ftmsObj)
  }
  
  return(ftmsObj)
  
}