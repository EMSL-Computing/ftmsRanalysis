#' Molecule filter object
#'
#' This function returns a moleculeFilt object for use with \code{\link{applyFilt}}
#'
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}.
#'
#' @return Object of class 'moleculeFilt' that contains the molecule identifier and the number of samples for which the molecule was measured (not 0). This can be used in conjunction with \code{\link{applyFilt}} to remove peaks observed in fewer than \code{min_num} samples.
#'
#'
#' @author Lisa Bramer
#'
#' @export
#'
#' @examples
#' data("examplePeakData")
#' filter_obj <- molecule_filter(examplePeakData)
#' peakDataFiltered <- applyFilt(filter_obj, examplePeakData, min_num=2)
molecule_filter <- function(ftmsObj){

  ## some initial checks ##
  
  # check that ftmsObj is of appropriate class #
  if(!inherits(ftmsObj,"peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be of class 'peakData' or 'compoundData'")
  
  edata_id = getEDataColName(ftmsObj)
  sample_cols <- as.character(ftmsObj$f_data[, getFDataColName(ftmsObj)])

  # get row sums of nonmiss = number of times each feature is observed #
  num_obs <- n_present(ftmsObj$e_data[,sample_cols], data_scale = getDataScale(ftmsObj))

  # output #
  output <- data.frame(ftmsObj$e_data[, edata_id], num_obs)
  names(output) <- c(edata_id, "Num_Observations")
  
  orig_class <- class(output)
  class(output) <- c("moleculeFilt", orig_class)

  return(output)
  
}


