#' Molecule filter object
#'
#' This function returns a moleculeFilt object for use with \code{\link{applyFilt}}
#'
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#'
#' @return Object of class 'moleculeFilt' that contains the molecule identifier and the number of samples for which the molecule was measured (not 0). This can be used in conjunction with \code{\link{applyFilt}} to remove peaks observed in fewer than \code{min_num} samples.
#'
#'
#' @author Lisa Bramer
#'
#' @export
#'
#' @examples
#' data("peakIcrData")
#' filter_obj <- molecule_filter(peakIcrData)
#' peakIcrDataFiltered <- applyFilt(filter_obj, min_num=2)
molecule_filter <- function(icrData){

  ## some initial checks ##
  
  # check that icrData is of appropriate class #
  if(!inherits(icrData,"peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be of class 'peakIcrData' or 'compoundIcrData'")
  
  edata_id = getEDataColName(icrData)

  # get row sums of nonmiss = number of times each feature is observed #
  num_obs <- n_present(icrData$e_data[,-edata_id], data_scale = getDataScale(icrData))

  # output #
  output <- data.frame(icrData$e_data[, edata_id], num_obs)
  names(output) <- c(edata_id, "Num_Observations")
  
  orig_class <- class(output)
  class(output) <- c("moleculeFilt", orig_class)

  return(output)
  
}


