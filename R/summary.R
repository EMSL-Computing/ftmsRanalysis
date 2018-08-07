#' Summary of icrData object
#'
#' @param icrData object of type icrData
#'
#' @return list object containing summary statistics
#' @export
summary.icrData <- function(icrData) {
  
  res <- list()
  samp_names <- as.character(unique(icrData$f_data[, getFDataColName(icrData)]))
  res$Samples <- length(samp_names)
  res$Molecules <- nrow(icrData$e_data)
  data_vals <- as.matrix(icrData$e_data[, samp_names])
  if (getDataScale(icrData) == "abundance" | getDataScale(icrData) == "pres") {
    nmissing <- sum(data_vals == 0 | is.na(data_vals))
  } else {
    nmissing <- sum(is.na(data_vals))
  }
  res$Percent_Missing <- nmissing/prod(dim(data_vals))*100
  
  class(res) <- "icrDataSummary"
  return(res)
}

#' @export
print.icrDataSummary <- function(obj) {
  cat(sprintf("Samples: %d\nMolecules: %d\nPercent Missing: %.3f%%\n", obj$Samples, obj$Molecules, obj$Percent_Missing))
}