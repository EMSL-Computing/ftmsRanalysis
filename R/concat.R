#' Combine multiple ftmsData objects
#' 
#' Combine multiple ftmsData objects into a single ftmsData object
#' for further analysis or visualizations. The inputs to this function are
#' the ftmsData objects created by \code{\link{divideByGroup}} or 
#' \code{\link{divideByGroupComparisons}}. The output is a new ftmsData
#' with all the data of the supplied inputs, in a form that can be 
#' summarized with \code{\link{summarizeGroups}} or \code{\link{summarizeGroupComparisons}}
#' respectively.
#' 
#' @param ... two or more ftmsData objects that are the output of \code{\link{summarizeGroups}} or \code{\link{summarizeGroupComparisons}}
#' 
#' @author Amanda White
#' 
#' @export
#' 
#' @examples 
#' data("exampleProcessedPeakData")
#' exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Location")
#' grp1 <- divideByGroup(exampleProcessedPeakData)
#' 
#' exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Crop.Flora")
#' grp2 <- divideByGroup(exampleProcessedPeakData)
#' 
#' allGrps <-concat(grp1, grp2)
#' grpSummaries <- summarizeGroups(allGrps, c("n_present", "prop_present"))
concat <- function(...) {
  parms <- list(...)
  # test inputs
  if (length(parms) == 0) return(NULL)
  if (!all(unlist(lapply(parms, function(x) inherits(x, "list"))))) 
    stop("input values must be list objects")
  if (!all(unlist(lapply(parms, function(x) inherits(x[[1]], "ftmsData") | inherits(x[[1]], "groupComparison"))))) 
    stop("input values must be lists of ftmsData or groupComparison objects")
  
  if (length(parms) == 1) return(parms[[1]])
  
  # combine multiple lists of ftmsData Objects
  res <- c(unlist(parms, recursive=FALSE))

  return(res)
}
