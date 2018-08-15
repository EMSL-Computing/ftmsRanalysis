#' Combine multiple distributed data objects of icrData
#' 
#' Combine multiple distributed data objects (ddo) into a single ddo
#' for further analysis or visualizations. The inputs to this function are
#' the ddo objects created by \code{\link{divideByGroup}} or 
#' \code{\link{divideByGroupComparison}}. The output is a new ddo
#' with all the data of the supplied inputs, in a form that can be 
#' summarized with \code{\link{summarizeGroups}} or \code{\link{summarizeGroupComparisons}}
#' respectively, or visualized with \code{trelliscope}.
#' 
#' @param ... two or more ddo objects that are the output of \code{\link{summarizeGroups}} or \code{\link{summarizeGroupComparisons}}
#' 
#' @seealso \link{ddo}
#' @author Amanda White
#' 
#' @export
#' 
#' @examples 
#' data("peakIcrProcessed")
#' peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Location")
#' grpDdo1 <- divideByGroup(peakIcrProcessed)
#' 
#' peakIcrProcessed <- group_designation(peakIcrProcessed, main_effects = "Crop.Flora")
#' grpDdo2 <- divideByGroup(peakIcrProcessed)
#' 
#' allGrpDdo <-concat(grpDdo1, grpDdo2)
#' grpSummaries <- summarizeGroups(allGrpDdo, c("n_present", "prop_present"))
concat <- function(...) {
  require(datadr)
  
  parms <- list(...)
  # test inputs
  if (length(parms) == 0) return(NULL)
  if (!all(unlist(lapply(parms, function(x) inherits(x, "ddo"))))) 
    stop("input values must be ddo objects")
  if (!all(unlist(lapply(parms, function(x) inherits(x[[1]]$value, "icrData") | inherits(x[[1]]$value, "groupComparison"))))) 
    stop("input values must be distributed data objects of icrData or groupComparison objects")
  
  if (length(parms) == 1) return(parms[[1]])
  
  # combine multiple DDOs
  tmp <- unlist(lapply(parms, as.list), recursive = FALSE)
  res <- ddo(tmp)

  return(res)
}

