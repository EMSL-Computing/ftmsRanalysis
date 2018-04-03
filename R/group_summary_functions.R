# @description \code{n_present} is a group summary function to count the number of samples in which a row is observed. 
# @details These functions are designed to be used with \code{\link{summarizeGroups}}.
# @param x columns of an \code{icrData} objects \code{e_data} component corresponding to a set of samples that 
# should be summarized
# @param data_scale data scale, the result of \code{\link{getDataScale}} function applied to the icrData object 
# from which x is extracted 
n_present <- function(x, data_scale) {
  if (data_scale %in% c('pres', 'abundance')) 
    return(rowSums(x>0))
  else 
    return(rowSums(!is.na(x)))
} 
attr(n_present, "default_column_name") <- "n_present"

# @description \code{prop_present} is a group summary function to count the proportion of samples in which a row is observed. 
prop_present <- function(x, data_scale) {
  if (data_scale %in% c('pres', 'abundance')) 
    return(rowSums(x>0)/ncol(x))
  else 
    return(rowSums(!is.na(x))/ncol(x))
}
attr(prop_present, "default_column_name") <- "prop_present"

#' @title Group summary functions
#' @description \code{getGroupSummaryFunctionNames} returns the names of valid group summary
#' functions that may be used with the \code{\link{summarizeGroups}} function.
#' @export
getGroupSummaryFunctionNames <- function() {
  return(c("n_present", "prop_present"))
}
