# @description \code{n_present} is a group summary function to count the number of samples in which a row is observed. 
# @details These functions are designed to be used with \code{\link{summarizeGroups}}.
# @param x columns of an \code{ftmsData} objects \code{e_data} component corresponding to a set of samples that 
# should be summarized
# @param data_scale data scale, the result of \code{\link{getDataScale}} function applied to the ftmsData object 
# from which x is extracted 
n_present <- function(x, data_scale) {
  if (data_scale %in% c('pres', 'abundance')) {
    if (identical(dim(x), NULL)) { # vector
      res <- as.integer(x>0)
    } else { # 2-dimensional
      res <- as.integer(rowSums(x>0))
    }
  } else {
    if (identical(dim(x), NULL)) { # vector
      res <- as.integer(!is.na(x))
    } else { # 2-dimensional
      res <- as.integer(rowSums(!is.na(x)))
    }
  }
  return(data.frame(n_present=res))
} 
attr(n_present, "function_name") <- "n_present"

# @description \code{prop_present} is a group summary function to count the proportion of samples in which a row is observed. 
prop_present <- function(x, data_scale) {
  counts <- n_present(x, data_scale)
  prop <- counts/ncol(x)
  colnames(prop) <- "prop_present"
  return(prop)  
}
attr(prop_present, "function_name") <- "prop_present"

#' @title Group summary functions
#' @description \code{getGroupSummaryFunctionNames} returns the names of valid group summary
#' functions that may be used with the \code{\link{summarizeGroups}} function.
#' @export
getGroupSummaryFunctionNames <- function() {
  return(c("n_present", "prop_present"))
}
