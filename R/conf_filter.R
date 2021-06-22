#' Create conf\_filter object
#' 
#' @param cmsObj an object of class 'CoreMSData', should be output of the function 
#' \code{as.CoreMSData}
#'
#' @details To be used with \code{\link{applyFilt.confFilt}} to subset peaks with a minimum confidence score of \code{min_conf}
#'
#' @return object of class 'confFilt' 
#' 
#' @export

conf_filter <- function(cmsObj) {
  
  if(!inherits(cmsObj, "CoreMSData")) stop("cmsObj must be of class 'CoreMSData")
  
  conf_id <- attr(cmsObj, "cnames")$conf_cname
  mass_id  <- attr(cmsObj, "cnames")$mass_cname
  error_id <- attr(cmsObj, "cnames")$error_cname
  file_id <- attr(cmsObj, "cnames")$file_cname
  formula_id <- attr(cmsObj, "cnames")$mf_cname
  
  output <- cmsObj %>% dplyr::select(mass_id, conf_id, error_id, formula_id, file_id)
  
  class(output) <- c("confFilt", "data.frame")
  
  return(output)
  
}