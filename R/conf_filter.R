#' Create conf\_filter object
#' 
#' @param cmsObj an object of class 'CoreMSData', should be output of the function 
#' \code{\link{as.CoreMSData}}
#'
#' @details To be used with \code{\link{applyFilt.confFilt}} to subset peaks with a minimum confidence score of \code{min_conf}
#'
#' @return object of class 'confFilt' 
#' 
#' @author Natalie Winans
#' 
#' @export

conf_filter <- function(cmsObj) {
  
  if(!inherits(cmsObj, "CoreMSData")) stop("cmsObj must be of class 'CoreMSData")
  
  conf_id <- attr(cmsObj, "cnames")$conf_cname
  obs_mass_id  <- attr(cmsObj, "cnames")$obs_mass_cname
  error_id <- attr(cmsObj, "cnames")$error_cname
  file_id <- attr(cmsObj, "cnames")$file_cname
  formula_id <- attr(cmsObj, "cnames")$mf_cname
  
  moutput <- cmsObj$monoiso_data %>% dplyr::select(obs_mass_id, conf_id, error_id, formula_id, file_id)
  ioutput <- cmsObj$iso_data %>% dplyr::select(obs_mass_id, conf_id, error_id, formula_id, file_id)
  
  output <- list("monoiso_data" = moutput, "iso_data" = ioutput)
  
  class(output) <- c("confFilt", "list")
  
  attr(output, "conf_cname") <- conf_id
  
  return(output)
  
}