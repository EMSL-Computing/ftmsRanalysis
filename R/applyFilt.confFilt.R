#' applyFilt function for confFilt
#'
#' @param filter_object 
#' @param cmsObj 
#' @param min_conf 
#'
#' @return
#' @export
#'
#' @examples
#' 
applyFilt.confFilt <- function(filter_object, cmsObj, min_conf = 0.5) {
  
  if ("confFilt" %in% names(attr(cmsObj, "filters"))) {
    prev_min_conf <- attr(cmsObj, "filters")$confFilt$threshold
    
    stop(paste0("A confidence filter has already been run on this dataset using a 'min_conf' of ", prev_min_conf))
  
  } else {    # no previous confFilt
    
    # check min_conf is numeric and of length 1
    if(!class(min_conf) %in% c("numeric", "integer") | min_conf < 0) stop("min_conf must be a number greater than zero")
    if(length(min_conf) != 1) stop("min_conf must be of length 1")
    
    mass_cname <- attr(cmsObj, "cnames")$mass_cname
    conf_cname <- attr(cmsObj, "cnames")$conf_cname
    
    conf_info <- dplyr::pull(filter_object, conf_cname)
    
    indices <- which(conf_info >= min_conf)
    
    if(length(indices) < 1) stop("Filtering using specified minimum confidence results in no peaks left in the data.")
    
    ids <- filter_object[indices, mass_cname]
    
    
  }
}