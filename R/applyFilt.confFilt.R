#' applyFilt function for confFilt
#'
#' @param filter_object 
#' @param cmsObj an object of the class 'CoreMSData', the output of \code{as.CoreMSData}
#' @param min_conf a numeric value greater than 0 specifying the minimum confidence score a peak should have in order to be retained
#'
#' @return an object of the class 'CoreMSData' that has been filtered by confidence score
#' 
#' @export
applyFilt.confFilt <- function(filter_object, cmsObj, min_conf = 0.5) {

  if ("confFilt" %in% names(attr(cmsObj, "filters"))) {
    prev_min_conf <- attr(cmsObj, "filters")$confFilt$threshold

    stop(paste0("A confidence filter has already been run on this dataset using a 'min_conf' of ", prev_min_conf))

  } else {    # no previous confFilt
    
    # check min_conf is numeric and of length 1
    if(!class(min_conf) %in% c("numeric", "integer") | min_conf < 0) stop("min_conf must be a number greater than zero")
    if(length(min_conf) != 1) stop("min_conf must be of length 1")
  
    orig_nrow <- nrow(cmsObj)
    
    mass_cname <- attr(cmsObj, "cnames")$mass_cname
    conf_cname <- attr(cmsObj, "cnames")$conf_cname

    filtered_cmsObj <- cmsObj %>% dplyr::filter(cmsObj[,conf_cname] >= min_conf)
    peaks_removed <- cmsObj %>% 
      dplyr::filter(cmsObj[,conf_cname] < min_conf | is.na(cmsObj[,conf_cname])) %>% 
      dplyr::select(mass_cname)
    
    if(nrow(filtered_cmsObj) < 1) stop("Filtering using specified minimum confidence results in no peaks left in the data.")

    num_rmv <- orig_nrow - nrow(filtered_cmsObj) 
    
    cmsObj <- filtered_cmsObj
    
    attr(cmsObj, "filters")$confFilt <- list(report_text = "", minimum = c(), removed = c())
    attr(cmsObj, "filters")$confFilt$report_text <- paste0("A confidence filter was applied to the data, removing peaks with a confidence score of less than ", min_conf, ". A total of ", num_rmv, " rows were removed by this filter.")
    attr(cmsObj, "filters")$confFilt$minimum <- min_conf
    attr(cmsObj, "filters")$confFilt$removed <- peaks_removed

    return(cmsObj)
  }
}