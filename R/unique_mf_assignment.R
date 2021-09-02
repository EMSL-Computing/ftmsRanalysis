#' Unique Molecular Formula Assignment
#' 
#' @param cmsObj \code{CoreMSData} object, output of functions \link{\code{read_CoreMS_data}} and \link{\code{as.CoreMSData}}
#' @param method Must be either "max_conf", "max_height", or "most_prev". Selects formula corresponding to highest confidence score, peak height, or prevalence across samples, respectively.
#'
#' @return \code{CoreMSData} object
#' @export
#'
#' @author Natalie Winans
#' 
#' @examples
unique_mf_assingment <- function(cmsObj, method) {
  
  # check inputs
  if(!inherits(cmsObj, "CoreMSData")) stop("cmsObj must be of the class 'CoreMSData'")
  if(!(method %in% c("max_conf", "max_height", "most_prev"))) stop("method must be 'max_conf', 'max_height', or 'most_prev'")
 
  # get column names from cmsObj
  obs_mass <- attr(cmsObj, "cnames")$obs_mass_cname
  calc_mass <- attr(cmsObj, "cnames")$calc_mass_cname
  peak_height <- attr(cmsObj, "cnames")$pheight_cname
  conf_score <- attr(cmsObj, "cnames")$conf_cname
  formula <- attr(cmsObj, "cnames")$mf_cname
  filename <- attr(cmsObj, "cnames")$file_cname
  
  if (method == "max_conf") {
    # get maximum of confidence score ties
    tied_max_confs <- cmsObj %>%  
      dplyr::select(obs_mass, calc_mass, peak_height, conf_score, formula, filename) %>%
      dplyr::group_by(.data[[obs_mass]]) %>% 
      dplyr::slice(which(.data[[conf_score]] == max(.data[[conf_score]]))) %>% 
      dplyr::group_by(.data[[obs_mass]]) %>%
      dplyr::filter(n() > 1) 
     
    if (nrow(tied_max_confs) > 0) {
      max_tied_conf <- max(tied_max_confs[,conf_score])
      stop(paste0("Data set contains tied confidence scores. Apply a confidence filter at a threshold of at least ", 
                  max_tied_conf + 0.01, ".")) # add .01 to ensure all ties are removed by filter
    } else { # if no ties - likely a conf filter has already been applied
      cmsObj <- cmsObj %>%
        dplyr::group_by(.data[[obs_mass]]) %>%
        dplyr::slice(which.max(.data[[conf_score]]))
      return(cmsObj)
    }
  } else if (method == "max_height") {
    
    tied_max_heights <- cmsObj %>%  
      dplyr::select(obs_mass, calc_mass, peak_height, conf_score, formula, filename) %>%
      dplyr::group_by(.data[[filename]], .data[[formula]]) %>% 
      dplyr::slice(which(.data[[peak_height]] == max(.data[[peak_height]]))) %>% 
      dplyr::filter(n() > 1)
    
    if (nrow(tied_max_heights) > 0) {
      stop("Data set contains tied maximum peak heights")
      
      #could do this
      # cmsObj <- cmsObj %>%
      #   dplyr::group_by(.data[[filename]], .data[[formula]]) %>% 
      #   dplyr::slice(which(.data[[peak_height]] == max(.data[[peak_height]]))) %>% 
      #   dplyr::slice(which(.data[[conf_score]] == max(.data[[conf_score]])))
      # return(cmsObj)
      
    } else {
      cmsObj <- cmsObj %>%
        dplyr::group_by(.data[[filename]], .data[[formula]]) %>% 
        dplyr::slice(which(.data[[peak_height]] == max(.data[[peak_height]])))
      return(cmsObj)
    }
    
  } else if (method == "most_prev") {
    
  }
  

  
}