#' Unique Molecular Formula Assignment
#'
#' Takes an object of class 'CoreMSData' and assigns unique molecular formulas
#' to each peak within each sample
#'
#' @param cmsObj \code{CoreMSData} object, output of functions
#'   \code{\link{read_CoreMS_data}} and \code{\link{as.CoreMSData}}
#' @param method Must be either "confidence", "peak_intensity", or "prevalence".
#'   Selects formula corresponding to highest confidence score, peak height, or
#'   prevalence across samples, respectively.
#'
#' @return \code{CoreMSData} object
#'
#' @author Natalie Winans
#'
#' @export
unique_mf_assignment <- function(cmsObj, method) {
  
  # check inputs
  if(!inherits(cmsObj, "CoreMSData")) stop("cmsObj must be of the class 'CoreMSData'")
  if(!(method %in% c("confidence", "peak_intensity", "prevalence"))) 
    stop("method must be 'confidence', 'peak_intensity', or 'prevalence'")
 
  # get column names from cmsObj
  index <- attr(cmsObj, "cnames")$index_cname
  obs_mass <- attr(cmsObj, "cnames")$obs_mass_cname
  calc_mass <- attr(cmsObj, "cnames")$calc_mass_cname
  peak_height <- attr(cmsObj, "cnames")$pheight_cname
  conf_score <- attr(cmsObj, "cnames")$conf_cname
  mono_index <- attr(cmsObj, "cnames")$monoiso_index_cname
  formula <- attr(cmsObj, "cnames")$mf_cname
  filename <- attr(cmsObj, "cnames")$file_cname
  
  if (method == "confidence") {
    # get maximum of confidence score ties
    tied_max_confs <- cmsObj$monoiso_data %>%  
      dplyr::group_by(.data[[filename]], .data[[obs_mass]]) %>% 
      dplyr::slice(which(.data[[conf_score]] == max(.data[[conf_score]]))) %>% 
      dplyr::filter(dplyr::n() > 1) 
     
    if (nrow(tied_max_confs) > 0) {
      max_tied_conf <- max(tied_max_confs[,conf_score])
      stop(paste0("Data set contains tied confidence scores. Apply a confidence filter at a threshold of at least ", 
                  round(max_tied_conf + 0.01, 2), ".")) # add .01 to ensure all ties are removed by filter
      
    } else { # if no ties - likely a conf filter has already been applied
      new_monoiso_data <- cmsObj$monoiso_data %>%
        dplyr::group_by(.data[[filename]], .data[[obs_mass]]) %>%
        dplyr::slice(which.max(.data[[conf_score]])) %>% 
        dplyr::group_by(.data[[filename]], .data[[formula]]) %>% 
        dplyr::slice(which.max(.data[[conf_score]])) 
      
      # get unique identifier with index & sample of removed mono peaks
      mono_peaks_removed <- suppressMessages(dplyr::anti_join(cmsObj$monoiso_data, new_monoiso_data)) %>% 
        dplyr::mutate(index_id = paste0(.data[[index]], "_", .data[[filename]])) %>% 
        dplyr::pull(index_id) %>% 
        unique()
      
      # remove associated isotopic peaks
      new_iso_data <- cmsObj$iso_data %>% 
        dplyr::mutate(monoindex_id = paste0(.data[[mono_index]], "_", .data[[filename]])) %>% 
        dplyr::filter(!(monoindex_id %in% mono_peaks_removed))
      
      cmsObj$monoiso_data <- new_monoiso_data
      cmsObj$iso_data <- new_iso_data
      
      return(cmsObj)
    }
    
  } else if (method == "peak_intensity") {
    
    tied_max_heights <- cmsObj$monoiso_data %>%  
      dplyr::group_by(.data[[filename]], .data[[obs_mass]]) %>% 
      dplyr::slice(which(.data[[peak_height]] == max(.data[[peak_height]]))) %>% 
      dplyr::filter(dplyr::n() > 1)
    
    if (nrow(tied_max_heights) > 0) {
      
      conf_threshold <- tied_max_heights %>% 
        dplyr::slice(which(.data[[conf_score]] ==  min(.data[[conf_score]]))) %>% 
        dplyr::pull(.data[[conf_score]]) %>% 
        max()
      
      stop("Data set contains tied maximum peak heights. Apply a confidence filter at a threshold of at least ", 
           round(conf_threshold + 0.01, 2), ".")
      
    } else { # if no ties - likely a conf filter has already been applied

      new_monoiso_data <- cmsObj$monoiso_data %>% 
        dplyr::filter(!(is.na(.data[[formula]]))) %>% 
        dplyr::group_by(.data[[filename]], .data[[obs_mass]]) %>% 
        dplyr::slice(which(.data[[peak_height]] == max(.data[[peak_height]]))) %>% 
        dplyr::group_by(.data[[filename]], .data[[formula]]) %>% 
        dplyr::slice(which(.data[[peak_height]] == max(.data[[peak_height]])))
      
      # get unique identifier with index & sample of removed mono peaks
      mono_peaks_removed <- suppressMessages(dplyr::anti_join(cmsObj$monoiso_data, new_monoiso_data)) %>% 
        dplyr::mutate(index_id = paste0(.data[[index]], "_", .data[[filename]])) %>% 
        dplyr::pull(index_id) %>% 
        unique()
      
      # remove associated isotopic peaks
      new_iso_data <- cmsObj$iso_data %>% 
        dplyr::mutate(monoindex_id = paste0(.data[[mono_index]], "_", .data[[filename]])) %>% 
        dplyr::filter(!(monoindex_id %in% mono_peaks_removed))
      
      cmsObj$monoiso_data <- new_monoiso_data
      cmsObj$iso_data <- new_iso_data
      return(cmsObj)
    }
    
  } else if (method == "prevalence") {
    
    # extract sets of mfs for each file/sample
    mf_sets <- cmsObj$monoiso_data %>%
      dplyr::select(formula, filename) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data[[filename]]) %>%
      dplyr::summarize(set_of_mfs = paste(sort(.data[[formula]]), collapse = ", ")) %>%
      tibble::column_to_rownames(filename) %>%
      purrr::transpose()

    mf_sets <- purrr::map(mf_sets, unlist) 
    
    # select formula present in the most files/samples
    unq_prev <- cmsObj$monoiso_data %>% 
      dplyr::group_by(.data[[filename]], .data[[obs_mass]]) %>%
      dplyr::slice(which.max(sum(unlist(purrr::map(mf_sets, 
                                                   stringr::str_detect, 
                                                   pattern = .data[[formula]]))))) 
  
    # pull out formulas that occur more than once within a file/sample
    tied_most_prev <- unq_prev %>% 
      dplyr::group_by(.data[[filename]], .data[[formula]]) %>% 
      dplyr::filter(dplyr::n() > 1)
    
    if (nrow(tied_most_prev) > 0) {
      
      conf_threshold <- tied_most_prev %>% 
        dplyr::group_by(.data[[filename]], .data[[formula]]) %>% 
        dplyr::slice(which(.data[[conf_score]] ==  min(.data[[conf_score]]))) %>% 
        dplyr::pull(.data[[conf_score]]) %>% 
        max()
      
      stop("Data set contains tied most prevalent formulas. Apply a confidence filter at a threshold of at least ", 
           round(conf_threshold + 0.01, 2), ".")
      
    } else { # if no ties - likely a conf filter has already been applied
      
      new_monoiso_data <- unq_prev
      
      # get unique identifier with index & sample of removed mono peaks
      mono_peaks_removed <- suppressMessages(dplyr::anti_join(cmsObj$monoiso_data, new_monoiso_data)) %>% 
        dplyr::mutate(index_id = paste0(.data[[index]], "_", .data[[filename]])) %>% 
        dplyr::pull(index_id) %>% 
        unique()
      
      # remove associated isotopic peaks
      new_iso_data <- cmsObj$iso_data %>% 
        dplyr::mutate(monoindex_id = paste0(.data[[mono_index]], "_", .data[[filename]])) %>% 
        dplyr::filter(!(monoindex_id %in% mono_peaks_removed))
      
      cmsObj$monoiso_data <- new_monoiso_data
      cmsObj$iso_data <- new_iso_data
      
      return(cmsObj)
    }
  }
}