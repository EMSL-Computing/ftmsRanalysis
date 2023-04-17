#' Confidence Filter Data Table
#' 
#' Creates a data table for confidence filtering in Shiny app
#'
#' @param cmsObj CoreMSData object created using \code{\link{as.CoreMSData}}
#' @param min_conf numeric value between 0 and 1 denoting minimum confidence threshold
#'
#' @return `data.frame` containg counts of remaining and removed monoisotopic peaks in each file/sample, based on selected minimum confidence threshold
#'
conf_filter_dt <- function(cmsObj, min_conf) {
  
  conf_score <- attr(cmsObj, "cnames")$conf_cname
  filename <- attr(cmsObj, "cnames")$file_cname
  
  orig_peaks <- cmsObj$monoiso_data %>% 
    dplyr::group_by(.data[[filename]]) %>% 
    dplyr::count() %>% 
    dplyr::rename(orig_n_peaks = n, Sample = .data[[filename]])

  dt <- cmsObj$monoiso_data %>% 
    dplyr::filter(.data[[conf_score]] >= min_conf) %>% 
    dplyr::group_by(.data[[filename]]) %>% 
    dplyr::count() %>%
    dplyr::rename(`Peaks Remaining` = n, Sample = .data[[filename]]) 

  cf_dt <- suppressMessages(dplyr::left_join(orig_peaks, dt)) %>%
    dplyr::mutate(`Peaks Remaining (%)` = round((`Peaks Remaining`/orig_n_peaks)*100, 1),
                  `Peaks Removed` = orig_n_peaks - `Peaks Remaining`,
                  `Peaks Removed (%)` = round((orig_n_peaks - `Peaks Remaining`)/orig_n_peaks*100, 1)) %>%
    dplyr::select(!orig_n_peaks)

  return(cf_dt)
}