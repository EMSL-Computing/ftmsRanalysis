#' CoreMSData to ftmsData
#' 
#' Converts an object of the class `CoreMSData` into an `ftmsData` object consisting of the data frames `e_data`, `f_data`, and `e_meta`
#'
#' @param cmsObj a `CoreMSData` object, created using \code{\link{as.CoreMSData}}. Must not contain redundant m/z values or molecular formulas within a sample. This is accomplished using the function \code{\link{unique_mf_assignment}}.
#' 
#'@seealso \code{\link{conf_filter}} and \code{\link{applyFilt}}
#'
#' @return a list of three data frames, `e_data`, `f_data`, and `e_meta`
#' 
#' @author Natalie Winans
#' 
#' @export
CoreMSData_to_ftmsData <- function(cmsObj) {
  
  if(!inherits(cmsObj, "CoreMSData")) stop("cmsObj must be of the class 'CoreMSData'")
  
  obs_mass <- attr(cmsObj, "cnames")$obs_mass_cname
  calib_mass <- attr(cmsObj, "cnames")$calib_mass_cname
  calc_mass <- attr(cmsObj, "cnames")$calc_mass_cname
  peak_height <- attr(cmsObj, "cnames")$pheight_cname
  formula <- attr(cmsObj, "cnames")$mf_cname
  heteroatom <- attr(cmsObj, "cnames")$heteroatom_cname
  ion_type <- attr(cmsObj, "cnames")$iontype_cname
  filename <- attr(cmsObj, "cnames")$file_cname
  
  # check that there are no redundant masses or formulas within a sample
  dup_mass <- cmsObj$monoiso_data %>% 
    dplyr::group_by(.data[[filename]], .data[[obs_mass]]) %>% 
    dplyr::filter(dplyr::n() > 1)
  dup_form <- cmsObj$monoiso_data %>% 
    dplyr::group_by(.data[[filename]], .data[[formula]]) %>% 
    dplyr::filter(dplyr::n() > 1)
  
  if(nrow(dup_mass) > 0 | nrow(dup_form) > 0) stop("cmsObj contains either duplicate m/z values or duplicate molecular formulas within a sample. The function `unique_mf_assignment` must be used before converting `CoreMSData` object to `ftmsData` object.")
  
  # create e_data
  e_data <- cmsObj$monoiso_data %>% 
    dplyr::ungroup() %>% 
    dplyr::select(.data[[obs_mass]], .data[[formula]], 
                  .data[[peak_height]], .data[[filename]]) %>% 
    dplyr::group_by(.data[[formula]]) %>% 
    dplyr::mutate(Mass = mean(.data[[obs_mass]])) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(!.data[[obs_mass]], !.data[[formula]]) %>% 
    tidyr::pivot_wider(., id_cols = Mass,
                names_from = .data[[filename]],
                values_from = .data[[peak_height]],
                values_fill = 0) %>% 
    dplyr::arrange(Mass)
  
  # create f_data
  f_data <- cmsObj$monoiso_data %>% 
    dplyr::ungroup() %>% 
    dplyr::select(.data[[filename]]) %>% 
    dplyr::distinct() %>% 
    dplyr::rename(SampleID = .data[[filename]])
  
  # create e_meta
  possible_elements <- c("C", "H", "O", "N", "S", "P")
  
  forms <- cmsObj$monoiso_data %>% 
    dplyr::ungroup() %>% 
    dplyr::pull(.data[[formula]]) 
  
  # get indices of elements that occur in dataset
  these_elements <- which(colSums(do.call(rbind, purrr::map(forms, 
                                                            stringr::str_detect, 
                                                            pattern = possible_elements)), 
                                  na.rm = TRUE) > 0)
  
  elem_cnames <- possible_elements[these_elements]
  
  e_meta <- cmsObj$monoiso_data %>% 
    dplyr::ungroup() %>% 
    dplyr::select(.data[[obs_mass]], .data[[formula]], .data[[calib_mass]], 
                  .data[[calc_mass]], .data[[heteroatom]], .data[[ion_type]]) %>% 
    dplyr::group_by(.data[[formula]]) %>%
    dplyr::mutate(Mass = mean(.data[[obs_mass]]),
                  `Calibrated m/z` = mean(.data[[calib_mass]])) %>% 
    dplyr::select(Mass, .data[[formula]], `Calibrated m/z`, .data[[calc_mass]], 
                  .data[[heteroatom]], .data[[ion_type]]) %>% 
    dplyr::distinct() %>% 
    tidyr::separate(col = .data[[formula]], into = elem_cnames, sep = " ") %>% 
    dplyr::mutate(C = if (exists("C", where = .)) as.integer(stringr::str_remove(C, "C")) else NULL,
                  H = if (exists("H", where = .)) as.integer(stringr::str_remove(H, "H")) else NULL,
                  O = if (exists("O", where = .)) as.integer(stringr::str_remove(O, "O")) else NULL,
                  N = if (exists("N", where = .)) as.integer(stringr::str_remove(N, "N")) else NULL,
                  S = if (exists("S", where = .)) as.integer(stringr::str_remove(S, "S")) else NULL,
                  P = if (exists("P", where = .)) as.integer(stringr::str_remove(P, "P")) else NULL) %>% 
    dplyr::arrange(Mass)

  ftmsObj <- list("e_data"= e_data, "f_data" = f_data, "e_meta" = e_meta)
  
  return(ftmsObj)
}