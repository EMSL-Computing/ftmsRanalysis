#' CoreMSData to ftmsData
#'
#' @param cms_data a `CoreMSData` object, created using \link{\code{as.CoreMSData}}
#'
#' @return e_data and e_meta
#' @export
#'
#' @examples
CoreMSData_to_ftmsData <- function(cmsObj) {
  
  if(!inherits(cmsObj, "CoreMSData")) stop("cmsObj must be of the class 'CoreMSData'")
  
  obs_mass <- attr(cmsObj, "cnames")$obs_mass_cname
  peak_height <- attr(cmsObj, "cnames")$pheight_cname
  formula <- attr(cmsObj, "cnames")$mf_cname
  filename <- attr(cmsObj, "cnames")$file_cname
  
  # create e_data
  e_data <- cmsObj$monoiso_data %>% 
    dplyr::mutate(Mass = .data[[obs_mass]], .keep = "unused") %>% 
    tidyr::pivot_wider(id_cols = Mass, 
                names_from = .data[[filename]], 
                values_from = .data[[peak_height]], 
                values_fill = 0) %>% 
    dplyr::arrange(Mass)
  
  
  # create e_meta
  possible_elements <- c("C", "H", "O", "N", "S", "P")
  
  forms<- cmsObj$monoiso_data %>% 
    dplyr::ungroup() %>% 
    dplyr::pull(.data[[formula]]) 
  
  # get indices of elements that occur in dataset
  these_elements <- which(colSums(do.call(rbind, purrr::map(forms, stringr::str_detect, pattern = possible_elements)), 
                                  na.rm = TRUE) > 0)
  
  elem_cnames <- possible_elements[these_elements]
  
  e_meta <- cmsObj$monoiso_data %>% 
    dplyr::ungroup() %>% 
    dplyr::select(.data[[obs_mass]], .data[[formula]]) %>% 
    tidyr::separate(col = .data[[formula]], into = elem_cnames, sep = " ") %>% 
    dplyr::mutate(Mass = .data[[obs_mass]], .keep = "unused") %>% 
    dplyr::select(Mass, everything()) %>% 
    dplyr::mutate(C = if (exists("C", where = .)) as.integer(stringr::str_remove(C, "C")) else NULL,
                  H = if (exists("H", where = .)) as.integer(stringr::str_remove(H, "H")) else NULL,
                  O = if (exists("O", where = .)) as.integer(stringr::str_remove(O, "O")) else NULL,
                  N = if (exists("N", where = .)) as.integer(stringr::str_remove(N, "N")) else NULL,
                  S = if (exists("S", where = .)) as.integer(stringr::str_remove(S, "S")) else NULL,
                  P = if (exists("P", where = .)) as.integer(stringr::str_remove(P, "P")) else NULL) %>% 
    dplyr::arrange(Mass)
  
  
  ftmsObj <- list("e_data"= e_data, "e_meta" = e_meta)
  
  return(ftmsObj)
}