#' Create CoreMSData Object
#'
#' Converts `CoreMSrbind` object to `CoreMSData` object
#'
#' @param all_data a data.frame/CoreMSData object, should be the output of
#'   \code{read_CoreMS_data}
#' @param obs_mass_cname a character string specifying th name of the column 
#'   containing the observed mass to charge ratio (m/z) in \code{all_data}
#' @param calc_mass_cname a character string specifying the name of the column
#'   containing the calculated mass to charge ratio (m/z) in \code{all_data}
#' @param pheight_cname a character string specifying the name of the column
#'   containing the peak height in \code{all_data}
#' @param error_cname a character string specifying the name of the column
#'   containing the mass error (ppm) in \code{all_data}
#' @param conf_cname a character string specifying the name of the column
#'   containing the confidence score in \code{all_data}
#' @param file_cname a character string specifying the name of the column
#'   containing the filenames in \code{all_data}
#' @param mf_cname a character string specifying the name of the column
#'   containing the molecular formula in \code{all_data}
#' @param c13_cname a character string specifying the name of the column
#'   containing the C13 logical indicator in \code{all_data}
#' @param s34_cname a character string specifying the name of the column
#'   containing the S43 logical indicator in \code{all_data}
#' @param o18_cname a character string specifying the name of the column
#'   containing the O18 logical indicator in \code{all_data}
#' @param n15_cname a character string specifying the name of the column
#'   containing the N15 logical indicator in \code{all_data}
#'
#' @return `CoreMSData` object, consisting of a list of two data frames
#' 
#' @author Natalie Winans
#'
#' @export

as.CoreMSData <- function(all_data,
                          index_cname = "Index",
                          obs_mass_cname = "m/z",
                          calib_mass_cname = "Calibrated m/z",
                          calc_mass_cname = "Calculated m/z",
                          pheight_cname = "Peak Height",
                          error_cname = "Mass Error (ppm)",
                          conf_cname = "Confidence Score",
                          heteroatom_cname = "Heteroatom Class",
                          iontype_cname = "Ion Type",
                          file_cname = "Filename",
                          monoiso_index_cname = "Mono Isotopic Index",
                          mf_cname = "Molecular Formula",
                          c13_cname = NULL,
                          s34_cname = NULL,
                          o18_cname = NULL,
                          n15_cname = NULL){
  
  # check that all_data is of class 'CoreMSrbind'
  if(!inherits(all_data, "CoreMSrbind")) stop("all_data must be of the class 'CoreMSrbind'")
  
  # check that specified columns exist in all_data
  if(!(index_cname %in% names(all_data))) stop(paste("Index column", index_cname, "not found in all_data"))
  if(!(obs_mass_cname %in% names(all_data))) stop(paste("Observed mass column", obs_mass_cname, "not found in all_data"))
  if(!(calc_mass_cname %in% names(all_data))) stop(paste("Calculated mass column", calc_mass_cname,"not found in all_data"))
  if(!(calib_mass_cname %in% names(all_data))) stop(paste("Calibrated mass column", calib_mass_cname,"not found in all_data"))
  if(!(pheight_cname %in% names(all_data))) stop(paste("Peak height/intensity column", pheight_cname,"not found in all_data"))
  if(!(error_cname %in% names(all_data))) stop(paste("Mass error column", error_cname,"not found in all_data"))
  if(!(conf_cname %in% names(all_data))) stop(paste("Confidence score column", conf_cname,"not found in all_data"))
  if(!(heteroatom_cname %in% names(all_data))) stop(paste("Heteroatom class column", heteroatom_cname,"not found in all_data"))
  if(!(iontype_cname %in% names(all_data))) stop(paste("Ion type column", iontype_cname,"not found in all_data"))
  if(!(file_cname %in% names(all_data))) stop(paste("Filename column", file_cname,"not found in all_data"))
  if(!(monoiso_index_cname %in% names(all_data))) stop(paste("Monoisotopic index column", mf_cname, "not found in all_data"))
  if(!(mf_cname %in% names(all_data))) stop(paste("Molecular Formula column", mf_cname, "not found in all_data"))
  
  # isotopic columns may or may not be present in all_data
  if(!(is.null(c13_cname))) {
    if(!(c13_cname %in% names(all_data))) stop(paste("Carbon column", c13_cname, "not found in all_data"))
  }  
  if(!(is.null(o18_cname))) {
    if(!(o18_cname %in% names(all_data))) stop(paste("Oxygen column", o18_cname, "not found in all_data"))
  }    
  if(!(is.null(n15_cname))) {
    if(!(n15_cname %in% names(all_data))) stop(paste("Nitrogen column", n15_cname, "not found in all_data"))
  }      
  if(!(is.null(s34_cname))) {
    if(!(s34_cname %in% names(all_data))) stop(paste("Sulfur column", s34_cname, "not found in all_data"))
  }
  
  iso_cols <- c(c13_cname, o18_cname, n15_cname, s34_cname)
  
  # check that isotopic cnames specified if columns present
  if(any(c("13C", "18O", "15N", "34S") %in% colnames(all_data)) & is.null(iso_cols)) stop("Isotopic columns present in all_data. Provide column names for all isotopic columns.") 
  
  # split all_data into monoisotopic and isotopic dataframes
  if (!is.null(iso_cols)) {
    iso_data <- all_data %>% dplyr::filter_at(dplyr::vars(all_of(iso_cols)), dplyr::any_vars(. > 0))
    monoiso_data <- suppressMessages(dplyr::anti_join(all_data, iso_data))
  } else {
    iso_data <- data.frame(matrix(ncol = ncol(all_data), nrow = 0))
    colnames(iso_data) <- colnames(all_data)
    monoiso_data <- all_data
  }
  
  # check that all isotopic peaks are indicated by isotopic columns and correct if not
  iso_mf_peaks <- monoiso_data %>%
    dplyr::filter(stringr::str_detect(.data[[mf_cname]], "13C|18O|15N|34S"))
  
  if (nrow(iso_mf_peaks) > 0) {
    iso_data <- rbind(iso_data, iso_mf_peaks)
    monoiso_data <- suppressMessages(dplyr::anti_join(monoiso_data, iso_mf_peaks))
  }
  
  # output list of both dataframes
  res <- list("monoiso_data" = monoiso_data, "iso_data" = iso_data)
  
  attr(res, "cnames") <- list(index_cname = index_cname, obs_mass_cname = obs_mass_cname, 
                              calib_mass_cname = calib_mass_cname, calc_mass_cname = calc_mass_cname, 
                              pheight_cname = pheight_cname, error_cname = error_cname, 
                              conf_cname = conf_cname, heteroatom_cname = heteroatom_cname, 
                              iontype_cname = iontype_cname, monoiso_index_cname = monoiso_index_cname, 
                              mf_cname = mf_cname, file_cname = file_cname, c13_cname = c13_cname, 
                              o18_cname = o18_cname, s34_cname = s34_cname, n15_cname = n15_cname)
  
  class(res) <- c("list", "CoreMSData")
  
  return(res)
}
