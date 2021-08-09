#' Read in CoreMS output
#'
#' Reads in CoreMS output file(s) as single dataframe with class 'CoreMSrbind'
#'
#' @param list_of_files a vector of character strings specifying the data files
#'   (in .csv format) to be read in and concatenated
#'
#'
#' @details \code{read_CoreMS_data} reads in selected .csv files, appends
#'   'filename' column, verifies identical column names, and combines all files
#'   into a single data.frame/CoreMSrbind object
#'
#' @return data.frame/CoreMSrbind object
#' @export

read_CoreMS_data <- function(list_of_files) {
  
  # Check that list_of_files is a vector of strings
  if(!is.character(list_of_files)) stop("list_of_files must be of the class 'chr'")
  
  # Extract filename from path
  read_add_filename <- function(file_name) {
    suppressMessages(readr::read_csv(file_name, col_types = readr::cols())) %>%
      dplyr::mutate(Filename = tail(stringr::str_split(file_name, "/")[[1]], 1))
  }
  
  file_list <- lapply(list_of_files, read_add_filename)
  
  # Check that columns are consistent across files
  # df_cnames <- list()
  # for (i in 1:length(file_list)) {
  #   df_cnames[[i]] <- colnames(file_list[[i]])
  # }
  # col_nums <- table(unlist(df_cnames))
  # exclude_cols <- names(which(col_nums < length(file_list)))
  # 
  # if (force_rbind) {
  #   all_data <- dplyr::bind_rows(file_list) %>%  dplyr::select(-exclude_cols)
  # } else if (length(exclude_cols) > 0) {
  #   stop("one or more columns not present in all files")
  # } else {
  #   all_data <- dplyr::bind_rows(file_list)
  # }
  
  all_data <- dplyr::bind_rows(file_list)
  
  if ("X1" %in% names(all_data)) {
    all_data <- all_data %>% dplyr::select(-X1)
  } 
  if ("...1" %in% names(all_data)) {
    all_data <- all_data %>% dplyr::select(-`...1`)
  }
  
  class(all_data) <- append("CoreMSrbind", class(all_data))
  
  return(all_data)
}


#' Convert Data to CoreMSData Object
#'
#' Converts 'CoreMSrbind' object to 'CoreMSData' object
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
#' @details
#'
#' @export

as.CoreMSData <- function(all_data,
                          obs_mass_cname = "m/z",
                          calc_mass_cname = "Calculated m/z",
                          pheight_cname = "Peak Height",
                          error_cname = "Mass Error (ppm)",
                          conf_cname = "Confidence Score",
                          file_cname = "Filename",
                          mf_cname = "Molecular Formula",
                          c13_cname = "13C",
                          s34_cname = "34S",
                          o18_cname = "18O",
                          n15_cname = "15N"){
  
  # check that all_data is of class 'CoreMSrbind'
  if(!inherits(all_data, "CoreMSrbind")) stop("all_data must be of the class 'CoreMSrbind'")
  
  # check that required columns exist in all_data
  if(!(obs_mass_cname %in% names(all_data))) stop(paste("Observed mass column", obs_mass_cname, "not found in all_data"))
  if(!(calc_mass_cname %in% names(all_data))) stop(paste("Calculated mass column", calc_mass_cname,"not found in all_data"))
  if(!(pheight_cname %in% names(all_data))) stop(paste("Peak height/intensity column", pheight_cname,"not found in all_data"))
  if(!(error_cname %in% names(all_data))) stop(paste("Mass error column", error_cname,"not found in all_data"))
  if(!(conf_cname %in% names(all_data))) stop(paste("Confidence score column", conf_cname,"not found in all_data"))
  if(!(file_cname %in% names(all_data))) stop(paste("Filename column", file_cname,"not found in all_data"))
  if(!(mf_cname %in% names(all_data))) stop(paste("Molecular Formula column", mf_cname, "not found in all_data"))
  
  # check that optional cname arguments are found in all_data
  # if(!is.null(c13_cname)){
  #   if(!(c13_cname %in% names(all_data))) stop(paste("Carbon column", c13_cname, "not found in all_data"))
  # }
  # if(!is.null(o18_cname)){
  #   if(!(o18_cname %in% names(all_data))) stop(paste("Oxygen column", o18_cname, "not found in all_data"))
  # }
  # if(!is.null(n15_cname)){
  #   if(!(n15_cname %in% names(all_data))) stop(paste("Nitrogen column", n15_cname, "not found in all_data"))
  # }
  # if(!is.null(s34_cname)){
  #   if(!(s34_cname %in% names(all_data))) stop(paste("Sulfur column", s34_cname, "not found in all_data"))
  # }
  
  iso_cols <- c("13C", "15N", "18O", "34S")[which(c("13C", "15N", "18O", "34S") %in% names(all_data))]

  # split all_data into monoisotopic and isotopic dataframes
  iso_data <- all_data %>% dplyr::filter_at(dplyr::vars(iso_cols), dplyr::any_vars(. > 0))
  monoiso_data <- suppressMessages(dplyr::anti_join(all_data, iso_data))

  # output list of both dataframes
  res <- list("monoiso_data" = monoiso_data, "iso_data" = iso_data)
  
  attr(res, "cnames") <- list(obs_mass_cname = obs_mass_cname, calc_mass_cname = calc_mass_cname, 
                                   pheight_cname = pheight_cname, error_cname = error_cname, 
                                   conf_cname = conf_cname, file_cname = file_cname, mf_cname = mf_cname,
                                   c13_cname = c13_cname, o18_cname = o18_cname, s34_cname = s34_cname, 
                                   n15_cname = n15_cname)
  
  class(res) <- c("list", "CoreMSData")
  
  return(res)
}
