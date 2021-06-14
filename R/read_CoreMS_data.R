#' Read in CoreMS output
#'
#' Reads in CoreMS output file(s) as single dataframe with class 'CoreMSrbind'
#'
#' @param list_of_files a vector of character strings specifying the data files
#'   (in .csv format) to be read in and concatenated
#' @param force_rbind logical, if TRUE any columns not present in all files will
#'   be omitted prior to concatenation. Defaults to FALSE.
#'
#'
#' @details \code{read_CoreMS_data} reads in selected .csv files, appends
#'   'filename' column, verifies identical column names, and combines all files
#'   into a single data.frame/CoreMSrbind object
#'
#' @return data.frame/CoreMSrbind object
#' @export

read_CoreMS_data <- function(list_of_files, force_rbind = FALSE) {

  # Check that list_of_files is a vector of strings
  if(!is.character(list_of_files)) stop("list_of_files must be of the class 'chr'")

  read_add_filename <- function(file_name) {
    readr::read_csv(file_name) %>%
      dplyr::mutate(Filename = file_name)
  }

  file_list <- lapply(list_of_files, read_add_filename)

  # Check that columns are consistent across files
  df_cnames <- list()
  for (i in 1:length(file_list)) {
    df_cnames[[i]] <- colnames(file_list[[i]])
  }
  col_nums <- table(unlist(df_cnames))
  exclude_cols <- names(which(col_nums < length(file_list)))

  if (force_rbind) {
    all_data <- dplyr::bind_rows(file_list) %>%  dplyr::select(-exclude_cols)
  } else if (length(exclude_cols) > 0) {
      stop("one or more columns not present in all files")
  } else {
    all_data <- dplyr::bind_rows(file_list)
  }

  if ("X1" %in% names(all_data)) {
    all_data <- all_data %>% dplyr::select(-X1)
  }

  class(all_data) <- append(class(all_data), "CoreMSrbind")

  return(all_data)
}


#' Convert Data to CoreMSData Class
#'
#' Converts 'CoreMSrbind' object to 'CoreMSData' object
#'
#' @param all_data a data.frame/CoreMSData object, should be the output of
#'   \code{read_CoreMS_data}
#' @param mass_cname a character string specifying the name of the column
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
                          mass_cname = "Calculated m/z",
                          pheight_cname = "Peak Height",
                          error_cname = "Mass Error (ppm)",
                          conf_cname = "Confidence Score",
                          file_cname = "Filename",
                          mf_cname = NULL, c13_cname = NULL,
                          s34_cname = NULL, o18_cname = NULL,
                          n15_cname = NULL){

  # check that all_data is of class 'CoreMSrbind'
  if(!inherits(all_data, "CoreMSrbind")) stop("all_data must be of the class 'CoreMSrbind'")

  # check that required columns exist in all_data
  if(!(mass_cname %in% names(all_data))) stop(paste("Calculated mass column", mass_cname,"not found in all_data"))
  if(!(pheight_cname %in% names(all_data))) stop(paste("Peak height/intensity column", pheight_cname,"not found in all_data"))
  if(!(error_cname %in% names(all_data))) stop(paste("Mass error column", error_cname,"not found in all_data"))
  if(!(conf_cname %in% names(all_data))) stop(paste("Confidence score column", conf_cname,"not found in all_data"))
  if(!(file_cname %in% names(all_data))) stop(paste("Filename column", file_cname,"not found in all_data"))

  # check that cname arguments are found in all_data
if(!is.null(mf_cname)){
    if(!(mf_cname %in% names(all_data))) stop(paste("Molecular Formula column", mf_cname, "not found in all_data"))
  }
  if(!is.null(c13_cname)){
    if(!(c13_cname %in% names(all_data))) stop(paste("Carbon column", c13_cname, "not found in all_data"))
  }
if(!is.null(o18_cname)){
    if(!(o18_cname %in% names(all_data))) stop(paste("Oxygen column", o18_cname, "not found in all_data"))
  }
  if(!is.null(n15_cname)){
    if(!(n15_cname %in% names(all_data))) stop(paste("Nitrogen column", n15_cname, "not found in all_data"))
  }
  if(!is.null(s34_cname)){
    if(!(s34_cname %in% names(all_data))) stop(paste("Sulfur column", s34_cname, "not found in all_data"))
  }

  attr(all_data, "cnames") <- list(mass_cname = mass_cname, pheight_cname = pheight_cname,
                                   error_cname = error_cname, conf_cname = conf_cname,
                                   file_cname = file_cname, mf_cname = mf_cname,
                                   c13_cname = c13_cname, o18_cname = o18_cname,
                                   s34_cname = s34_cname, n15_cname = n15_cname)


  class(all_data) <- c("CoreMSData", "data.frame")

  return(all_data)
}
