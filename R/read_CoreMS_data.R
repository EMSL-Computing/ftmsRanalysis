#' Read in CoreMS output
#'
#' Reads in CoreMS output file(s) in CSV format as a single `data.frame` with class `CoreMSrbind`
#'
#' @param list_of_files a vector of character strings specifying the data files
#'   (in .csv format) to be read in and concatenated
#' @param sample_names (optional) a vector of character strings denoting preferred sample names
#'
#' @details \code{read_CoreMS_data} reads in selected CSV files, appends
#'   'filename' column, verifies identical column names, and combines all files
#'   into a single `data.frame`/`CoreMSrbind` object
#'
#' @return CoreMSrbind object
#' 
#' @author Natalie Winans
#' 
#' @export

read_CoreMS_data <- function(list_of_files, sample_names = NULL) {
  
  # Check that list_of_files is a vector of strings
  if(!is.character(list_of_files)) stop("list_of_files must be of the class 'chr'")
  
  # Check that list_of_files is a vector of strings
  if(!is.null(sample_names)){
    if(!is.character(sample_names)) stop("list_of_files must be of the class 'chr'")
  }
  
  if(is.null(sample_names)){
  # Extract filename from path
    read_add_filename <- function(file_name) {
      suppressMessages(readr::read_csv(file_name, col_types = readr::cols())) %>%
        dplyr::mutate(Filename = tools::file_path_sans_ext(file_name))
    }
    file_list <- lapply(list_of_files, read_add_filename)
  } else {
    file_list <- list()
    for (i in 1:length(list_of_files)) {
      file_list[[i]] <- suppressMessages(readr::read_csv(list_of_files[i], col_types = readr::cols())) %>% 
        dplyr::mutate(Filename = sample_names[i])
    }
    
  }
  
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


