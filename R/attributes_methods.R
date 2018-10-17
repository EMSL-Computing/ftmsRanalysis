#' Get database associated with object
#' 
#' Get the database associated with an object that has been mapped to
#' the compound or module level.
#' @param icrData an object of type icrData
#' @return database (KEGG or MetaCyc)
#' @export
getDatabase <- function(icrData) {
    if (!inherits(icrData, "icrData")) {
        stop("icrData must be of type icrData")
    } 
    return(attr(icrData, "DB"))
}

# Internal only function to set database
setDatabase <- function(icrData, db_name) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!is.character(db_name)) {
    stop("db_name must be of type character")
  }
  attr(icrData, "DB") <- db_name
  invisible(icrData)
}

#' Get instrument_type attribute from icrData object
#' 
#' Returns the instrument type which generated the data. Usually used for determining the
#' types of Van Krevelen, Kendrick, etc. plots that should be used
#' 
#' @param icrData icrData object
#' @return character string indicating the instrument type
#' 
#' @export
#'
getInstrumentType <- function(icrData){
  if (!inherits(icrData, "icrData")) stop("icrData must be of type icrData")
  return(attr(icrData, "data_info")$instrument_type)
}

# Internal only function to set instrument type
setInstrumentType <- function(icrData, instrType){
  if (!inherits(icrData, "icrData")) stop("icrData must be of type icrData")
  if (!is.character(instrType)) stop("instrType must be of type character")
  
  if (is.null(attr(icrData, "data_info"))){
    attr(icrData, "data_info") <-  list(instrument_type=instrType)
  } else {
    attr(icrData, "data_info")$instrument_type <- instrType
  }
  invisible(icrData)
}


#' Get group_DF attribute from icrData object
#' 
#' Returns the group_DF attribute which gives groups of interest 
#' 
#' @param icrData icrData object
#' @return character string indicating the instrument type
#' @export
getGroupDF <- function(icrData){
  if (!inherits(icrData, "icrData")) stop("icrData must be of type icrData")
  return(attr(icrData, "group_DF"))
}

setGroupDF <- function(icrData, group_df) {
  if (!inherits(icrData, "icrData")) stop("icrData must be of type icrData")
  if (!identical(group_df, NULL)) {
    if (!inherits(group_df, "data.frame")) stop("group_df must be of type data.frame")
  }
  attr(icrData, "group_DF") <- group_df
  invisible(icrData)
}

#' Get data scale 
#' 
#' Get the data scale (e.g. 'abundance', 'pres', 'log2', 'log10', 'log')
#'
#' @param icrData icrData object
#' @return character string indicating the scale of the data
#' @export
getDataScale <- function(icrData) {
  if (!inherits(icrData, "icrData")) stop("icrData must be of type icrData")
  res <- attr(icrData, "data_info")$data_scale
  if (is.null(res)) res <- "abundance"
  return(res)
}


# Internal only function: set data scale parameter
setDataScale <- function(icrData, data_scale) {
  if (!inherits(icrData, "icrData")) stop("icrData must be of type icrData")
  if (!is.character(data_scale)) stop("data_scale must be of type character")
  data_info <- attr(icrData, "data_info")
  if (is.null(data_info)) data_info <- list()
  data_info$data_scale <- data_scale
  attr(icrData, "data_info") <- data_info
  invisible(icrData)
}
