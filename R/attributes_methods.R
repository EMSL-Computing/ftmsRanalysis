#' Get database associated with object
#' 
#' Get the database associated with an object that has been mapped to
#' the compound or module level.
#' @param icrData an object of type icrData
#' @return database (KEGG or MetaCyc)
#' @export
get_db <- function(icrData) {
    if (!inherits(icrData, "icrData")) {
        stop("icrData must be of type icrData")
    } 
    return(attr(icrData, "DB"))
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

#' Get group_DF attribute from icrData object
#' 
#' Returns the group_DF attribute which gives groups of interest 
#' 
#' @param icrData icrData object
#' @return character string indicating the instrument type
#' 
getGroupDF <- function(icrData){
  if (!inherits(icrData, "icrData")) stop("icrData must be of type icrData")
  return(attr(icrData, "group_DF"))
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
