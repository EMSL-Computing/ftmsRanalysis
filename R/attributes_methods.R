#' Get database associated with object
#' 
#' Get the database associated with an object that has been mapped to
#' the compound or module level.
#' @param icrDataObj an object of type icrData
#' @return database (KEGG or MetaCyc)
#' @export
get_db <- function(icrDataObj) {
    if (!inherits(icrDataObj, "icrData")) {
        stop("icrDataObj must be of type icrData")
    } 
    return(attr(icrDataObj, "DB"))
}

#' Get instrument_type attribute from icrData object
#' 
#' Returns the instrument type which generated the data. Usually used for determining the
#' types of Van Krevelen, Kendrick, etc. plots that should be used
#' 
#' @param icrDataObj icrData object
#' @return character string indicating the instrument type
#' 
#' @export
#'
getInstrumentType <- function(icrDataObj){
  if (!inherits(icrDataObj, "icrData")) stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "data_info")$instrument_type)
}

#' Get group_DF attribute from icrData object
#' 
#' Returns the group_DF attribute which gives groups of interest 
#' 
#' @param icrDataObj icrData object
#' @return character string indicating the instrument type
#' 
getGroupDF <- function(icrDataObj){
  if (!inherits(icrDataObj, "icrData")) stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "group_DF"))
}
