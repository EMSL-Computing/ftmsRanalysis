#' Get database associated with an ftmsData object
#' 
#' Get the database associated with an object that has been mapped to
#' the compound or module level.
#' @param ftmsObj an object of type ftmsData
#' @return database name
#' @export
getDatabase <- function(ftmsObj) {
    if (!inherits(ftmsObj, "ftmsData")) {
        stop("ftmsObj must be of type ftmsData")
    } 
    return(attr(ftmsObj, "DB"))
}

# Internal only function to set database
setDatabase <- function(ftmsObj, db_name) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!is.character(db_name)) {
    stop("db_name must be of type character")
  }
  attr(ftmsObj, "DB") <- db_name
  invisible(ftmsObj)
}

#' Get instrument_type attribute from ftmsData object
#' 
#' Returns the instrument type which generated the data. Usually used for determining the
#' types of Van Krevelen, Kendrick, etc. plots that should be used
#' 
#' @param ftmsObj ftmsData object
#' @return character string indicating the instrument type
#' 
#' @export
#'
getInstrumentType <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be of type ftmsData")
  return(attr(ftmsObj, "data_info")$instrument_type)
}

# Internal only function to set instrument type
setInstrumentType <- function(ftmsObj, instrType){
  if (!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be of type ftmsData")
  if (!is.character(instrType)) stop("instrType must be of type character")
  
  if (is.null(attr(ftmsObj, "data_info"))){
    attr(ftmsObj, "data_info") <-  list(instrument_type=instrType)
  } else {
    attr(ftmsObj, "data_info")$instrument_type <- instrType
  }
  invisible(ftmsObj)
}


#' Get group_DF attribute from ftmsData object
#' 
#' Returns the group_DF attribute which gives groups of interest 
#' 
#' @param ftmsObj ftmsData object
#' @return character string indicating the instrument type
#' @export
getGroupDF <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be of type ftmsData")
  return(attr(ftmsObj, "group_DF"))
}

setGroupDF <- function(ftmsObj, group_df) {
  if (!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be of type ftmsData")
  if (!identical(group_df, NULL)) {
    if (!inherits(group_df, "data.frame")) stop("group_df must be of type data.frame")
  }
  attr(ftmsObj, "group_DF") <- group_df
  invisible(ftmsObj)
}

#' Set the valence_DF attribute
#' 
#' Stores a dataframe of various valence combinations in the valence_DF attribute of the ftmsObject 
#'
#' @param ftmsObj an object of type ftmsData
#' @param valences a dataframe with columns C, H, N, O, S, P and values representing valences for each element
#' @return updated ftmsObj
#' 
setDBEValenceDF <- function(ftmsObj, valences) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!inherits(valences, "data.frame")){
    stop("valence_DF must be a data frame")
  }
  if(!all(colnames(valences) %in% c('C', 'H', 'N', 'O', 'S', 'P'))){
    stop("valence_DF must have column names: 'C, H, N, O, S, P'")
  }
  if(!all(sapply(valences, is.numeric))){
    stop("valence_DF must have numeric columns")
  }
  
  attr(ftmsObj, "valence_DF") <- valences
  return(ftmsObj)
}

#' Get data scale 
#' 
#' Get the data scale (e.g. 'abundance', 'pres', 'log2', 'log10', 'log')
#'
#' @param ftmsObj ftmsData object
#' @return character string indicating the scale of the data
#' @export
getDataScale <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be of type ftmsData")
  res <- attr(ftmsObj, "data_info")$data_scale
  if (is.null(res)) res <- "abundance"
  return(res)
}


# Internal only function: set data scale parameter
setDataScale <- function(ftmsObj, data_scale) {
  if (!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be of type ftmsData")
  if (!is.character(data_scale)) stop("data_scale must be of type character")
  data_info <- attr(ftmsObj, "data_info")
  if (is.null(data_info)) data_info <- list()
  data_info$data_scale <- data_scale
  attr(ftmsObj, "data_info") <- data_info
  invisible(ftmsObj)
}
