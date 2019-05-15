#' Get module name
#' 
#' Get the module name(s) for one or more module IDs.
#' This requires the MetaCycData package (\url{http://github.com/EMSL-Computing/MetaCycData}).
#'
#' @param module_ids a character vector of module IDs
#' @param db database to match, currently 'MetaCyc' is the only allowed value
#'
#' @return character vector of module names
#' @export
getModuleName <- function(module_ids, db="MetaCyc") {
  if (missing(module_ids)) stop("module_ids must be specified")
  if (!is.character(module_ids)) stop("module_ids must be a character vector")
  
  if (toupper(db) == "METACYC") {
    ind <- match(module_ids, MetaCycData::mc_modules$MODULE)
    return(MetaCycData::mc_modules[ind, "COMMON-NAME"])
  } else {
    stop("Unknown value for db, must be 'MetaCyc'")
  }
}