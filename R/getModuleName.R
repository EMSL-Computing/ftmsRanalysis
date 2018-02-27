#' Get module name
#' 
#' Get the module name(s) for one or more module IDs.
#'
#' @param module_ids a character vector of module IDs
#' @param db database to match, one of 'KEGG' or 'MetaCyc'
#'
#' @return character vector of module names
#' @export
getModuleName <- function(module_ids, db="KEGG") {
  if (missing(module_ids)) stop("module_ids must be specified")
  if (!is.character(module_ids)) stop("module_ids must be a character vector")
  
  if (toupper(db) == "KEGG") {
    ind <- match(module_ids, KEGGdata::kegg_modules$MODULE)
    return(KEGGdata::kegg_modules[ind, "NAME"])
  } else if (toupper(db) == "METACYC") {
    ind <- match(module_ids, MetaCycData::mc_modules$MODULE)
    return(MetaCycData::mc_modules[ind, "COMMON-NAME"])
  } else {
    stop("Unknown value for db, must be 'KEGG' or 'MetaCyc'")
  }
}