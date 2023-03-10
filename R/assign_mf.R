#' Assign Molecular Formulae Based on Element Counts
#' 
#' Construct molecular formulae based on element counts 
#' 
#' @param ftmsObj an object of class 'ftmsData', typically a result of \code{\link{as.peakData}}. e_meta must be present.
#' @param metacyc TRUE/FALSE, use MetaCyc style formulae? (FALSE by default)
#' 
#' @details Assigns molecular formulae for observed peaks, when possible. Formulae are assigned in a manner so they can be matched to databases (e.g. MetaCyc). If a Carbon 13 column is given, formulae are not assigned to peaks where C13 is present.
#' 
#' @return an object of class 'ftmsData' with a column in \code{e\_meta} giving the molecular formula.
#' 
#' @author Lisa Bramer
#'

assign_mf <- function(ftmsObj, metacyc=FALSE){
  
  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # check that ftmsObj doesn't already have cnames specified for mf in e_meta #
  if(!is.null(getMFColName(ftmsObj))) message("mf_cname was already specified and will be overwritten")
  
  # pull out columns of elements from e_meta and list of the element names present in e_meta
  x <- dplyr::select(ftmsObj$e_meta, all_of(as.character(attr(ftmsObj, "cnames")$element_col_names)))
  n <- names(x)
  
  # use helper function to map the element counts to construct a chemical formula
  formulas <- apply(x, MARGIN = 1, function(x) paste(purrr::map2(as.numeric(x), n, formula_writer_helper, metacyc=metacyc), collapse = ""))
  
  # blank formula --> NA
  formulas[which(formulas == "")] = NA
  
  # assign formulas to formula column in e_meta
  ftmsObj$e_meta$MolForm = formulas
  
  # assign mf_cname #
  res = setMFColName(ftmsObj, cname = "MolForm")

  return(res)
}

# Internal helper function that returns the subsection of a molecular formula for an element and its count within the peak
# Used in assign_mf where it's iterated over all elements and peaks
formula_writer_helper <- function(count, name, metacyc=FALSE){
  if (count > 1){
    return(paste0(name, count))
  } else if(count == 1 && metacyc==FALSE){
    return(name)
  } else if(count == 1 && metacyc==TRUE){
    return(paste0(name, count))
  } else {
    return("")
  }
}