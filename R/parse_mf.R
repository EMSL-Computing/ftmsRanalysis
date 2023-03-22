#' Parse Molecular Formulae to Obtain Elemental Counts
#' 
#' Construct elemental count columns based on provided molecular formulae
#' 
#' @param ftmsObj an object of class 'ftmsData', typically a result of \code{\link{as.peakData}}. 
#' 
#' @details Parses molecular formulae for number of observed elements.
#' 
#' @return an object of class 'ftmsData' with a column in \code{e\_meta} giving the molecular formula.
#' 
#' @author Lisa Bramer
#'

parse_mf <- function(ftmsObj){

  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # check that ftmsObj doesn't already have cnames specified for elemental counts in e_meta #
  if(all(!sapply(attr(ftmsObj, "cnames")$element_col_names, is.null))) stop("Element column names were already specified and will be overwritten")
  
  # check that mf_cname is non-null #
  if(is.null(getMFColName(ftmsObj))) stop("mf_cname is NULL and must be specified")
  
  # pull molecular formulas as a vector from e_meta
  mf_cname = getMFColName(ftmsObj)
  mf_vals = as.vector(ftmsObj$e_meta[,mf_cname])
  
  # create vector of all element names to iterate through
  elements <- unique(sapply(element_names, gsub, pattern="\\d", replacement=""))
  
  # apply helper function which creates an element count vector for each element and then cbinds them together
  counts <- lapply(elements, atom_count_vectorizer, mf_vals) %>%
    as.data.frame(col.names = elements)
  
  # remove columns that are all 0 (element was not present in any formulas)
  counts <- counts %>%
    select(where(~ any(. != 0)))
  
  # column bind the element counts to e_meta
  ftmsObj$e_meta <- cbind(ftmsObj$e_meta, counts)
  
  # set the element col name attribute for each element
  for (element in names(counts)) {
    ftmsObj <- setElementColName(ftmsObj, element, element)
  }

  return(ftmsObj)
  
}

# Internal helper function that creates a vector of atom counts for a given element on a list of formulas
# Used in lapply in the parse_mf function
atom_count_vectorizer <- function(element, formulas){
  atoms <- stringr::str_extract(formulas, pattern = paste0(element, "(?![a-z])(\\d{1,})?"))
  atoms_counts <- ifelse(atoms == element, 1, atoms)
  atoms_counts <- ifelse(!is.na(atoms_counts), as.integer(gsub("\\D", "", atoms_counts, perl = TRUE)), 0)
}
