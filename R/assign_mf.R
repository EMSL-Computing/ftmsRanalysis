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
  
  # pull column name attributes and set them #
  c_cname = getCarbonColName(ftmsObj)
  o_cname = getOxygenColName(ftmsObj)
  h_cname = getHydrogenColName(ftmsObj)
  n_cname = getNitrogenColName(ftmsObj)
  s_cname = getSulfurColName(ftmsObj)
  p_cname = getPhosphorusColName(ftmsObj)
  
  x = ftmsObj$e_meta  
  
  temp = paste("C", x[,c_cname], "H", x[,h_cname], "O", x[,o_cname], "N", x[,n_cname], "S", x[,s_cname], "P", x[,p_cname], sep = "")
      
      # look for Carbon 
      temp = gsub(pattern = "C1([^0-9])", replacement = "C\\1", temp)
      temp = gsub(pattern = "P1$", replacement = "P", temp)
      
      # get rid of 1's #     
      if (!metacyc) {
        temp = gsub("(?<=[[:alpha:]])1(?=[[:alpha:]])", "", temp, perl=TRUE)
      }
      
      # get rid of zeros #
      temp = gsub(pattern = "[[:alpha:]]0", replacement = "", temp)
    
      temp[which(temp == "")] = NA
      
      ftmsObj$e_meta$MolForm = temp
  
  # assign mf_cname #
  res = setMFColName(ftmsObj, cname = "MolForm")

  return(res)
}


