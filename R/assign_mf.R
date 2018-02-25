#' Assign Molecular Formulae Based on Element Counts
#' 
#' Construct molecular formulae based on element counts 
#' 
#' @param icrData an object of class 'icrData', typically a result of \code{\link{as.icrData}}. e_meta must be present.
#' 
#' @details Assigns molecular formulae for observed peaks, when possible. Formulae are assigned in a manner so they can be matched to databases (e.g. KEGG). If a Carbon 13 column is given, formulae are not assigned to peaks where C13 is present.
#' 
#' @return an object of class 'icrData' with a column in \code{e\_meta} giving the molecular formula.
#' 
#' @author Lisa Bramer
#'

assign_mf <- function(icrData){
  
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  # check that icrData doesn't already have cnames specified for mf in e_meta #
  if(!is.null(getMFColName(icrData))) message("mf_cname was already specified and will be overwritten")
  
  # pull column name attributes and set them #
  c_cname = getCarbonColName(icrData)
  o_cname = getOxygenColName(icrData)
  h_cname = getHydrogenColName(icrData)
  n_cname = getNitrogenColName(icrData)
  s_cname = getSulfurColName(icrData)
  p_cname = getPhosphorusColName(icrData)
  
  x = icrData$e_meta  
  
  temp = paste("C", x[,c_cname], "H", x[,h_cname], "N", x[,n_cname], "O", x[,o_cname], "S", x[,s_cname], "P", x[,p_cname], sep = "")
      
      # look for Carbon 
      temp = gsub(pattern = "C1([^0-9])", replacement = "C\\1", temp)
      temp = gsub(pattern = "P1$", replacement = "P", temp)
      
      # get rid of 1's #     
      temp = gsub("(?<=[[:alpha:]])1(?=[[:alpha:]])", "", temp, perl=TRUE)
      
      # get rid of zeros #
      temp = gsub(pattern = "[[:alpha:]]0", replacement = "", temp)
    
      temp[which(temp == "")] = NA
      
      icrData$e_meta$MolForm = temp
  
  # assign mf_cname #
  res = setMFColName(icrData, cname = "MolForm")

  return(res)
}


