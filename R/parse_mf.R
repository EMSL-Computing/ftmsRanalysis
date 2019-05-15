#' Parse Molecular Formulae to Obtain Elemental Counts
#' 
#' Construct elemental count columns based on provided molecular formulae
#' 
#' @param ftmsObj an object of class 'ftmsData', typically a result of \code{\link{as.peakData}}. 
#' 
#' @details Parses molecular formulae for number of observed C, H, O, N, S, and P.
#' 
#' @return an object of class 'ftmsData' with a column in \code{e\_meta} giving the molecular formula.
#' 
#' @author Lisa Bramer
#'

parse_mf <- function(ftmsObj){
  

  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # check that ftmsObj doesn't already have cnames specified for elemental counts in e_meta #
  if(!is.null(getCarbonColName(ftmsObj)) & !is.null(getHydrogenColName(ftmsObj)) & !is.null(getOxygenColName(ftmsObj)) & !is.null(getNitrogenColName(ftmsObj)) & !is.null(getSulfurColName(ftmsObj)) &!is.null(getPhosphorusColName(ftmsObj))) stop("c_cname, h_cname, n_cname, o_cname, s_cname, and p_cname were already specified and will be overwritten")
  
  # check that mf_cname is non-null #
  if(is.null(getMFColName(ftmsObj))) stop("mf_cname is NULL and must be specified")
  
  mf_cname = getMFColName(ftmsObj)

  mf_vals = as.vector(ftmsObj$e_meta[,mf_cname])
  
  # pull out carbon portion of string #
  c_num = gsub(".*(C[0-9]{0,3}).*", "\\1", mf_vals)
  c_num = gsub("^C$", "1", c_num)
  c_num = gsub("^C([0-9]{0,3})$", "\\1", c_num)  
  c_num[setdiff(1:length(c_num), grep("C", mf_vals))] = "0"
  c_num = as.numeric(c_num)

  # pull out hydrogen portion of string #
  h_num = gsub(".*(H[0-9]{0,3}).*", "\\1", mf_vals)
  h_num = gsub("^H$", "1", h_num)
  h_num = gsub("^H([0-9]{0,3})$", "\\1", h_num)  
  h_num[setdiff(1:length(h_num), grep("H", mf_vals))] = "0"
  h_num = as.numeric(h_num)

  # pull out carbon portion of string #
  o_num = gsub(".*(O[0-9]{0,3}).*", "\\1", mf_vals)
  o_num = gsub("^O$", "1", o_num)
  o_num = gsub("^O([0-9]{0,3})$", "\\1", o_num)  
  o_num[setdiff(1:length(o_num), grep("O", mf_vals))] = "0"
  o_num = as.numeric(o_num)

  # pull out carbon portion of string #
  n_num = gsub(".*(N[0-9]{0,3}).*", "\\1", mf_vals)
  n_num = gsub("^N$", "1", n_num)
  n_num = gsub("^N([0-9]{0,3})$", "\\1", n_num)  
  n_num[setdiff(1:length(n_num), grep("N", mf_vals))] = "0"
  n_num = as.numeric(n_num)

  # pull out carbon portion of string #
  s_num = gsub(".*(S[0-9]{0,3}).*", "\\1", mf_vals)
  s_num = gsub("^S$", "1", s_num)
  s_num = gsub("^S([0-9]{0,3})$", "\\1", s_num)  
  s_num[setdiff(1:length(s_num), grep("S", mf_vals))] = "0"
  s_num = as.numeric(s_num)
  
  # pull out phosphorus portion of string #
  p_num = gsub(".*(P[0-9]{0,3}).*", "\\1", mf_vals)
  p_num = gsub("^P$", "1", p_num)
  p_num = gsub("^P([0-9]{0,3})$", "\\1", p_num)  
  p_num[setdiff(1:length(p_num), grep("P", mf_vals))] = "0"
  p_num = as.numeric(p_num)
  
  temp = ftmsObj$e_meta
  
  temp$C = c_num
  temp$H = h_num
  temp$O = o_num
  temp$N = n_num
  temp$S = s_num
  temp$P = p_num
  
  ftmsObj$e_meta = temp 
  
  ftmsObj = setCarbonColName(ftmsObj, "C")
  ftmsObj =setHydrogenColName(ftmsObj, "H")
  ftmsObj = setOxygenColName(ftmsObj, "O")
  ftmsObj = setNitrogenColName(ftmsObj, "N")
  ftmsObj = setSulfurColName(ftmsObj, "S")
  ftmsObj = setPhosphorusColName(ftmsObj, "P")

  return(ftmsObj)
  
}