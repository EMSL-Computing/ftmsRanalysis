#' Assign compound class to each peak/mass
#' 
#' Assigns a compound class to each peak/mass, where possible, based Oxygen:Carbon ratio and Hydrogen:Carbon ratio and a chosen boundary set.
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#' @param boundary_set character vector specifying which boundary set to use when determining class. Valid options are currently "bs1", "bs2" and "bs3". Defaults to "bs1". See Details for more information on boundary sets.
#' @param calc_ratios logical argument, if elemental ratios needed for class assignment are not already calculated, should they be added to the data? Defaults to TRUE.
#' 
#' @details "bs1" and "bs2" are based on O:C and H:C ratio only, while "bs3" is based on other elemental ratios and counts. "bs1" is based on the boundaries given in Kim et al., 2003. "bs2" are relaxed boundary conditions given in Bailey et al., 2017 and "bs3" is given in Rivas-Ubach et al., 2018. See \code{\link{assign_class}} for comparisons of boundary sets with example data.
#' 
#' @references Kim, S., Kramer, R. W., & Hatcher, P. G. (2003). Graphical method for analysis of ultrahigh-resolution broadband mass spectra of natural organic matter, the van Krevelen diagram. Analytical Chemistry, 75(20), 5336-5344.
#' Bailey, V. L., Smith, A. P., Tfaily, M., Fansler, S. J., & Bond-Lamberty, B. (2017). Differences in soluble organic carbon chemistry in pore waters sampled from different pore size domains. Soil Biology and Biochemistry, 107, 133-143.
#' Rivas-Ubach, A., Liu, Y., Bianchi, T. S., Toliċ, N., Jansson, C., & Paša-Tolić, L. (2018). Moving beyond the van Krevelen diagram: A new stoichiometric approach for compound classification in organisms. Analytical chemistry. DOI: 10.1021/acs.analchem.8b00529 
#' 
#' @return an object of the same class as \code{icrData} with a column added in \code{e_meta} giving the class information for each peak/compound, when possible
#' 
#' @author Lisa Bramer
#' 
#' @export

assign_class <- function(icrData, boundary_set = "bs1", calc_ratios = TRUE){
  
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  # check that boundary_set is valid argument #
  if(!(boundary_set %in% c("bs1", "bs2", "bs3"))) stop("Invalid option provided for boundary_set argument.")
  
  # check that calc_vankrev is logical #
  if(class(calc_ratios) != "logical") stop("calc_ratios must be of class 'logical'")
  
  # check that O:C and H:C are non-NULL #
  # if they are NULL check calc_vankrev argument and act accordingly #
  if(is.null(getOCRatioColName(icrData)) | is.null(getHCRatioColName(icrData)) | is.null(getNCRatioColName(icrData)) | is.null(getPCRatioColName(icrData)) | is.null(getNPRatioColName(icrData))){
    if(calc_ratios == TRUE){
      icrData = compound_calcs(icrData, "calc_element_ratios")
    }else{
      stop("Elemental ratio data (e.g. O:C) not found in data and calc_ratios = FALSE. Set calc_ratios = TRUE or ensure ratios are present and specified in your data")
    }
  }
  
  # get boundary set data.frame #
  bound_df = getVanKrevelenCategoryBounds(boundary_set)$VKbounds
  bound_match = bound_df[-which(rownames(bound_df) == "Other"),]

  
  # get e_meta #
  temp = icrData$e_meta
  
  # identify rows without molecular formula #
  noforms = which(is.na(temp[,getMFColName(icrData)]))
  forms = which(!is.na(temp[,getMFColName(icrData)]))
  
  # if there are rows with no MF, take them out #  
  if(length(noforms) > 0){
    temp2 = temp[-noforms,]
  }else{
    temp2 = temp
  }
  
  if(boundary_set == "bs1"){
    classes = mapply(x_hc = temp2[,getHCRatioColName(icrData)], x_oc = temp2[,getOCRatioColName(icrData)], MoreArgs = list(bound_match), assign_bs1)
  }
  if(boundary_set == "bs2"){
    classes = mapply(x_hc = temp2[,getHCRatioColName(icrData)], x_oc = temp2[,getOCRatioColName(icrData)], MoreArgs = list(bound_match), assign_bs2)
    classes[which(classes == "")] = "Other"
  }
  if(boundary_set == "bs3"){
    classes = mapply(x_hc = temp2[,getHCRatioColName(icrData)], x_oc = temp2[,getOCRatioColName(icrData)], x_nc = temp2[,getNCRatioColName(icrData)], x_pc = temp2[,getPCRatioColName(icrData)], x_np = temp2[,getNPRatioColName(icrData)], x_o = temp2[,getOxygenColName(icrData)], x_n = temp2[,getNitrogenColName(icrData)], x_s = temp2[,getSulfurColName(icrData)], x_p = temp2[,getPhosphorusColName(icrData)], x_mass = temp2[,getMassColName(icrData)], MoreArgs = list(bound_match), assign_bs3)
    classes[which(classes == "")] = "Other"
    classes[grep("Nucleotide", classes)] = "Nucleotide"
  }
  
  
  comp_class = rep(NA, nrow(temp))
  comp_class[forms] = classes
  
  # add the column to e_meta #
  temp[,(ncol(temp) + 1)] = comp_class
  names(temp)[ncol(temp)] = paste(boundary_set, "_class", sep = "")
  
  # reassign temp back to e_meta in icrData #
  icrData$e_meta = temp
  
  # set the classcname #
  if(boundary_set == "bs1"){
    icrData = setBS1ColName(icrData, paste(boundary_set, "_class", sep = ""))
  }
  if(boundary_set == "bs2"){
    icrData = setBS2ColName(icrData, paste(boundary_set, "_class", sep = ""))
  }
  if(boundary_set == "bs3"){
    icrData = setBS3ColName(icrData, paste(boundary_set, "_class", sep = ""))
  }
  # return icrData #
  return(icrData)
}


assign_bs1 = function(x_hc, x_oc, bound_match){
  ids = which(x_hc >= bound_match$HC.low & x_hc <= bound_match$HC.high & x_oc >= bound_match$OC.low & x_oc <= bound_match$OC.high)
  if(length(ids) > 0){
    paste(rownames(bound_match)[ids], collapse = ";")
  }else{
    "Other"
  }
}

assign_bs2 = function(x_hc, x_oc, bound_match){

  lips = ifelse(x_hc >= bound_match$HC.low[1] & x_hc <= bound_match$HC.high[1] & x_oc > bound_match$OC.low[1] & x_oc <= bound_match$OC.high[1], "Lipid", NA)
  unsa = ifelse(x_hc >= bound_match$HC.low[2] & x_hc < bound_match$HC.high[2] & x_oc >= bound_match$OC.low[2] & x_oc <= bound_match$OC.high[2], "Unsat Hydrocarbon", NA)
  prot = ifelse(x_hc >= bound_match$HC.low[3] & x_hc <= bound_match$HC.high[3] & x_oc > bound_match$OC.low[3] & x_oc <= bound_match$OC.high[3], "Protein", NA)
  lign = ifelse(x_hc >= bound_match$HC.low[4] & x_hc < bound_match$HC.high[4] & x_oc > bound_match$OC.low[4] & x_oc <= bound_match$OC.high[4], "Lignin", NA)
  carb = ifelse(x_hc >= bound_match$HC.low[5] & x_hc <= bound_match$HC.high[5] & x_oc > bound_match$OC.low[5] & x_oc <= bound_match$OC.high[5], "Carbohydrate", NA)
  amsu = ifelse(x_hc >= bound_match$HC.low[6] & x_hc <= bound_match$HC.high[6] & x_oc > bound_match$OC.low[6] & x_oc <= bound_match$OC.high[6], "Amino Sugar", NA)
  tann = ifelse(x_hc >= bound_match$HC.low[7] & x_hc < bound_match$HC.high[7] & x_oc > bound_match$OC.low[7] & x_oc <= bound_match$OC.high[7], "Tannin", NA)
  cond = ifelse(x_hc >= bound_match$HC.low[8] & x_hc < bound_match$HC.high[8] & x_oc >= bound_match$OC.low[8] & x_oc <= bound_match$OC.high[8], "Cond Hydrocarbon", NA)
  
  ids = which(!is.na(c(lips, unsa, prot, lign, carb, amsu, tann, cond)))
  temps = paste(c(lips, unsa, prot, lign, carb, amsu, tann, cond)[ids], collapse = ";")
  temps
}

assign_bs3 = function(x_hc, x_oc, x_nc, x_pc, x_np, x_o, x_n, x_p, x_s, x_mass, bound_match){
  lips = ifelse(x_hc >= bound_match$HC.low[1] & x_oc <= bound_match$OC.high[1] & x_nc <= bound_match$NC.high[1] & x_pc < bound_match$PC.high[1] & x_np <= bound_match$NP.high[1], "Lipid", NA)
  prot1 = ifelse(x_hc > bound_match$HC.low[2] & x_hc < bound_match$HC.high[2] & x_oc > bound_match$OC.low[2] & x_oc <= bound_match$OC.high[2] & x_nc >= bound_match$NC.low[2] & x_nc <= bound_match$NC.high[2] & x_pc < bound_match$PC.high[2] & x_n >= bound_match$N.low[2], "Protein", NA)
  prot2 = ifelse(x_hc > bound_match$HC.low[3] & x_hc < bound_match$HC.high[3] & x_oc > bound_match$OC.low[3] & x_oc <= bound_match$OC.high[3] & x_nc > bound_match$NC.low[3] & x_nc <= bound_match$NC.high[3] & x_pc < bound_match$PC.high[3] & x_n >= bound_match$N.low[3], "Protein", NA)
 amsu = ifelse(x_hc >= bound_match$HC.low[4] & x_oc >= bound_match$OC.low[4] & x_nc > bound_match$NC.low[4] & x_nc <= bound_match$NC.high[4] & x_pc < bound_match$PC.high[4] & x_np <= bound_match$NP.high[4] & x_o >= bound_match$O.low[4] & x_n >= bound_match$N.low[4], "Amino Sugar", NA)
  carb = ifelse(x_hc >= bound_match$HC.low[5] & x_hc < bound_match$HC.high[5] & x_oc >= bound_match$OC.low[5] & x_n == bound_match$N.low[5], "Carbohydrate", NA)
  nucl = ifelse(x_hc > bound_match$HC.low[6] & x_hc < bound_match$HC.high[6] & x_oc >= bound_match$OC.low[6] & x_oc < bound_match$OC.high[6] & x_nc >= bound_match$NC.low[6] & x_nc <= bound_match$NC.high[6] & x_pc >= bound_match$PC.low[6] & x_pc < bound_match$PC.high[6] & x_np > bound_match$NP.low[6] & x_np <= bound_match$NP.high[6] & x_n >= bound_match$N.low[6] & x_p >= bound_match$P.low[6] & x_s == bound_match$S.low[6] & x_mass > bound_match$mass.low[6] & x_mass < bound_match$mass.high[6], "Nucleotide", NA)
  phyt = ifelse(x_hc < bound_match$HC.high[7] & x_oc <= bound_match$OC.high[7] & x_nc < bound_match$NC.high[7] & x_pc <= bound_match$PC.high[7] & x_np <= bound_match$NP.high[7], "Phytochemical", NA)
  
  ids = which(!is.na(c(lips, prot1, prot2, amsu, carb, nucl, phyt)))
  temps = paste(c(lips, prot1, prot2, amsu, carb, nucl, phyt)[ids], collapse = ";")
  temps
}
