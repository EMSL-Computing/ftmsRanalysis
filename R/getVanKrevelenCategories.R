#' Get Van Krevelen category bounds for a boundary set
#' 
#' Print Van Krevelen category bounds for a boundary set
#'
#' @param boundary_set character vector specifying which boundary set to use when determining class. Valid options are currently "bs1", "bs2", and "bs3".
#' @return  a data.frame of Van Krevelen category bounds (Hydrogen:Carbon and Oxygen:Carbon lower and upper limits) for each category.
#' @details "bs1" and "bs2" are based on O:C and H:C ratio only, while "bs3" is based on other elemental ratios and counts. "bs1" is based on the boundaries given in Kim et al., 2003. "bs2" are relaxed boundary conditions given in Bailey et al., 2017 and "bs3" is given in Rivas-Ubach et al., 2018. See \code{\link{assign_class}} for comparisons of boundary sets with example data.
#' 
#' @references Kim, S., Kramer, R. W., & Hatcher, P. G. (2003). Graphical method for analysis of ultrahigh-resolution broadband mass spectra of natural organic matter, the van Krevelen diagram. Analytical Chemistry, 75(20), 5336-5344.
#' Bailey, V. L., Smith, A. P., Tfaily, M., Fansler, S. J., & Bond-Lamberty, B. (2017). Differences in soluble organic carbon chemistry in pore waters sampled from different pore size domains. Soil Biology and Biochemistry, 107, 133-143.
#' Rivas-Ubach, A., Liu, Y., Bianchi, T. S., Toliċ, N., Jansson, C., & Paša-Tolić, L. (2018). Moving beyond the van Krevelen diagram: A new stoichiometric approach for compound classification in organisms. Analytical chemistry. DOI: 10.1021/acs.analchem.8b00529
#' 
#' @author Lisa Bramer
#' @export
getVanKrevelenCategoryBounds <- function(boundary_set) {
  
  if(!(boundary_set %in% c("bs1", "bs2", "bs3"))) stop("Invalid option provided for boundary_set argument.")
  
  if(boundary_set == "bs1"){
  HC.low = c(">= 1.55",">= 0.7",">= 1.45",">= 0.81",">= 1.48",">= 1.34",">= 0.7",">= 0.3", ">= 0")
  HC.high = c("<= 2.25", "<= 1.5","<= 2","<= 1.45","<= 2.15","<= 1.8","<= 1.3","<= 0.81", "<= Inf")
  OC.low = c(">= 0",">= 0.05",">= 0.3",">= 0.28",">= 0.68",">= 0.54",">= 0.65",">= 0.12", ">= 0")
  OC.high = c("<= 0.3","<= 0.15","<= 0.55","<= 0.65","<= 1","<= 0.71","<= 1.05","<= 0.7", "<= Inf")
  NC.low = rep(">= 0", 9)
  NC.high = rep("< Inf", 9)
  PC.low = rep(">= 0", 9)
  PC.high = rep("< Inf", 9)
  NP.low = rep(">= 0", 9)
  NP.high = rep("< Inf", 9)
  O.low = rep(">= 0", 9)
  O.high = rep("< Inf", 9)
  N.low = rep(">= 0", 9)
  N.high = rep("< Inf", 9)
  P.low = rep(">= 0", 9)
  P.high = rep("< Inf", 9)
  S.low = rep(">= 0", 9)
  S.high = rep("< Inf", 9)
  mass.low = rep(">= 0", 9)
  mass.high = rep("< Inf", 9)
  category = c("Lipid","Unsat Hydrocarbon", "Protein","Lignin","Carbohydrate","Amino Sugar","Tannin","Cond Hydrocarbon","Other")
  }
  if(boundary_set == "bs2"){
    HC.low = c(">= 1.5", ">= 0.8", ">= 1.5", ">= 0.8", ">= 1.5", ">= 1.5", ">= 0.8", ">= 0.2", ">= 0")
    HC.high = c("<= 2.5", "< 2.5", "<= 2.3", "< 1.5", "<= 2.5", "<= 2.2", "< 1.5", "< 0.8", "< Inf")
    OC.low = c("> 0", ">= 0", "> 0.3", "> 0.125", "> 0.7", "> 0.55", "> 0.65", ">= 0", ">= 0")
    OC.high = c("<= 0.3", "<= 0.125", "<= 0.55", "<= 0.65", "<= 1.5", "<= 0.7", "<= 1.1", "<= 0.95", "< Inf")
    NC.low = rep(">= 0", 9)
    NC.high = rep("< Inf", 9)
    PC.low = rep(">= 0", 9)
    PC.high = rep("< Inf", 9)
    NP.low = rep(">= 0", 9)
    NP.high = rep("< Inf", 9)
    O.low = rep(">= 0", 9)
    O.high = rep("< Inf", 9)
    N.low = rep(">= 0", 9)
    N.high = rep("< Inf", 9)
    P.low = rep(">= 0", 9)
    P.high = rep("< Inf", 9)
    S.low = rep(">= 0", 9)
    S.high = rep("< Inf", 9)
    mass.low = rep(">= 0", 9)
    mass.high = rep("< Inf", 9)
    category = c("Lipid","Unsat Hydrocarbon", "Protein","Lignin","Carbohydrate","Amino Sugar","Tannin","Cond Hydrocarbon","Other")
  }
  if(boundary_set == "bs3"){
    OC.low = c(">= 0", "> 0.12", "> 0.6", ">= 0.61", ">= 0.8", ">= 0.5", ">= 0", ">= 0")
    OC.high = c("<= 0.6", "<= 0.6", "<= 1", "< Inf", "< Inf", "< 1.7", "<= 1.15", "< Inf")
    HC.low = c(">= 1.32", "> 0.9", "> 1.2", ">= 1.45", ">= 1.65", "> 1", ">= 0", ">= 0")
    HC.high = c("< Inf", "< 2.5", "< 2.5", "< Inf", "< 2.7", "< 1.8", "< 1.32", "< Inf")
    NC.low = c(">= 0", ">= 0.126", "> 0.2", "> 0.07", ">= 0", ">= 0.2", ">= 0", ">= 0")
    NC.high = c("<= 0.126", "<= 0.7", "<= 0.7", "<= 0.2", "< Inf", "<= 0.5", "< 0.126", "< Inf" )
    PC.low = c(rep(">= 0", 5), ">= 0.1", ">= 0", ">= 0")
    PC.high = c("< 0.35", "< 0.17", "< 0.17", "< 0.3", "< Inf", "< 0.35", "<= 0.2", "< Inf")
    NP.low = c(rep(">= 0", 5), "> 0.6", ">= 0", ">= 0")
    NP.high = c("<= 5", "< Inf", "< Inf", "<= 2", "< Inf", "<= 5", "<= 3", "< Inf")
    O.low = c(rep(">= 0", 3), ">= 3", rep(">= 0", 4))
    O.high = rep("< Inf", 8)
    N.low = c(">= 0", rep(">= 1", 3), ">= 0", ">= 2", ">= 0", ">= 0")
    N.high = c(rep("< Inf", 4), "<= 0", rep("< Inf", 3))
    P.low = c(rep(">= 0", 5), ">= 1", ">= 0", ">= 0")
    P.high = rep("< Inf", 8)
    S.low = rep(">= 0", 8)
    S.high = c(rep("< Inf", 5), "<= 0", rep("< Inf", 2))
    mass.low = c(rep(">= 0", 5), "> 305", rep(">= 0", 2))
    mass.high = c(rep("< Inf", 5), "< 523", rep("< Inf", 2))
    category = c("Lipid", "Protein","Protein2", "Amino Sugar","Carbohydrate","Nucleotide", "Phytochemical","Other")
  }
  vanKrevelenCategoryLogic <- data.frame(HC.low, HC.high, OC.low, OC.high, NC.low, NC.high, PC.low, PC.high, NP.low, NP.high, O.low, O.high, N.low, N.high, P.low, P.high, S.low, S.high, mass.low, mass.high, stringsAsFactors = F)
  vanKrevelenCategoryBounds = vanKrevelenCategoryLogic
  vanKrevelenCategoryBounds = data.frame(lapply(vanKrevelenCategoryBounds, function(x) as.numeric(as.character(gsub("> |>= |< |<= ", "", x)))))
  rownames(vanKrevelenCategoryBounds) <- as.character(category)
  rownames(vanKrevelenCategoryLogic) = as.character(category)
  return(list(VKbounds = vanKrevelenCategoryBounds, VKlogic = vanKrevelenCategoryLogic))
}


# Returns a data.frame in a convenient format for plotting the boundaries of
# the Van Krevelen categories.
# Internal only
getVanKrevelenPlottingDF <- function(boundary_set) {
  
  # should not use this for "bs3" as definition depends on more than HC and OC #
   if(!(boundary_set %in% c("bs1", "bs2"))) stop("Invalid option provided for boundary_set argument.")
  vkBounds <- getVanKrevelenCategoryBounds(boundary_set = boundary_set)$VKbounds
  # data.frame for plotting purposes
  vankrev_categories <- data.frame(y0 = c(vkBounds$HC.low, vkBounds$HC.high, vkBounds$HC.low, vkBounds$HC.low), 
                                   y1 = c(vkBounds$HC.low, vkBounds$HC.high, vkBounds$HC.high, vkBounds$HC.high), 
                                   x0 = c(vkBounds$OC.low, vkBounds$OC.low, vkBounds$OC.low, vkBounds$OC.high), 
                                   x1 = c(vkBounds$OC.high, vkBounds$OC.high, vkBounds$OC.low, vkBounds$OC.high), 
                                   category = rep(rownames(vkBounds), 4))
  

  return(vankrev_categories)
}
