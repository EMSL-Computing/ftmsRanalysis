#' Map peak ICR data to compounds 
#' 
#' Map peak leve ICR data to compounds from either KEGG or MetaCyc. 
#' @param picr object of type peakIcrData
#' @param db database to map to, either 'KEGG' or 'MetaCyc' (case insensitive). This requires
#' the KeggData package or the MetaCycData package respectively.
#' @return compoundIcrObject
#' @export
mapPeaksToCompounds <- function(picr, db="KEGG") {
  if (!inherits(picr, "peakIcrData")) {
    stop("picr must be an object of type peakIcrData")
  }
  if (!(toupper(db) %in% c("KEGG", "METACYC"))) {
    stop("db must be one of 'KEGG' or 'MetaCyc'")
  }

  require(dplyr)
  
  if (toupper(db) == "KEGG") {
    require(KeggData)
    data("kegg_compounds")
    compounds <- kegg_compounds
    # rm(kegg_compounds)
    
    data("kegg_compounds_per_formula")
    compounds_per_formula <- kegg_compounds_per_formula
    colnames(compounds_per_formula) <- c(getMFColName(picr), "Num_Compounds_Per_Formula")
    # rm(kegg_compounds_per_formula)
    
    db_name <- "KEGG"
  } else if (toupper(db) == "METACYC") {
    require(MetaCycData)
    data("mc_compounds")
    compounds <- mc_compounds %>%
      rename(FORMULA=MF)
    # rm(mc_compounds)
    
    data("mc_compounds_per_formula") ## TODO make this in MetaCycData package
    compounds_per_formula <- mc_compounds_per_formula
    colnames(compounds_per_formula) <- c(getMFColName(picr), "Num_Compounds_Per_Formula")
    # rm(mc_compounds_per_formula)
    
    db_name <- "MetaCyc"
  }
  
  # get only peaks that have mass formulas
  e_meta <- picr$e_meta %>%
    filter_(lazyeval::interp(~!is.na(var), var = as.name(getMFColName(picr)))) %>%
    mutate_(tmp19302859032 = as.character(getMFColName(picr))) %>%
    filter(tmp19302859032 != "") %>%
    select(-tmp19302859032)
  cmp.col <- select(compounds, FORMULA, COMPOUND)
  colnames(cmp.col) <- c(getMFColName(picr), "Compound")  
    
  # keep only rows that have a mapping in the compound database
  e_meta <- inner_join(e_meta, cmp.col)

  # filter e_data to match e_meta
  peaks.to.keep <- unique(select(e_meta, !!getEDataColName(picr), !!getMFColName(picr)))
  e_data <- left_join(peaks.to.keep, picr$e_data) %>%
    select(-matches(getEDataColName(picr)))
  
  # construct resulting compoundIcrData object
  res <- as.compoundIcrData(e_data, picr$f_data, e_meta, edata_cname=getMFColName(picr), fdata_cname=getFDataColName(picr), 
                            mass_cname=getMassColName(picr), mf_cname=getMFColName(picr), compound_cname="Compound")  
  
  # make a copy of column names attributes
  cnames.new <- attr(res, "cnames")
  cnames.old <- attr(picr, "cnames")
  for (cc in setdiff(names(cnames.old), c("edata_cname", "fdata_cname", "mass_cname", "mf_cname"))) {
    if (!is.null(cnames.old[[cc]]))
    cnames.new[[cc]] <- cnames.old[[cc]]
  }
  attr(res, "cnames") <- cnames.new
  attr(res, "DB") <- db_name
  attr(res, "filters") <- attr(picr, "filters")
  attr(res, "group_DF") <- attr(picr, "group_DF")
  attr(res, "instrument_type") <- attr(picr, "instrument_type")
  
  return(res)
}