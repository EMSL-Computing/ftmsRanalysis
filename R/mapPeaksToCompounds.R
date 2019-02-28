#' Map peak ICR data to compounds 
#' 
#' Map peak leve ICR data to compounds from either KEGG or MetaCyc. 
#'
#' @param peakObj object of type peakData
#' @param db database to map to, either 'KEGG' or 'MetaCyc' (case insensitive). This requires
#' the KeggData package or the MetaCycData package respectively.
#' @return compoundIcrObject
#' 
#' @author Amanda White
#' 
#' @export
mapPeaksToCompounds <- function(peakObj, db="MetaCyc") {
  if (!inherits(peakObj, "peakData")) {
    stop("peakData must be an object of type peakData")
  }
  if (!(toupper(db) %in% c("KEGG", "METACYC"))) {
    stop("db must be one of 'KEGG' or 'MetaCyc'")
  }

  if (toupper(db) == "KEGG") {
    require(KeggData)
    data("kegg_compounds")
    compounds <- kegg_compounds
    # rm(kegg_compounds)
    
    data("kegg_compounds_per_formula")
    compounds_per_formula <- kegg_compounds_per_formula
    colnames(compounds_per_formula) <- c(getMFColName(peakObj), "Num_Compounds_Per_Formula")
    # rm(kegg_compounds_per_formula)
    
    db_name <- "KEGG"
  } else if (toupper(db) == "METACYC") {
    require(MetaCycData)
    data("mc_compounds")
    compounds <- mc_compounds %>%
      dplyr::rename(FORMULA=MF)
    # rm(mc_compounds)
    
    data("mc_compounds_per_formula") ## TODO make this in MetaCycData package
    compounds_per_formula <- mc_compounds_per_formula
    colnames(compounds_per_formula) <- c(getMFColName(peakObj), "Num_Compounds_Per_Formula")
    # rm(mc_compounds_per_formula)
    
    db_name <- "MetaCyc"
    
    # reassign molecular formulas using metacyc format
    suppressMessages(peakObj <- fticRanalysis:::assign_mf(peakObj, metacyc=TRUE))
  }
  
  # get only peaks that have mass formulas
  e_meta <- peakObj$e_meta %>%
    dplyr::filter_(lazyeval::interp(~!is.na(var), var = as.name(getMFColName(peakObj)))) %>%
    dplyr::mutate_(tmp19302859032 = as.character(getMFColName(peakObj))) %>%
    dplyr::filter(tmp19302859032 != "") %>%
    dplyr::select(-tmp19302859032)
  cmp.col <- dplyr::select(compounds, FORMULA, COMPOUND)
  colnames(cmp.col) <- c(getMFColName(peakObj), "Compound")  
    
  # keep only rows that have a mapping in the compound database
  e_meta <- dplyr::inner_join(e_meta, cmp.col, by=getMFColName(peakObj))

  # filter e_data to match e_meta
  peaks.to.keep <- unique(dplyr::select(e_meta, !!getEDataColName(peakObj), !!getMFColName(peakObj)))
  
  new_edata_colname <- getMFColName(peakObj)
  
  # identify if any compounds map to multiple masses. if so, an arbitrary unique ID column needs to be formed for e_data.
  if (anyDuplicated(peaks.to.keep[,  getMFColName(peakObj)]) > 0) {
    warning("Multiple masses map to the same molecular formula/compound. Constructing a unique ID for e_data.")
    peaks.to.keep$ID <- make.unique(peaks.to.keep[,  getMFColName(peakObj)])
    new_edata_colname <- "ID"
    e_meta <- dplyr::left_join(peaks.to.keep[, c("ID", getMFColName(peakObj))], e_meta, by=getMFColName(peakObj))
    peaks.to.keep <- dplyr::select(peaks.to.keep, -dplyr::matches(getMFColName(peakObj)))
  }
  
  e_data <- dplyr::left_join(peaks.to.keep, peakObj$e_data, by=getEDataColName(peakObj)) %>%
    dplyr::select(-dplyr::matches(getEDataColName(peakObj)))
  
  # construct resulting compoundIcrData object
  res <- as.compoundIcrData(e_data, peakObj$f_data, e_meta, edata_cname=new_edata_colname, fdata_cname=getFDataColName(peakObj), 
                            mass_cname=getMassColName(peakObj), mf_cname=getMFColName(peakObj), compound_cname="Compound",
                            data_scale=getDataScale(peakObj), instrument_type=getInstrumentType(peakObj))  
  
  # make a copy of column names attributes
  cnames.new <- attr(res, "cnames")
  cnames.old <- attr(peakObj, "cnames")
  for (cc in setdiff(names(cnames.old), c("edata_cname", "fdata_cname", "mass_cname", "mf_cname"))) {
    if (!is.null(cnames.old[[cc]]))
    cnames.new[[cc]] <- cnames.old[[cc]]
  }
  attr(res, "cnames") <- cnames.new
  attr(res, "filters") <- attr(peakObj, "filters")
  if (!is.null(getGroupDF(peakObj))) {
    res <- fticRanalysis:::setGroupDF(res, getGroupDF(peakObj))
  }
  # attr(res, "instrument_type") <- attr(peakObj, "instrument_type")
  # attr(res, "data_info") <- attr(peakObj, "data_info")
  res <- fticRanalysis:::setDatabase(res, db)
  
  return(res)
}