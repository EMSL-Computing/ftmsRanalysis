#' Map peak ICR data to compounds 
#' 
#' Map peak leve ICR data to compounds from MetaCyc. Additional database options may be added in the future.
#' @param peakIcrData object of type peakIcrData
#' @param db database to map to, currently 'MetaCyc' (case insensitive) is the 
#' only option. This requires the MetaCycData package (\link{http://github.com/EMSL-Computing/MetaCycData}).
#' @return compoundIcrObject
#' 
#' @author Amanda White
#' 
#' @export
mapPeaksToCompounds <- function(peakIcrData, db="MetaCyc") {
  if (!inherits(peakIcrData, "peakIcrData")) {
    stop("peakIcrData must be an object of type peakIcrData")
  }
  if (!(toupper(db) %in% c("METACYC"))) {
    stop("db must be 'MetaCyc'")
  }

  if (toupper(db) == "METACYC") {
    require(MetaCycData)
    data("mc_compounds")
    compounds <- mc_compounds %>%
      dplyr::rename(FORMULA=MF)
    # rm(mc_compounds)
    
    data("mc_compounds_per_formula") ## TODO make this in MetaCycData package
    compounds_per_formula <- mc_compounds_per_formula
    colnames(compounds_per_formula) <- c(getMFColName(peakIcrData), "Num_Compounds_Per_Formula")
    # rm(mc_compounds_per_formula)
    
    db_name <- "MetaCyc"
    
    # reassign molecular formulas using metacyc format
    suppressMessages(peakIcrData <- fticRanalysis:::assign_mf(peakIcrData, metacyc=TRUE))
  }
  
  # get only peaks that have mass formulas
  e_meta <- peakIcrData$e_meta %>%
    dplyr::filter_(lazyeval::interp(~!is.na(var), var = as.name(getMFColName(peakIcrData)))) %>%
    dplyr::mutate_(tmp19302859032 = as.character(getMFColName(peakIcrData))) %>%
    dplyr::filter(tmp19302859032 != "") %>%
    dplyr::select(-tmp19302859032)
  cmp.col <- dplyr::select(compounds, FORMULA, COMPOUND)
  colnames(cmp.col) <- c(getMFColName(peakIcrData), "Compound")  
    
  # keep only rows that have a mapping in the compound database
  e_meta <- dplyr::inner_join(e_meta, cmp.col, by=getMFColName(peakIcrData))

  # filter e_data to match e_meta
  peaks.to.keep <- unique(dplyr::select(e_meta, !!getEDataColName(peakIcrData), !!getMFColName(peakIcrData)))
  
  new_edata_colname <- getMFColName(peakIcrData)
  
  # identify if any compounds map to multiple masses. if so, an arbitrary unique ID column needs to be formed for e_data.
  if (anyDuplicated(peaks.to.keep[,  getMFColName(peakIcrData)]) > 0) {
    warning("Multiple masses map to the same molecular formula/compound. Constructing a unique ID for e_data.")
    peaks.to.keep$ID <- make.unique(peaks.to.keep[,  getMFColName(peakIcrData)])
    new_edata_colname <- "ID"
    e_meta <- dplyr::left_join(peaks.to.keep[, c("ID", getMFColName(peakIcrData))], e_meta, by=getMFColName(peakIcrData))
    peaks.to.keep <- dplyr::select(peaks.to.keep, -dplyr::matches(getMFColName(peakIcrData)))
  }
  
  e_data <- dplyr::left_join(peaks.to.keep, peakIcrData$e_data, by=getEDataColName(peakIcrData)) %>%
    dplyr::select(-dplyr::matches(getEDataColName(peakIcrData)))
  
  # construct resulting compoundIcrData object
  res <- as.compoundIcrData(e_data, peakIcrData$f_data, e_meta, edata_cname=new_edata_colname, fdata_cname=getFDataColName(peakIcrData), 
                            mass_cname=getMassColName(peakIcrData), mf_cname=getMFColName(peakIcrData), compound_cname="Compound",
                            data_scale=getDataScale(peakIcrData), instrument_type=getInstrumentType(peakIcrData))  
  
  # make a copy of column names attributes
  cnames.new <- attr(res, "cnames")
  cnames.old <- attr(peakIcrData, "cnames")
  for (cc in setdiff(names(cnames.old), c("edata_cname", "fdata_cname", "mass_cname", "mf_cname"))) {
    if (!is.null(cnames.old[[cc]]))
    cnames.new[[cc]] <- cnames.old[[cc]]
  }
  attr(res, "cnames") <- cnames.new
  attr(res, "filters") <- attr(peakIcrData, "filters")
  if (!is.null(getGroupDF(peakIcrData))) {
    res <- fticRanalysis:::setGroupDF(res, getGroupDF(peakIcrData))
  }
  # attr(res, "instrument_type") <- attr(peakIcrData, "instrument_type")
  # attr(res, "data_info") <- attr(peakIcrData, "data_info")
  res <- fticRanalysis:::setDatabase(res, db)
  
  return(res)
}