#' Map compound level data to reactions in either KEGG or MetaCyc
#' 
#' Map compound data to KEGG or MetaCyc reactions. The database used
#' is determined by the database previously used to map peaks to compounds.
#' @param compoundObj an object of type compoundData
#' @return reactionIcrData object
#' 
#' @author Amanda White
#' 
#' @export
mapCompoundsToReactions <- function(compoundObj) {
  if (!inherits(compoundObj, "compoundData")) {
    stop("compoundObj must be an object of type 'compoundData'")
  }
  if (anyDuplicated(compoundObj$e_data[, getEDataColName(compoundObj)]) > 0) {
    stop("compoundObj$e_data cannot have duplicates in the getEDataColName(compoundObj) column")
  }
  
  db <- getDatabase(compoundObj)
  if (toupper(db) == "KEGG") {
    require(KeggData)
    data("kegg_reactions")
    reactions <- kegg_reactions
    data("kegg_compounds")
    compounds <- kegg_compounds
    
    data("kegg_compound_reaction_map")
    comp_rxn_map <- kegg_compound_reaction_map
    
  } else if (toupper(db) == "METACYC") {
    require(MetaCycData)
    data("mc_reactions")
    reactions <- mc_reactions %>%
      dplyr::rename(ENZYME=`EC-NUMBER`)
    
    data("mc_compounds")
    compounds <- mc_compounds 
    data("mc_compound_reaction_map")
    comp_rxn_map <- mc_compound_reaction_map
  }
  
  if (getDataScale(compoundObj) != "pres") {
    compoundObj <- edata_transform(compoundObj, data_scale="pres")
  }

  # data frame of Compounds and corresponding Reactions
  comp_rxn <- comp_rxn_map
  ind <- unlist(lapply(comp_rxn, function(x) return(all(is.null(x)) || all(is.na(x)))))
  comp_rxn <- comp_rxn[!ind]
  comp_rxn <- tibble::tibble(Compound=names(comp_rxn), Reaction=comp_rxn) %>%
    tidyr::unnest() %>%
    dplyr::filter(!is.na(Reaction))
  
  # add compounds to e_data 
  e_data <- dplyr::select_(compoundObj$e_meta, getCompoundColName(compoundObj), getEDataColName(compoundObj)) %>%
    dplyr::left_join(compoundObj$e_data, by=getEDataColName(compoundObj)) %>%
    dplyr::select(-dplyr::matches(getEDataColName(compoundObj)))
  
  # join e_data to reactions table by compound column
  join_by <- c("Compound")
  names(join_by) <- getCompoundColName(compoundObj)
  e_data <- dplyr::inner_join(comp_rxn, e_data, by=join_by)
  
  # get compounds observed in this dataset for each reaction (for e_meta)
  observed_comp_per_rxn <- e_data %>% dplyr::mutate(n_samples_848234=rowSums(dplyr::select(., -dplyr::one_of(c(getCompoundColName(compoundObj), "Reaction"))))) %>%
    dplyr::select_(getCompoundColName(compoundObj), "Reaction", "n_samples_848234") %>%
    dplyr::filter(n_samples_848234 > 0) %>%
    dplyr::select(-n_samples_848234) %>%
    dplyr::group_by(Reaction) %>%
    dplyr::rename_(Compound = getCompoundColName(compoundObj)) %>%
    dplyr::summarise(Compounds_in_Dataset=paste(Compound, collapse=";"))
    
  # collapse Reaction rows in e_data
  e_data <- e_data %>% 
    dplyr::group_by(Reaction) %>%
    dplyr::select(-dplyr::matches(getCompoundColName(compoundObj))) %>%
    dplyr::summarise_all(dplyr::funs(sum)) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  
  e_meta <- data.frame(Reaction=e_data$Reaction, stringsAsFactors = FALSE) %>%
    dplyr::left_join(dplyr::select(reactions, REACTION, ENZYME), by=c('Reaction'='REACTION')) %>%
    dplyr::rename(EC_Number=ENZYME) %>%
    dplyr::left_join(observed_comp_per_rxn, by="Reaction")
  
  # get # observable compounds per reaction subject to mass filter applied to compoundObj
  obs_comp <- dplyr::inner_join(comp_rxn, compounds, by=c('Compound'='COMPOUND'))
  if (!is.null(attr(compoundObj, "filters")) && !is.null(attr(compoundObj, "filters")$massFilt)) {
    thresh.min <- min(attr(compoundObj, "filters")$massFilt$threshold)
    thresh.max <- max(attr(compoundObj, "filters")$massFilt$threshold)
    
    if (toupper(db) == "KEGG") {
      obs_comp <- fticRanalysis:::kegg_mass_filter(obs_comp, thresh.min, thresh.max)
    } else if (toupper(db) == "METACYC") {
      obs_comp <- fticRanalysis:::metacyc_mass_filter(obs_comp, thresh.min, thresh.max)
    } else {
      stop(paste("Unknown database:", db))
    }
  }
  
  obs_comp <- dplyr::group_by(obs_comp, Reaction) %>%
    dplyr::summarise(`N_Observable_Compounds`=dplyr::n_distinct(Compound))
  e_meta <- dplyr::left_join(e_meta, obs_comp, by="Reaction")
  
  result <- as.reactionIcrData(e_data, compoundObj$f_data, e_meta, edata_cname = "Reaction", 
                               fdata_cname=getFDataColName(compoundObj), reaction_cname="Reaction", 
                               instrument_type=getInstrumentType(compoundObj), db=getDatabase(compoundObj))
  
  #attributes from compoundObj to carry forward:
#   result <- fticRanalysis:::setDatabase(result, db)
  result <- fticRanalysis:::setGroupDF(result, getGroupDF(compoundObj))
#   result <- fticRanalysis:::setInstrumentType(result, getInstrumentType(compoundObj)) 
  result <- fticRanalysis:::setDataScale(result, "count")
  attr(result, "filters") <- attr(compoundObj, "filters")
  
  ## TODO: are there any other cnames that need to be carried through??
  if (!is.null(attr(compoundObj, "cnames")$extraction_cname)) {
    attr(result, "cnames")$extaction_cname <- attr(compoundObj, "cnames")$extraction_cname
  }
  
  return(result)
}
