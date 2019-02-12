#' Map compound level data to reactions in MetaCyc
#' 
#' Map compound data to MetaCyc modules. Additional database options may be added in the future.
#' For MetaCyc, modules are pathways that are not super-pathways. 
#' This requires the MetaCycData package (\link{http://github.com/EMSL-Computing/MetaCycData}).
#' 
#' @param compoundIcrData an object of type compoundIcrData
#' @return reactionIcrData object
#' 
#' @author Amanda White
#' 
#' @export
mapCompoundsToReactions <- function(compoundIcrData) {
  if (!inherits(compoundIcrData, "compoundIcrData")) {
    stop("compoundIcrData must be an object of type compoundIcrData")
  }
  if (anyDuplicated(compoundIcrData$e_data[, getEDataColName(compoundIcrData)]) > 0) {
    stop("compoundIcrData$e_data cannot have duplicates in the getEDataColName(compoundIcrData) column")
  }
  
  db <- getDatabase(compoundIcrData)
  if (toupper(db) == "METACYC") {
    require(MetaCycData)
    data("mc_reactions")
    reactions <- mc_reactions %>%
      dplyr::rename(ENZYME=`EC-NUMBER`)
    
    data("mc_compounds")
    compounds <- mc_compounds 
    data("mc_compound_reaction_map")
    comp_rxn_map <- mc_compound_reaction_map
  }
  
  if (getDataScale(compoundIcrData) != "pres") {
    compoundIcrData <- edata_transform(compoundIcrData, data_scale="pres")
  }

  # data frame of Compounds and corresponding Reactions
  comp_rxn <- comp_rxn_map
  ind <- unlist(lapply(comp_rxn, function(x) return(all(is.null(x)) || all(is.na(x)))))
  comp_rxn <- comp_rxn[!ind]
  comp_rxn <- tibble::tibble(Compound=names(comp_rxn), Reaction=comp_rxn) %>%
    tidyr::unnest() %>%
    dplyr::filter(!is.na(Reaction))
  
  # add compounds to e_data 
  e_data <- dplyr::select_(compoundIcrData$e_meta, getCompoundColName(compoundIcrData), getEDataColName(compoundIcrData)) %>%
    dplyr::left_join(compoundIcrData$e_data, by=getEDataColName(compoundIcrData)) %>%
    dplyr::select(-dplyr::matches(getEDataColName(compoundIcrData)))
  
  # join e_data to reactions table by compound column
  join_by <- c("Compound")
  names(join_by) <- getCompoundColName(compoundIcrData)
  e_data <- dplyr::inner_join(comp_rxn, e_data, by=join_by)
  
  # get compounds observed in this dataset for each reaction (for e_meta)
  observed_comp_per_rxn <- e_data %>% dplyr::mutate(n_samples_848234=rowSums(dplyr::select(., -dplyr::one_of(c(getCompoundColName(compoundIcrData), "Reaction"))))) %>%
    dplyr::select_(getCompoundColName(compoundIcrData), "Reaction", "n_samples_848234") %>%
    dplyr::filter(n_samples_848234 > 0) %>%
    dplyr::select(-n_samples_848234) %>%
    dplyr::group_by(Reaction) %>%
    dplyr::rename_(Compound = getCompoundColName(compoundIcrData)) %>%
    dplyr::summarise(Compounds_in_Dataset=paste(Compound, collapse=";"))
    
  # collapse Reaction rows in e_data
  e_data <- e_data %>% 
    dplyr::group_by(Reaction) %>%
    dplyr::select(-dplyr::matches(getCompoundColName(compoundIcrData))) %>%
    dplyr::summarise_all(dplyr::funs(sum)) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  
  e_meta <- data.frame(Reaction=e_data$Reaction, stringsAsFactors = FALSE) %>%
    dplyr::left_join(dplyr::select(reactions, REACTION, ENZYME), by=c('Reaction'='REACTION')) %>%
    dplyr::rename(EC_Number=ENZYME) %>%
    dplyr::left_join(observed_comp_per_rxn, by="Reaction")
  
  # get # observable compounds per reaction subject to mass filter applied to compoundIcrData
  obs_comp <- dplyr::inner_join(comp_rxn, compounds, by=c('Compound'='COMPOUND'))
  if (!is.null(attr(compoundIcrData, "filters")) && !is.null(attr(compoundIcrData, "filters")$massFilt)) {
    thresh.min <- min(attr(compoundIcrData, "filters")$massFilt$threshold)
    thresh.max <- max(attr(compoundIcrData, "filters")$massFilt$threshold)
    
    if (toupper(db) == "METACYC") {
      obs_comp <- fticRanalysis:::metacyc_mass_filter(obs_comp, thresh.min, thresh.max)
    } else {
      stop(paste("Unknown database:", db))
    }
  }
  
  obs_comp <- dplyr::group_by(obs_comp, Reaction) %>%
    dplyr::summarise(`N_Observable_Compounds`=dplyr::n_distinct(Compound))
  e_meta <- dplyr::left_join(e_meta, obs_comp, by="Reaction")
  
  result <- as.reactionIcrData(e_data, compoundIcrData$f_data, e_meta, edata_cname = "Reaction", 
                               fdata_cname=getFDataColName(compoundIcrData), reaction_cname="Reaction", 
                               instrument_type=getInstrumentType(compoundIcrData), db=getDatabase(compoundIcrData))
  
  #attributes from compoundIcrData to carry forward:
#   result <- fticRanalysis:::setDatabase(result, db)
  result <- fticRanalysis:::setGroupDF(result, getGroupDF(compoundIcrData))
#   result <- fticRanalysis:::setInstrumentType(result, getInstrumentType(compoundIcrData)) 
  result <- fticRanalysis:::setDataScale(result, "count")
  attr(result, "filters") <- attr(compoundIcrData, "filters")
  
  ## TODO: are there any other cnames that need to be carried through??
  if (!is.null(attr(compoundIcrData, "cnames")$extraction_cname)) {
    attr(result, "cnames")$extaction_cname <- attr(compoundIcrData, "cnames")$extraction_cname
  }
  
  return(result)
}
