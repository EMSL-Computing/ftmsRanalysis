#' Map compound level data to reactions in either KEGG or MetaCyc
#' 
#' Map compound data to KEGG or MetaCyc reactions. The database used
#' is determined by the database previously used to map peaks to compounds.
#' @param cicr an object of type compoundIcrData
#' @return reactionIcrData object
#' @export
mapCompoundsToReactions <- function(cicr) {
  if (!inherits(cicr, "compoundIcrData")) {
    stop("cicr must be an object of type compoundIcrData")
  }
  if (anyDuplicated(cicr$e_data[, getEDataColName(cicr)]) > 0) {
    stop("cicr cannot have duplicate compound rows in e_data")
  }
  
  db <- get_db(cicr)
  if (toupper(db) == "KEGG") {
    require(KEGGdata)
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
      rename(ENZYME=`EC-NUMBER`)
    
    data("mc_compounds")
    compounds <- mc_compounds 
    data("mc_compound_reaction_map")
    comp_rxn_map <- mc_compound_reaction_map
  }

  # data frame of Compounds and corresponding Reactions
  comp_rxn <- comp_rxn_map
  ind <- unlist(lapply(comp_rxn, function(x) return(all(is.null(x)) || all(is.na(x)))))
  comp_rxn <- comp_rxn[!ind]
  comp_rxn <- tibble::tibble(Compound=names(comp_rxn), Reaction=comp_rxn) %>%
    tidyr::unnest() %>%
    dplyr::filter(!is.na(Reaction))
  
  # add compounds to e_data 
  e_data <- select_(cicr$e_meta, getCompoundColName(cicr), getEDataColName(cicr)) %>%
    left_join(cicr$e_data) %>%
    select(-matches(getEDataColName(cicr)))
  
  # join e_data to reactions table by compound column
  join_by <- c("Compound")
  names(join_by) <- getCompoundColName(cicr)
  e_data <- inner_join(comp_rxn, e_data, by=join_by)
  
  # get compounds observed in this dataset for each reaction (for e_meta)
  observed_comp_per_rxn <- e_data %>% mutate(n_samples_848234=rowSums(select(., -one_of(c(getCompoundColName(cicr), "Reaction"))))) %>%
    select_(getCompoundColName(cicr), "Reaction", "n_samples_848234") %>%
    filter(n_samples_848234 > 0) %>%
    select(-n_samples_848234) %>%
    group_by(Reaction) %>%
    rename_(Compound = getCompoundColName(cicr)) %>%
    summarise(Compounds_in_Dataset=paste(Compound, collapse=";"))
    
  # collapse Reaction rows in e_data
  e_data <- e_data %>% 
    group_by(Reaction) %>%
    select(-matches(getCompoundColName(cicr))) %>%
    summarise_all(funs(sum)) %>%
    ungroup() %>%
    as.data.frame()
  
  e_meta <- data.frame(Reaction=e_data$Reaction, stringsAsFactors = FALSE) %>%
    left_join(select(reactions, REACTION, ENZYME), by=c('Reaction'='REACTION')) %>%
    rename(EC_Number=ENZYME) %>%
    left_join(observed_comp_per_rxn)
  
  # get # observable compounds per reaction subject to mass filter applied to cicr
  obs_comp <- inner_join(comp_rxn, compounds, by=c('Compound'='COMPOUND'))
  if (!is.null(attr(cicr, "filters")) && !is.null(attr(cicr, "filters")$massFilt)) {
    thresh.min <- min(attr(cicr, "filters")$massFilt$threshold)
    thresh.max <- max(attr(cicr, "filters")$massFilt$threshold)
    
    if (toupper(db) == "KEGG") {
      obs_comp <- icRanalysis:::kegg_mass_filter(obs_comp, thresh.min, thresh.max)
    } else if (toupper(db) == "METACYC") {
      obs_comp <- icRanalysis:::metacyc_mass_filter(obs_comp, thresh.min, thresh.max)
    } else {
      stop(paste("Unknown database:", db))
    }
  }
  
  obs_comp <- group_by(obs_comp, Reaction) %>%
    summarise(`N_Observable_Compounds`=n_distinct(Compound))
  e_meta <- left_join(e_meta, obs_comp)
  
  result <- as.reactionIcrData(e_data, cicr$f_data, e_meta, edata_cname = "Reaction", 
                               fdata_cname=getFDataColName(cicr), reaction_cname="Reaction")
  
  #attributes from cicr to carry forward:
  attr(result, "DB") <- db
  attr(result, "filters") <- attr(cicr, "filters")
  attr(result, "group_DF") <- attr(cicr, "group_DF")
  attr(result, "instrument_type") <- attr(cicr, "instrument_type")
  
  ## TODO: are there any other cnames that need to be carried through??
  if (!is.null(attr(cicr, "cnames")$extraction_cname)) {
    attr(result, "cnames")$extaction_cname <- attr(cicr, "cnames")$extraction_cname
  }
  
  
  return(result)
}
