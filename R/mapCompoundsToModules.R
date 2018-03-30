#' Map compound level data to modules in either KEGG or MetaCyc
#' 
#' Map compound data to KEGG or MetaCyc modules The database used
#' is determined by the database previously used to map peaks to compounds.
#' For MetaCyc, modules are pathways that are not super-pathways
#' @param cicr an object of type compoundIcrData
#' @return moduleIcrData object
#' @export
mapCompoundsToModules <- function(cicr) {
  if (!inherits(cicr, "compoundIcrData")) {
    stop("cicr must be an object of type compoundIcrData")
  }
  if (anyDuplicated(cicr$e_data[, getEDataColName(cicr)]) > 0) {
    stop("cicr cannot have duplicate compound rows in e_data")
  }
  require(dplyr)
  
  db <- get_db(cicr)
  if (toupper(db) == "KEGG") {
    require(KeggData)
    # data("kegg_reactions")
    # reactions <- kegg_reactions
    data("kegg_compounds")
    compounds <- kegg_compounds
    
    data("kegg_compound_reaction_map")
    comp_rxn_map <- kegg_compound_reaction_map
    
#     data("kegg_modules")
#     modules <- kegg_modules
    data("kegg_module_reaction_map")
    mod_rxn_map <- kegg_module_reaction_map
    
    # mapping from compound --> reaction --> module node
    data("kegg_reaction_module_node_map")
    reaction_module_node_map <- kegg_reaction_module_node_map

  } else if (toupper(db) == "METACYC") {
    require(MetaCycData)
    # data("mc_reactions")
    # reactions <- mc_reactions
    
    data("mc_compounds")
    compounds <- mc_compounds
    data("mc_compound_reaction_map")
    comp_rxn_map <- mc_compound_reaction_map
    
    # mapping from compound --> reaction --> module node
    data("mc_reaction_module_node_map")
    reaction_module_node_map <- mc_reaction_module_node_map
    
  }

  comp_rxn <- comp_rxn_map
  ind <- unlist(lapply(comp_rxn, function(x) return(all(is.null(x)) || all(is.na(x)))))
  comp_rxn <- comp_rxn[!ind]
  comp_rxn <- tibble::tibble(Compound=names(comp_rxn), Reaction=comp_rxn) %>%
    tidyr::unnest() %>%
    dplyr::filter(!is.na(Reaction))

  # mapping from compound --> reaction --> module node
  comp_rxn_mod <- inner_join(comp_rxn, reaction_module_node_map, by=c('Reaction'='REACTION'))
  comp_rxn_mod <- rename(comp_rxn_mod, Module = MODULE, Module_Node = MODULE_NODE) %>%
    mutate(Module_Node_Comb=paste0(Module, ": ", Module_Node))
  
  # add compounds to e_data 
  e_data <- select_(cicr$e_meta, getCompoundColName(cicr), getEDataColName(cicr)) %>%
    left_join(cicr$e_data) %>%
    select(-matches(getEDataColName(cicr)))
  
  # join e_data to compound/reaction/module mapping
  join_by <- c("Compound")
  names(join_by) <- getCompoundColName(cicr)
  e_data <- inner_join(comp_rxn_mod, e_data, by=join_by) 
  
  # get compounds observed in this dataset for each reaction (for e_meta)
  observed_comp_per_rxn <- e_data %>% 
    mutate(n_samples_848234=rowSums(select(., -one_of(c(getCompoundColName(cicr)), "Reaction", "Module_Node", "Module", "Module_Node_Comb")))) %>%
    select_(getCompoundColName(cicr), "Module_Node_Comb", "n_samples_848234") %>%
    filter(n_samples_848234 > 0) %>%
    select(-n_samples_848234) %>%
    group_by(Module_Node_Comb) %>%
    rename_(Compound = getCompoundColName(cicr)) %>%
    summarise(Compounds_in_Dataset=paste(Compound, collapse=";"))
  
  e_meta <- select(e_data, Module_Node, Module, Module_Node_Comb) %>%
    left_join(observed_comp_per_rxn)
  
    ## Calculate number of UNIQUE compounds per node
  e_data <- e_data %>%
    group_by_("Module_Node_Comb", getCompoundColName(cicr)) %>%
    summarise_if(is.numeric, funs(sum)) %>%
    ungroup()
  
  e_data <- e_data %>%
    mutate_if(is.numeric, funs(ifelse(.>0, 1, 0))) %>%
    select(-matches(getCompoundColName(cicr))) %>%
    group_by(Module_Node_Comb) %>%
    summarise_all(funs(sum)) %>% 
    ungroup() %>%
    as.data.frame()
  
  # get # OBSERVABLE compounds per reaction subject to mass filter applied to cicr
  obs_comp <- inner_join(comp_rxn_mod, compounds, by=c('Compound'='COMPOUND'))
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
  
  obs_comp <- group_by(obs_comp, Module_Node_Comb) %>%
    summarise(`N_Observable_Compounds`=n_distinct(Compound))
  e_meta <- left_join(e_meta, obs_comp) %>% as.data.frame()
  e_meta <- arrange(e_meta, Module, Module_Node_Comb)
  
  result <- as.moduleIcrData(e_data, cicr$f_data, e_meta, edata_cname = "Module_Node_Comb", 
                               fdata_cname=getFDataColName(cicr), module_cname="Module", module_node_cname = "Module_Node")

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
