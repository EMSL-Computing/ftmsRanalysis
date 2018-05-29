#' Map compound level data to modules in either KEGG or MetaCyc
#' 
#' Map compound data to KEGG or MetaCyc modules The database used
#' is determined by the database previously used to map peaks to compounds.
#' For MetaCyc, modules are pathways that are not super-pathways
#' @param compoundIcrData an object of type compoundIcrData
#' @return moduleIcrData object
#' 
#' @author Amanda White
#' 
#' @export
mapCompoundsToModules <- function(compoundIcrData) {
  if (!inherits(compoundIcrData, "compoundIcrData")) {
    stop("compoundIcrData must be an object of type compoundIcrData")
  }
  if (anyDuplicated(compoundIcrData$e_data[, getEDataColName(compoundIcrData)]) > 0) {
    stop("compoundIcrData$e_data cannot have duplicates in the getEDataColName(compoundIcrData) column")
  }

  db <- getDatabase(compoundIcrData)
  if (toupper(db) == "KEGG") {
    require(KeggData)
    data("kegg_compounds")
    compounds <- kegg_compounds
    
    data("kegg_compound_reaction_map")
    comp_rxn_map <- kegg_compound_reaction_map
    
    data("kegg_module_reaction_map")
    mod_rxn_map <- kegg_module_reaction_map
    
    # mapping from compound --> reaction --> module node
    data("kegg_reaction_module_node_map")
    reaction_module_node_map <- kegg_reaction_module_node_map

  } else if (toupper(db) == "METACYC") {
    require(MetaCycData)

    data("mc_compounds")
    compounds <- mc_compounds
    data("mc_compound_reaction_map")
    comp_rxn_map <- mc_compound_reaction_map
    
    # mapping from compound --> reaction --> module node
    data("mc_reaction_module_node_map")
    reaction_module_node_map <- mc_reaction_module_node_map
    
  }

  if (getDataScale(compoundIcrData) != "pres") {
    compoundIcrData <- edata_transform(compoundIcrData, data_scale="pres")
  }
  
  comp_rxn <- comp_rxn_map
  ind <- unlist(lapply(comp_rxn, function(x) return(all(is.null(x)) || all(is.na(x)))))
  comp_rxn <- comp_rxn[!ind]
  comp_rxn <- tibble::tibble(Compound=names(comp_rxn), Reaction=comp_rxn) %>%
    tidyr::unnest() %>%
    dplyr::filter(!is.na(Reaction))

  # mapping from compound --> reaction --> module node
  comp_rxn_mod <- dplyr::inner_join(comp_rxn, reaction_module_node_map, by=c('Reaction'='REACTION'))
  comp_rxn_mod <- dplyr::rename(comp_rxn_mod, Module = MODULE, Module_Node = MODULE_NODE) %>%
    dplyr::mutate(Module_Node_Comb=paste0(Module, ": ", Module_Node))
  
  # add compounds to e_data 
  e_data <- dplyr::select_(compoundIcrData$e_meta, getCompoundColName(compoundIcrData), getEDataColName(compoundIcrData)) %>%
    dplyr::left_join(compoundIcrData$e_data, by=getEDataColName(compoundIcrData)) %>%
    dplyr::select(-dplyr::matches(getEDataColName(compoundIcrData)))
  
  # join e_data to compound/reaction/module mapping
  join_by <- c("Compound")
  names(join_by) <- getCompoundColName(compoundIcrData)
  e_data <- dplyr::inner_join(comp_rxn_mod, e_data, by=join_by) 
  
  # get compounds observed in this dataset for each reaction (for e_meta)
  observed_comp_per_rxn <- e_data %>% 
    dplyr::mutate(n_samples_848234=rowSums(dplyr::select(., -dplyr::one_of(c(getCompoundColName(compoundIcrData)), "Reaction", "Module_Node", "Module", "Module_Node_Comb")))) %>%
    dplyr::select_(getCompoundColName(compoundIcrData), "Module_Node_Comb", "n_samples_848234") %>%
    dplyr::filter(n_samples_848234 > 0) %>%
    dplyr::select(-n_samples_848234) %>%
    dplyr::group_by(Module_Node_Comb) %>%
    dplyr::rename_(Compound = getCompoundColName(compoundIcrData)) %>%
    dplyr::summarise(Compounds_in_Dataset=paste(Compound, collapse=";"))
  
  e_meta <- dplyr::select(e_data, Module_Node, Module, Module_Node_Comb) %>%
    dplyr::left_join(observed_comp_per_rxn, by="Module_Node_Comb")
  
  ## Calculate number of UNIQUE compounds per node
  e_data <- e_data %>%
    dplyr::group_by_("Module_Node_Comb", getCompoundColName(compoundIcrData)) %>%
    dplyr::summarise_if(is.numeric, dplyr::funs(sum)) %>%
    dplyr::ungroup()
  
  e_data <- e_data %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(ifelse(.>0, 1, 0))) %>%
    dplyr::select(-dplyr::matches(getCompoundColName(compoundIcrData))) %>%
    dplyr::group_by(Module_Node_Comb) %>%
    dplyr::summarise_all(dplyr::funs(sum)) %>% 
    dplyr::ungroup() %>%
    as.data.frame()
  
  # get # OBSERVABLE compounds per reaction subject to mass filter applied to compoundIcrData
  obs_comp <- dplyr::inner_join(comp_rxn_mod, compounds, by=c('Compound'='COMPOUND'))
  if (!is.null(attr(compoundIcrData, "filters")) && !is.null(attr(compoundIcrData, "filters")$massFilt)) {
    thresh.min <- min(attr(compoundIcrData, "filters")$massFilt$threshold)
    thresh.max <- max(attr(compoundIcrData, "filters")$massFilt$threshold)
    if (toupper(db) == "KEGG") {
      obs_comp <- fticRanalysis:::kegg_mass_filter(obs_comp, thresh.min, thresh.max)
    } else if (toupper(db) == "METACYC") {
      obs_comp <- fticRanalysis:::metacyc_mass_filter(obs_comp, thresh.min, thresh.max)
    } else {
      stop(paste("Unknown database:", db))
    }
  }
  
  obs_comp <- dplyr::group_by(obs_comp, Module_Node_Comb) %>%
    dplyr::summarise(`N_Observable_Compounds`=n_distinct(Compound))
  e_meta <- dplyr::left_join(e_meta, obs_comp, by="Module_Node_Comb") %>% as.data.frame()
  e_meta <- dplyr::arrange(e_meta, Module, Module_Node_Comb)
  
  result <- as.moduleIcrData(e_data, compoundIcrData$f_data, e_meta, edata_cname = "Module_Node_Comb", 
                             fdata_cname=getFDataColName(compoundIcrData), module_cname="Module", 
                             module_node_cname = "Module_Node", instrument_type = getInstrumentType(compoundIcrData), 
                             db=getDatabase(compoundIcrData))

  #attributes from compoundIcrData to carry forward:
#   result <- fticRanalysis:::setDatabase(result, db)
  result <- fticRanalysis:::setGroupDF(result, getGroupDF(compoundIcrData))
#   result <- fticRanalysis:::setInstrumentType(result, getInstrumentType(compoundIcrData))
  attr(result, "filters") <- attr(compoundIcrData, "filters")
  
  ## TODO: are there any other cnames that need to be carried through??
  if (!is.null(attr(compoundIcrData, "cnames")$extraction_cname)) {
    attr(result, "cnames")$extaction_cname <- attr(compoundIcrData, "cnames")$extraction_cname
  }

  return(result)
}
