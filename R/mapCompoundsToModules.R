#' Map compound level data to modules in either KEGG or MetaCyc
#' 
#' Map compound data to KEGG or MetaCyc modules The database used
#' is determined by the database previously used to map peaks to compounds.
#' For MetaCyc, modules are pathways that are not super-pathways
#' @param compoundObj an object of type compoundData
#' @return moduleData object
#' 
#' @author Amanda White
#' 
#' @export
mapCompoundsToModules <- function(compoundObj) {
  if (!inherits(compoundObj, "compoundData")) {
    stop("compoundObj must be an object of type 'compoundData'")
  }
  if (anyDuplicated(compoundObj$e_data[, getEDataColName(compoundObj)]) > 0) {
    stop("compoundObj$e_data cannot have duplicates in the getEDataColName(compoundObj) column")
  }

  db <- getDatabase(compoundObj)
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

  if (getDataScale(compoundObj) != "pres") {
    compoundObj <- edata_transform(compoundObj, data_scale="pres")
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
  e_data <- dplyr::select_(compoundObj$e_meta, getCompoundColName(compoundObj), getEDataColName(compoundObj)) %>%
    dplyr::left_join(compoundObj$e_data, by=getEDataColName(compoundObj)) %>%
    dplyr::select(-dplyr::matches(getEDataColName(compoundObj)))
  
  # join e_data to compound/reaction/module mapping
  join_by <- c("Compound")
  names(join_by) <- getCompoundColName(compoundObj)
  e_data <- dplyr::inner_join(comp_rxn_mod, e_data, by=join_by) 
  
  # get compounds observed in this dataset for each reaction (for e_meta)
  observed_comp_per_rxn <- e_data %>% 
    dplyr::mutate(n_samples_848234=rowSums(dplyr::select(., -dplyr::one_of(c(getCompoundColName(compoundObj)), "Reaction", "Module_Node", "Module", "Module_Node_Comb")))) %>%
    dplyr::select_(getCompoundColName(compoundObj), "Module_Node_Comb", "n_samples_848234") %>%
    dplyr::filter(n_samples_848234 > 0) %>%
    dplyr::select(-n_samples_848234) %>%
    dplyr::group_by(Module_Node_Comb) %>%
    dplyr::rename_(Compound = getCompoundColName(compoundObj)) %>%
    dplyr::summarise(Compounds_in_Dataset=paste(Compound, collapse=";"))
  
  e_meta <- dplyr::select(e_data, Module_Node, Module, Module_Node_Comb) %>%
    dplyr::left_join(observed_comp_per_rxn, by="Module_Node_Comb")
  
  ## Calculate number of UNIQUE compounds per node
  e_data <- e_data %>%
    dplyr::group_by_("Module_Node_Comb", getCompoundColName(compoundObj)) %>%
    dplyr::summarise_if(is.numeric, dplyr::funs(sum)) %>%
    dplyr::ungroup()
  
  e_data <- e_data %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(ifelse(.>0, 1, 0))) %>%
    dplyr::select(-dplyr::matches(getCompoundColName(compoundObj))) %>%
    dplyr::group_by(Module_Node_Comb) %>%
    dplyr::summarise_all(dplyr::funs(sum)) %>% 
    dplyr::ungroup() %>%
    as.data.frame()
  
  # get # OBSERVABLE compounds per reaction subject to mass filter applied to compoundObj
  obs_comp <- dplyr::inner_join(comp_rxn_mod, compounds, by=c('Compound'='COMPOUND'))
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
  
  obs_comp <- dplyr::group_by(obs_comp, Module_Node_Comb) %>%
    dplyr::summarise(`N_Observable_Compounds`=dplyr::n_distinct(Compound))
  e_meta <- dplyr::left_join(e_meta, obs_comp, by="Module_Node_Comb") %>% as.data.frame()
  e_meta <- dplyr::arrange(e_meta, Module, Module_Node_Comb)
  
  result <- as.moduleData(e_data, compoundObj$f_data, e_meta, edata_cname = "Module_Node_Comb", 
                             fdata_cname=getFDataColName(compoundObj), module_cname="Module", 
                             module_node_cname = "Module_Node", instrument_type = getInstrumentType(compoundObj), 
                             db=getDatabase(compoundObj))

  #attributes from compoundData to carry forward:
#   result <- fticRanalysis:::setDatabase(result, db)
  result <- fticRanalysis:::setGroupDF(result, getGroupDF(compoundObj))
#   result <- fticRanalysis:::setInstrumentType(result, getInstrumentType(compoundData))
  result <- fticRanalysis:::setDataScale(result, "count")
  attr(result, "filters") <- attr(compoundObj, "filters")
  
  ## TODO: are there any other cnames that need to be carried through??
  if (!is.null(attr(compoundObj, "cnames")$extraction_cname)) {
    attr(result, "cnames")$extaction_cname <- attr(compoundObj, "cnames")$extraction_cname
  }

  return(result)
}
