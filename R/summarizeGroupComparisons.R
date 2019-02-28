#' Summarize group comparisons
#' 
#' Summarize a group comparisons object or a ddo of group comparisons objects. This function
#' applies a summary function to the columns of \code{compIcrData$e_data} corresponding to each
#' column to calculate a summary column for each group. 
#' 
#' Currently this function does not allow executing the same summary function multiple times
#' with different parameters.
#'
#' @param compIcrData a groupComparison object or a ddo of groupComparison objects, i.e. the output 
#' of \code{\link{divideByGroupComparisons}}.
#' @param summary_functions vector of summary function names to apply to each row of \code{icrData$e_data} for each group. Valid
#' summary function names are given by \code{\link{getGroupComparisonSummaryFunctionNames}}. 
#' @param summary_function_params named list of list of other parameters to pass to the summary functions. Names should
#' match values in \code{summary_functions}, each value should be a list of name/value parameters, e.g.
#' \code{list(uniqueness_gtest=list(pval_threshold=0.01))}.
#'
#' @return a comparisonSummary object or a ddo of comparisonSummary objects
#' @export
summarizeGroupComparisons <- function(compIcrData, summary_functions, summary_function_params=NULL) {
  if (missing(compIcrData)) stop("compIcrData is missing")
  if (missing(compIcrData)) stop("summary_functions is missing")
  #if (length(summary_functions) != 1) stop("summary_functions must have length 1")
  
  if (!(inherits(compIcrData, "groupComparison") | inherits(compIcrData, "ddo") ) )
    stop("compIcrData must be of type groupComparison or a ddo containing groupComparisons")
  
  if (!is.null(summary_function_params)) {
    if (!is.list(summary_function_params)) {
      stop("summary_function_params must be a list")
    }
    if (!all(names(summary_function_params) %in% summary_functions)) {
      stop("all names(summary_function_params) must appear in summary_functions")
    }
  }
  
  if (inherits(compIcrData, "ddo")) {
    res <- drPersist(addTransform(compIcrData, function(v) {
      fticRanalysis:::.summarizeGroupComparisonsInternal(v, summary_functions, summary_function_params)
    }))
  } else {
    res <- .summarizeGroupComparisonsInternal(compIcrData, summary_functions, summary_function_params)
  }
  return(res)
  
}

getGroupComparisonSummaryFunctionNames <- function() {
  return(c("uniqueness_gtest", "uniqueness_nsamps", "uniqueness_prop"))
}

.summarizeGroupComparisonsInternal <- function(compIcrData, summary_functions, summary_function_params=NULL) {
  
  # Get function objects from names
  summary_func_names <- as.vector(unlist(summary_functions))
  validNames <- getGroupComparisonSummaryFunctionNames()
  summary_functions <- lapply(summary_functions, function(nn) {
    nn <- as.character(nn)
    if (!(nn %in% validNames)) stop(sprintf("'%s' is not a valid function name, see getGroupSummaryFunctionNames() for valid options", nn))
    return(get(nn, envir=asNamespace("fticRanalysis"), mode="function"))
  })
  names(summary_functions) <- summary_func_names
  
  groupDF <- getGroupDF(compIcrData)
  data_scale <- getDataScale(compIcrData)

  # for each group of sample columns, apply all summary functions and recombine columns
  edata_cols <- lapply(summary_func_names, function(fname) {
    parms <- list(edata_df=dplyr::select(compIcrData$e_data, -dplyr::matches(getEDataColName(compIcrData))),
                  group_df=dplyr::select(groupDF, dplyr::one_of("Group", getFDataColName(compIcrData))), 
                  data_scale=data_scale)
    names(parms)<- NULL
    if (!is.null(summary_function_params[[fname]])) {
      parms <- c(parms, summary_function_params[[fname]])
    }
    # tmp_result <- f(dplyr::select(compIcrData$e_data, -dplyr::matches(getEDataColName(compIcrData))),
    #   dplyr::select(groupDF, dplyr::one_of("Group", getFDataColName(compIcrData))),
    #   data_scale)
    tmp_result <- do.call(summary_functions[[fname]], parms)
    if (is.null(summary_function_params[[fname]])) {
      summary_params <- NA
    } else {
      summary_params <- list(summary_function_params[[fname]])
    }
    tmp_fdata <- tibble::tibble(Comparison_Summary_Column=colnames(tmp_result), 
                            Summary_Function_Name=fname, 
                            Parameters=summary_params)
    attr(tmp_result, "f_data") <- tmp_fdata
    return(tmp_result)
  })

  new_fdata <- do.call(rbind, lapply(edata_cols, function(x) attr(x, "f_data")))
  new_edata <- data.frame(compIcrData$e_data[, getEDataColName(compIcrData)], do.call(cbind, edata_cols))
  colnames(new_edata)[1] <- getEDataColName(compIcrData)
  
  if (inherits(compIcrData, "peakData")) {
    res <- as.peakData(new_edata, new_fdata, compIcrData$e_meta, getEDataColName(compIcrData), "Comparison_Summary_Column", 
                          getMassColName(compIcrData), mf_cname=getMFColName(compIcrData), instrument_type=getInstrumentType(compIcrData) )
  } else if (inherits(compIcrData, "compoundData")) {
    res <- as.compoundData(new_edata, new_fdata, compIcrData$e_meta, getEDataColName(compIcrData), "Comparison_Summary_Column", 
                              mass_cname=getMassColName(compIcrData), getCompoundColName(compIcrData), instrument_type=getInstrumentType(compIcrData) )
  } else if (inherits(compIcrData, "reactionData")) {
    res <- as.reactionData(new_edata, new_fdata, compIcrData$e_meta, getEDataColName(compIcrData), "Comparison_Summary_Column", 
                              getReactionColName(compIcrData), instrument_type=getInstrumentType(compIcrData), db=getDatabase(compIcrData) )
  } else if (inherits(compIcrData, "moduleData")) {
    res <- as.moduleData(new_edata, new_fdata, compIcrData$e_meta, getEDataColName(compIcrData), "Comparison_Summary_Column", 
                            getModuleColName(compIcrData), getModuleNodeColName(compIcrData), 
                            instrument_type=getInstrumentType(compIcrData) )
  }
  
  # copy other attributes to new object
  cnames.new <- attr(res, "cnames")
  cnames.old <- attr(compIcrData, "cnames")
  for (cc in setdiff(names(cnames.old), c("edata_cname", "fdata_cname", "mass_cname", "mf_cname", "compound_cname"))) {
    if (!is.null(cnames.old[[cc]]))
      cnames.new[[cc]] <- cnames.old[[cc]]
  }
  attr(res, "cnames") <- cnames.new
  
  # set class to include 'comparisonSummary'
  class(res) <- c("comparisonSummary", setdiff(class(res), "groupComparison"))
  
  # copy other attributes
  diffAttrNames <- c("cnames", "class", "names", "split")  #attribute names that should not be the same in the result object
  for (attr_name in setdiff(names(attributes(compIcrData)), diffAttrNames)) {
    attr(res, attr_name) <- attr(compIcrData, attr_name)
  }
  
  res <- fticRanalysis:::setDataScale(res, "summary")
  if (!is.null(getDatabase(compIcrData))) {
    res <- fticRanalysis:::setDatabase(res, getDatabase(compIcrData))
  }
  
  return(res)
}