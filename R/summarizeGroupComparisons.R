#' Summarize group comparisons
#' 
#' Summarize a group comparisons object or a ddo of group comparisons objects. This function
#' applies a summary function to the columns of \code{compData$e_data} corresponding to each
#' column to calculate a summary column for each group. 
#' 
#' Currently this function does not allow executing the same summary function multiple times
#' with different parameters.
#'
#' @param compData a groupComparison object or a ddo of groupComparison objects, i.e. the output 
#' of \code{\link{divideByGroupComparisons}}.
#' @param summary_functions vector of summary function names to apply to each row of \code{ftmsObj$e_data} for each group. Valid
#' summary function names are given by \code{\link{getGroupComparisonSummaryFunctionNames}}. 
#' @param summary_function_params named list of list of other parameters to pass to the summary functions. Names should
#' match values in \code{summary_functions}, each value should be a list of name/value parameters, e.g.
#' \code{list(uniqueness_gtest=list(pval_threshold=0.01))}.
#'
#' @return a comparisonSummary object or a ddo of comparisonSummary objects
#' @export
summarizeGroupComparisons <- function(compData, summary_functions, summary_function_params=NULL) {
  if (missing(compData)) stop("compData is missing")
  if (missing(compData)) stop("summary_functions is missing")
  #if (length(summary_functions) != 1) stop("summary_functions must have length 1")
  
  if (!(inherits(compData, "groupComparison") | inherits(compData, "ddo") ) )
    stop("compData must be of type groupComparison or a ddo containing groupComparisons")
  
  if (!is.null(summary_function_params)) {
    if (!is.list(summary_function_params)) {
      stop("summary_function_params must be a list")
    }
    if (!all(names(summary_function_params) %in% summary_functions)) {
      stop("all names(summary_function_params) must appear in summary_functions")
    }
  }
  
  if (inherits(compData, "ddo")) {
    res <- drPersist(addTransform(compData, function(v) {
      ftmsRanalysis:::.summarizeGroupComparisonsInternal(v, summary_functions, summary_function_params)
    }))
  } else {
    res <- .summarizeGroupComparisonsInternal(compData, summary_functions, summary_function_params)
  }
  return(res)
  
}

#' @title Group comparison summary functions
#' @description \code{getGroupComparisonSummaryFunctionNames} returns the names of valid group comparison 
#' summary functions that may be used with the \code{\link{summarizeGroups}} function.
#' @export
getGroupComparisonSummaryFunctionNames <- function() {
  return(c("uniqueness_gtest", "uniqueness_nsamps", "uniqueness_prop"))
}

.summarizeGroupComparisonsInternal <- function(compData, summary_functions, summary_function_params=NULL) {
  
  # Get function objects from names
  summary_func_names <- as.vector(unlist(summary_functions))
  validNames <- getGroupComparisonSummaryFunctionNames()
  summary_functions <- lapply(summary_functions, function(nn) {
    nn <- as.character(nn)
    if (!(nn %in% validNames)) stop(sprintf("'%s' is not a valid function name, see getGroupSummaryFunctionNames() for valid options", nn))
    return(get(nn, envir=asNamespace("ftmsRanalysis"), mode="function"))
  })
  names(summary_functions) <- summary_func_names
  
  groupDF <- getGroupDF(compData)
  data_scale <- getDataScale(compData)

  # for each group of sample columns, apply all summary functions and recombine columns
  edata_cols <- lapply(summary_func_names, function(fname) {
    parms <- list(edata_df=dplyr::select(compData$e_data, -dplyr::matches(getEDataColName(compData))),
                  group_df=dplyr::select(groupDF, dplyr::one_of("Group", getFDataColName(compData))), 
                  data_scale=data_scale)
    names(parms)<- NULL
    if (!is.null(summary_function_params[[fname]])) {
      parms <- c(parms, summary_function_params[[fname]])
    }
    # tmp_result <- f(dplyr::select(compData$e_data, -dplyr::matches(getEDataColName(compData))),
    #   dplyr::select(groupDF, dplyr::one_of("Group", getFDataColName(compData))),
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
  new_edata <- data.frame(compData$e_data[, getEDataColName(compData)], do.call(cbind, edata_cols))
  colnames(new_edata)[1] <- getEDataColName(compData)
  
  if (inherits(compData, "peakData")) {
    res <- as.peakData(new_edata, new_fdata, compData$e_meta, getEDataColName(compData), "Comparison_Summary_Column", 
                          getMassColName(compData), mf_cname=getMFColName(compData), instrument_type=getInstrumentType(compData) )
  } else if (inherits(compData, "compoundData")) {
    res <- as.compoundData(new_edata, new_fdata, compData$e_meta, getEDataColName(compData), "Comparison_Summary_Column", 
                              mass_cname=getMassColName(compData), getCompoundColName(compData), instrument_type=getInstrumentType(compData) )
  } else if (inherits(compData, "reactionData")) {
    res <- as.reactionData(new_edata, new_fdata, compData$e_meta, getEDataColName(compData), "Comparison_Summary_Column", 
                              getReactionColName(compData), instrument_type=getInstrumentType(compData), db=getDatabase(compData) )
  } else if (inherits(compData, "moduleData")) {
    res <- as.moduleData(new_edata, new_fdata, compData$e_meta, getEDataColName(compData), "Comparison_Summary_Column", 
                            getModuleColName(compData), getModuleNodeColName(compData), 
                            instrument_type=getInstrumentType(compData) )
  }
  
  # copy other attributes to new object
  cnames.new <- attr(res, "cnames")
  cnames.old <- attr(compData, "cnames")
  for (cc in setdiff(names(cnames.old), c("edata_cname", "fdata_cname", "mass_cname", "mf_cname", "compound_cname"))) {
    if (!is.null(cnames.old[[cc]]))
      cnames.new[[cc]] <- cnames.old[[cc]]
  }
  attr(res, "cnames") <- cnames.new
  
  # set class to include 'comparisonSummary'
  class(res) <- c("comparisonSummary", setdiff(class(res), "groupComparison"))
  
  # copy other attributes
  diffAttrNames <- c("cnames", "class", "names", "split")  #attribute names that should not be the same in the result object
  for (attr_name in setdiff(names(attributes(compData)), diffAttrNames)) {
    attr(res, attr_name) <- attr(compData, attr_name)
  }
  
  res <- ftmsRanalysis:::setDataScale(res, "summary")
  if (!is.null(getDatabase(compData))) {
    res <- ftmsRanalysis:::setDatabase(res, getDatabase(compData))
  }
  
  return(res)
}