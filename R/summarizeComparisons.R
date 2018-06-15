#' Summarize group comparisons
#' 
#' Summarize a group comparisons object or a ddo of group comparisons objects. This function
#' applies a summary function to the columns of \code{compIcrData$e_data} corresponding to each
#' column to calculate a summary column for each group.
#'
#' @param compIcrData a groupComparison object or a ddo of groupComparison objects, i.e. the output 
#' of \code{\link{divideByGroupComparisons}}.
#' @param summary_functions vector of summary function names to apply to each row of \code{icrData$e_data} for each group. Valid
#' summary function names are given by \code{\link{getGroupComparisonSummaryFunctionNames}}. 
#'
#' @return a comparisonSummary object or a ddo of comparisonSummary objects
#' @export
summarizeComparisons <- function(compIcrData, summary_functions) {
  if (missing(compIcrData)) stop("compIcrData is missing")
  if (missing(compIcrData)) stop("summary_functions is missing")
  if (length(summary_functions) != 1) stop("summary_functions must have length 1")
  
  if (!(inherits(compIcrData, "groupComparison") | inherits(compIcrData, "ddo") ) )
    stop("compIcrData must be of type groupComparison or a ddo containing groupComparisons")
  
  if (inherits(compIcrData, "ddo")) {
    res <- drPersist(addTransform(compIcrData, function(v) {
      fticRanalysis:::.summarizeComparisonsInternal(v, summary_functions)
    }))
  } else {
    res <- .summarizeComparisonsInternal(compIcrData, summary_functions)
  }
  return(res)
  
}

.summarizeComparisonsInternal <- function(compIcrData, summary_functions) {
  
  # Get function objects from names
  summary_func_names <- as.vector(unlist(summary_functions))
  validNames <- getGroupComparisonSummaryFunctionNames()
  summary_functions <- lapply(summary_functions, function(nn) {
    nn <- as.character(nn)
    if (!(nn %in% validNames)) stop(sprintf("'%s' is not a valid function name, see getGroupSummaryFunctionNames() for valid options", nn))
    return(get(nn, envir=asNamespace("fticRanalysis"), mode="function"))
  })
  
  groupDF <- getGroupDF(compIcrData)
  data_scale <- getDataScale(compIcrData)

  # for each group of sample columns, apply all summary functions and recombine columns
  edata_cols <- lapply(summary_functions, function(f) {
      tmp <- f(dplyr::select(compIcrData$e_data, -dplyr::matches(getEDataColName(compIcrData))),
        dplyr::select(groupDF, dplyr::one_of("Group", getFDataColName(compIcrData))),
        data_scale)
      tmp_fdata <- data.frame(Comparison_Summary_Column=colnames(tmp), 
                              Summary_Function_Name=summary_func_names, stringsAsFactors = FALSE)
      attr(tmp, "f_data") <- tmp_fdata
      return(tmp)
  })

  new_fdata <- do.call(rbind, lapply(edata_cols, function(x) attr(x, "f_data")))
  new_edata <- data.frame(compIcrData$e_data[, getEDataColName(compIcrData)], do.call(cbind, edata_cols))
  colnames(new_edata)[1] <- getEDataColName(compIcrData)
    
  if (inherits(compIcrData, "peakIcrData")) {
    res <- as.peakIcrData(new_edata, new_fdata, compIcrData$e_meta, getEDataColName(compIcrData), "Comparison_Summary_Column", 
                          getMassColName(compIcrData), mf_cname=getMFColName(compIcrData), instrument_type=getInstrumentType(compIcrData) )
  } else if (inherits(compIcrData, "compoundIcrData")) {
    res <- as.compoundIcrData(new_edata, new_fdata, compIcrData$e_meta, getEDataColName(compIcrData), "Comparison_Summary_Column", 
                              getMassColName(compIcrData), getCompoundColName(compIcrData), instrument_type=getInstrumentType(compIcrData) )
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
    res <- fticRanalysis:::setDatabase(res, db)
  }
  
  return(res)
}