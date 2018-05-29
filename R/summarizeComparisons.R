#' Summarize group comparisons
#' 
#' Summarize a group comparisons object or a ddo of group comparisons objects. This function
#' applies a summary function to the columns of \code{compIcrData$e_data} corresponding to each
#' column to calculate a summary column for each group.
#'
#' @param compIcrData a groupComparison object or a ddo of groupComparison objects, i.e. the output 
#' of \code{\link{divideByGroupComparisons}}.
#' @param summary_function name of a summary function, e.g. "n_present"
#'
#' @return a comparisonSummary object or a ddo of comparisonSummary objects
#' @export
summarizeComparisons <- function(compIcrData, summary_function) {
  if (missing(compIcrData)) stop("compIcrData is missing")
  if (missing(compIcrData)) stop("summary_function is missing")
  if (length(summary_function) != 1) stop("summary_function must have length 1")
  
  if (!(inherits(compIcrData, "groupComparison") | inherits(compIcrData, "ddo") ) )
    stop("compIcrData must be of type groupComparison or a ddo containing groupComparisons")
  
  validSummaryFunctions <- "n_present" ## add to this when applicable
  if (! (summary_function %in% validSummaryFunctions )) 
    stop(paste0("that is not a valid summary function, allowed values are ", paste0(validSummaryFunctions, collapse=", ")))
  
  if (inherits(compIcrData, "ddo")) {
    res <- drPersist(addTransform(compIcrData, function(v) {
      fticRanalysis:::.summarizeComparisonsInternal(v, summary_function)
    }))
  } else {
    res <- .summarizeComparisonsInternal(compIcrData, summary_function)
  }
  return(res)
  
}

.summarizeComparisonsInternal <- function(compIcrData, summary_function) {
  
  # Get function object from name
  summary_function_obj <- get(as.character(summary_function), envir=asNamespace("fticRanalysis"), mode="function")
  
  groupDF <- getGroupDF(compIcrData)
  data_scale <- getDataScale(compIcrData)
  groups <- unique(groupDF$Group)
  edata_cols <- lapply(groups, function(grp) {
    samp_cols <- as.character(groupDF[, getFDataColName(compIcrData)][groupDF$Group == grp])
    summary_function_obj(compIcrData$e_data[,samp_cols], data_scale)
  })
  names(edata_cols) <- groups
  new_edata <- data.frame(compIcrData$e_data[, getEDataColName(compIcrData)], edata_cols)
  colnames(new_edata)[1] <- getEDataColName(compIcrData)
  
  new_fdata <- data.frame(Group=names(edata_cols), 
                          Num.Samples=unlist(lapply(groups, function(grp) sum(groupDF$Group == grp))), 
                          Summary.Function.Name=summary_function, stringsAsFactors = FALSE)
  
  if (inherits(compIcrData, "peakIcrData")) {
    res <- as.peakIcrData(new_edata, new_fdata, compIcrData$e_meta, getEDataColName(compIcrData), "Group", 
                          getMassColName(compIcrData), mf_cname=getMFColName(compIcrData), instrument_type=getInstrumentType(compIcrData) )
  } else if (inherits(compIcrData, "compoundIcrData")) {
    res <- as.compoundIcrData(new_edata, new_fdata, compIcrData$e_meta, getEDataColName(compIcrData), "Group", 
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