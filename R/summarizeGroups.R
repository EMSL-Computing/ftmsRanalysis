#' Summarize samples to group level estimates
#' 
#' Summarize sample data by group using a specified summary function. The summary
#' function provided must take a data frame with one or more columns corresponding to 
#' samples. It must return a data frame with the same number of rows and one column. 
#' 
#' @param icrObj an object of type icrData that has only columns of e_data corresponding to the samples in
#' a group (e.g. the output of \code{\link{subset.icrData}} or a ddo of icrData objects (e.g. the output of \code{\link{divideByGroup}})
#' @param summary_functions list of summary functions to apply to each row of icrObj$e_data for each group. If the
#' list has names, those names will be used to name the resulting columns in the result.
#' @export
summarizeGroups <- function(icrObj, summary_functions) {
  require(datadr)
  if (!(inherits(icrObj, "peakIcrData") | !inherits(icrObj, "compoundIcrData")) & !inherits(icrObj, "ddo") )
      stop("icrObj must be of type peakIcrData, compoundIcrData, or a ddo containing those objects")
  if (missing(summary_functions)) stop("summary_function must be provided")
  if (!is.list(summary_functions)) stop("summary_function must be a list")
  
  # if summary_functions has any missing names, fill them in so they can be used to name output columns
  if (is.null(names(summary_functions))) {
    names(summary_functions) <- paste0("Summary", 1:length(summary_functions))
  } else if (any(is.na(names(summary_functions))) | any(nchar(names(summary_functions)) == 0)) {
    ind <- which(is.na(names(summary_functions)) | nchar(names(summary_functions)) == 0)
    names(summary_functions)[ind] <- paste0("Summary", ind)
  }
  
  if (inherits(icrObj, "ddo")) {
    ## do something
    res <- drPersist(addTransform(icrObj, function(v) {
      fticRanalysis:::.summarizeGroupsInternal(v, summary_functions)
    }))
  } else {
    res <- .summarizeGroupsInternal(icrObj, summary_functions)
  }
  return(res)
}
  
# Internal only function for use on 1 icrData object not a ddo of them
.summarizeGroupsInternal <- function(icrObj, summary_functions) {

  samp_cols <- setdiff(colnames(icrObj$e_data), getEDataColName(icrObj))
  edata_cols <- lapply(summary_functions, function(f) {
    f(icrObj$e_data[,samp_cols])
  })
  new_edata <- data.frame(icrObj$e_data[, getEDataColName(icrObj)], edata_cols)
  colnames(new_edata)[1] <- getEDataColName(icrObj)

  new_fdata <- data.frame(Group.Summary.Column=names(edata_cols), Num.Samples=length(samp_cols), 
                             Summary.Function.Name=NA, stringsAsFactors = FALSE)
  
  if (inherits(icrObj, "peakIcrData")) {
    res <- as.peakIcrData(new_edata, new_fdata, icrObj$e_meta, getEDataColName(icrObj), "Group.Summary.Column", getMassColName(icrObj), mf_cname=getMFColName(icrObj) )
  } else if (inherits(icrObj, "compoundIcrData")) {
      res <- as.compoundIcrData(new_edata, new_fdata, icrObj$e_meta, getEDataColName(icrObj), "Group.Summary.Column", getMassColName(icrObj), getCompoundColName(icrObj) )
  # } else if (inherits(icrObj, "reactionIcrData")) {
  #   res <- as.reactionIcrData(new.edata, new.fdata, icrObj$e_meta, getEDataColName(icrObj), "Group.Summary.Column", getReactionColName(icrObj))
  # } else if (inherits(icrObj, "moduleIcrData")) {
  #   res <- as.moduleIcrData(new.edata, new.fdata, icrObj$e_meta, getEDataColName(icrObj), "Group.Summary.Column", getModuleColName(icrObj),
  #                           getModuleNodeColName(icrObj))
  }

  # copy other attributes to new object
  cnames.new <- attr(res, "cnames")
  cnames.old <- attr(icrObj, "cnames")
  for (cc in setdiff(names(cnames.old), c("edata_cname", "fdata_cname", "mass_cname", "mf_cname", "compound_cname"))) {
    if (!is.null(cnames.old[[cc]]))
      cnames.new[[cc]] <- cnames.old[[cc]]
  }
  attr(res, "group_DF") <- attr(icrObj, "group_DF")
  attr(res, "cnames") <- cnames.new
  attr(res, "filters") <- attr(icrObj, "filters")
  attr(res, "instrument_type") <- attr(icrObj, "instrument_type")

  # set class to include 'groupSummary'
  class(res) <- c("groupSummary", class(res))
  
  return(res)
}