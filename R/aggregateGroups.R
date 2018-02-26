#' Aggregate samples to group level estimates
#' 
#' Aggregate sample data by group using a specified summary function. The summary
#' function provided must take a data frame with one or more columns corresponding to 
#' samples. It must return a data frame with the same number of rows and one column. 
#' 
#' @param icrObj an objecdt of type icrData
#' @param summary_function summary function to apply to each row of icrObj$e_data for each group
#' @export
aggregateGroups <- function(icrObj, summary_function, ...) {
  if (!inherits(icrObj, "icrData")) stop("icrObj must be of type icrData")
  if (missing(summary_function)) stop("summary_function must be provided")
  if (!is.function(summary_function)) stop("summary_function must be a function")
  
  groupDF <- attr(icrObj, "group_DF")
  if (is.null(groupDF)) {  ## this means each sample is its own group so construct a dummy groupDF 
    samp.names <- unique(icrObj$f_data[, getFDataColName(icrObj)])
    groupDF <- data.frame(Sample=samp.names, Group=samp.names)
    colnames(groupDF)[1] <- getFDataColName(icrObj)
  }
  
  new.edata <- lapply(unique(groupDF$Group), function(gname) {
    cnames <- as.character(dplyr::filter(groupDF, Group == gname)[, getFDataColName(icrObj)])
    return(summary_function(sample_columns=icrObj$e_data[, cnames]), row_ids=icrObj$e_data[, getEDataColName(icrObj)], ...) ## TODO does this need more parameters??
  })
  names(new.edata) <- unique(groupDF$Group)
  new.edata <- do.call(cbind, new.edata)
  new.edata <- data.frame(EDataname=icrObj$e_data[, getEDataColName(icrObj)], new.edata)
  colnames(new.edata)[1] <- getEDataColName(icrObj)
  
  new.fdata <- as.data.frame(dplyr::summarise(dplyr::group_by(groupDF, Group), Num_Samples=n()))
  
  if (inherits(icrObj, "peakIcrData")) {
    res <- as.peakIcrData(new.edata, new.fdata, icrObj$e_meta, getEDataColName(icrObj), "Group", getMassColName(icrObj), mf_cname=getMFColName(icrObj) )
  } else if (inherits(icrObj, "compoundIcrData")) {
      res <- as.compoundIcrData(new.edata, new.fdata, icrObj$e_meta, getEDataColName(icrObj), "Group", getMassColName(icrObj), getCompoundColName(icrObj) )
  } else if (inherits(icrObj, "reactionIcrData")) {
    res <- as.reactionIcrData(new.edata, new.fdata, icrObj$e_meta, getEDataColName(icrObj), "Group", getReactionColName(icrObj))
  } else if (inherits(icrObj, "moduleIcrData")) {
    res <- as.moduleIcrData(new.edata, new.fdata, icrObj$e_meta, getEDataColName(icrObj), "Group", getModuleColName(icrObj),
                            getModuleNodeColName(icrObj))
  }

  # copy other attributes to new object
  cnames.new <- attr(res, "cnames")
  cnames.old <- attr(picr, "cnames")
  for (cc in setdiff(names(cnames.old), c("edata_cname", "fdata_cname", "mass_cname", "mf_cname", "compound_cname"))) {
    if (!is.null(cnames.old[[cc]]))
      cnames.new[[cc]] <- cnames.old[[cc]]
  }
  attr(res, "cnames") <- cnames.new
  attr(res, "filters") <- attr(icrObj, "filters")
  attr(res, "instrument_type") <- attr(icrObj, "instrument_type")

  return
}