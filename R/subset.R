#' Subset an ftmsData object according to sample or group
#' 
#' Constructs a new ftmsData object that is a subset of the original
#' with only the specified samples or group. Exactly one of \code{samples}
#' or \code{group} must be specified.
#'
#' @param x ftmsData object
#' @param samples sample ID or vector of sample IDs
#' @param groups group name or vector of group names
#' @param check_rows logical indicating whether to remove peaks that have no observations after subsetting
#' @param ... other arguments
#' @return ftmsData object that contains only the subset of the original
#'         data related to the samples or groups provided
#' @rdname subset
#' @export
subset.peakData <- function(x, samples=NA, groups=NA, check_rows=FALSE, ...) {
  if (!inherits(x, "peakData")) {
    stop("ftmsObj must be of type peakData")
  }
  
  result <- .subset.ftmsData.internal(x, samples=samples, groups=groups)
  attr(result, "class") <- class(x)
  attr(result, "cnames") <- attr(x, "cnames")
  attr(result, "DB") <- attr(x, "DB")
  
  if(check_rows){
    molfilt <- molecule_filter(result)
    if(any(molfilt$Num_Observations == 0)) result <- applyFilt(molfilt, result, min_num = 1)
  }
  
  attr(result, "filters") <- attr(x, "filters")
  attr(result, "data_info") <- attr(x, "data_info")
  attr(result, "instrument_type") <- attr(x, "instrument_type")
  
  return(result)
}

#' @rdname subset
#' @export
subset.compoundData <- function(x, samples=NA, groups=NA, check_rows=FALSE, ...) {
  if (!inherits(x, "compoundData")) {
    stop("ftmsObj must be of type compoundData")
  }
  
  result <- .subset.ftmsData.internal(x, samples=samples, groups=groups)
  attr(result, "class") <- class(x)
  attr(result, "cnames") <- attr(x, "cnames")
  attr(result, "DB") <- attr(x, "DB")
  
  if(check_rows){
    molfilt <- molecule_filter(result)
    if(any(molfilt$Num_Observations == 0)) result <- applyFilt(molfilt, result, min_num = 1)
  }
  attr(result, "filters") <- attr(x, "filters")
  
  attr(result, "data_info") <- attr(x, "data_info")
  attr(result, "instrument_type") <- attr(x, "instrument_type")
  
  return(result)
}

#' @rdname subset
#' @export
subset.reactionData <- function(x, samples=NA, groups=NA, check_rows=FALSE, ...) {
  if (!inherits(x, "reactionData")) {
    stop("ftmsObj must be of type reactionData")
  }
  
  result <- .subset.ftmsData.internal(x, samples=samples, groups=groups)
  attr(result, "class") <- class(x)
  attr(result, "cnames") <- attr(x, "cnames")
  attr(result, "DB") <- attr(x, "DB")
  
  if(check_rows){
    molfilt <- molecule_filter(result)
    if(any(molfilt$Num_Observations == 0)) result <- applyFilt(molfilt, result, min_num = 1)
  }
  attr(result, "filters") <- attr(x, "filters")
  
  attr(result, "data_info") <- attr(x, "data_info")
  attr(result, "instrument_type") <- attr(x, "instrument_type")
  
  return(result)
}

#' @rdname subset
#' @export
subset.moduleData <- function(x, samples=NA, groups=NA, check_rows=FALSE, ...) {
  if (!inherits(x, "moduleData")) {
    stop("ftmsObj must be of type 'moduleData'")
  }
  
  result <- .subset.ftmsData.internal(x, samples=samples, groups=groups)
  attr(result, "class") <- class(x)
  attr(result, "cnames") <- attr(x, "cnames")
  attr(result, "DB") <- attr(x, "DB")
  
  if(check_rows){
    molfilt <- molecule_filter(result)
    if(any(molfilt$Num_Observations == 0)) result <- applyFilt(molfilt, result, min_num = 1)
  }
  attr(result, "filters") <- attr(x, "filters")
  
  attr(result, "data_info") <- attr(x, "data_info")
  attr(result, "instrument_type") <- attr(x, "instrument_type")
  
  return(result)
}

# Internal only function that performs the bulk of the subsetting operation,
# leaving the attributes of the result to be filled in by the individual
# subset.*Data functions.
.subset.ftmsData.internal <- function(ftmsObj, samples=NA, groups=NA) {
  
  group_DF <- attr(ftmsObj, "group_DF")
  
  if (!all(is.na(samples))) {
    ind <- ftmsObj$f_data[, getFDataColName(ftmsObj)] %in% samples
    fdata <- ftmsObj$f_data[ind, ]
    ss <- as.character(fdata[,getFDataColName(ftmsObj)])
  } else if (!all(is.na(groups))) {
    if (!is.null(group_DF)) {
      ind <- group_DF[, "Group"] %in% groups
      group_DF <- group_DF[ind, ]
      ss <- as.character(group_DF[, getFDataColName(ftmsObj)])
      
      ind <- ftmsObj$f_data[, getFDataColName(ftmsObj)] %in% ss
      fdata <- ftmsObj$f_data[ind, ]
    } else {
      stop("This object does not have a group specification attribute, cannot subset by groups")
    }
  } else {
    stop("Must specify exactly one of 'samples' or 'groups'")
  }
  
  edata <- ftmsObj$e_data[, c(getEDataColName(ftmsObj), ss)]
  emeta <- ftmsObj$e_meta
  result <- list(e_data=edata, f_data=fdata, e_meta=emeta)
  
  # subset group_DF if applicable
  if (!is.null(group_DF)) {
    ind2 <- group_DF[, getFDataColName(ftmsObj)] %in% ss
    group_DF <- group_DF[ind2, ]
    attr(result, "group_DF") <- group_DF
  }
  
  return(result)
}

