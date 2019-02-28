#' Subset an icrData object according to sample or group
#' 
#' Constructs a new icrData object that is a subset of the original
#' with only the specified samples or group. Exactly one of \code{samples}
#' or \code{group} must be specified.
#'
#' @param icrData icrData object
#' @param samples sample ID or vector of sample IDs
#' @param groups group name or vector of group names
#' @param check_rows logical indicating whether to remove peaks that have no observations after subsetting
#' @return icrData object that contains only the subset of the original
#'         data related to the samples or groups provided
#' @rdname subset
#' @export
subset.peakData <- function(icrData, samples=NA, groups=NA, check_rows=FALSE) {
  if (!inherits(icrData, "peakData")) {
    stop("icrData must be of type peakData")
  }
  
  result <- .subset.icrData.internal(icrData, samples=samples, groups=groups)
  attr(result, "class") <- class(icrData)
  attr(result, "cnames") <- attr(icrData, "cnames")
  attr(result, "DB") <- attr(icrData, "DB")
  
  if(check_rows){
    molfilt <- molecule_filter(result)
    if(any(molfilt$Num_Observations == 0)) result <- applyFilt(molfilt, result, min_num = 1)
  }
  
  attr(result, "filters") <- attr(icrData, "filters")
  attr(result, "data_info") <- attr(icrData, "data_info")
  attr(result, "instrument_type") <- attr(icrData, "instrument_type")
  
  return(result)
}

#' @rdname subset
#' @export
subset.compoundData <- function(icrData, samples=NA, groups=NA, check_rows=FALSE) {
  if (!inherits(icrData, "compoundData")) {
    stop("icrData must be of type compoundData")
  }
  
  result <- .subset.icrData.internal(icrData, samples=samples, groups=groups)
  attr(result, "class") <- class(icrData)
  attr(result, "cnames") <- attr(icrData, "cnames")
  attr(result, "DB") <- attr(icrData, "DB")
  
  if(check_rows){
    molfilt <- molecule_filter(result)
    if(any(molfilt$Num_Observations == 0)) result <- applyFilt(molfilt, result, min_num = 1)
  }
  attr(result, "filters") <- attr(icrData, "filters")
  
  attr(result, "data_info") <- attr(icrData, "data_info")
  attr(result, "instrument_type") <- attr(icrData, "instrument_type")
  
  return(result)
}

#' @rdname subset
#' @export
subset.reactionIcrData <- function(icrData, samples=NA, groups=NA, check_rows=FALSE) {
  if (!inherits(icrData, "reactionIcrData")) {
    stop("icrData must be of type reactionIcrData")
  }
  
  result <- .subset.icrData.internal(icrData, samples=samples, groups=groups)
  attr(result, "class") <- class(icrData)
  attr(result, "cnames") <- attr(icrData, "cnames")
  attr(result, "DB") <- attr(icrData, "DB")
  
  if(check_rows){
    molfilt <- molecule_filter(result)
    if(any(molfilt$Num_Observations == 0)) result <- applyFilt(molfilt, result, min_num = 1)
  }
  attr(result, "filters") <- attr(icrData, "filters")
  
  attr(result, "data_info") <- attr(icrData, "data_info")
  attr(result, "instrument_type") <- attr(icrData, "instrument_type")
  
  return(result)
}

#' @rdname subset
#' @export
subset.moduleIcrData <- function(icrData, samples=NA, groups=NA, check_rows=FALSE) {
  if (!inherits(icrData, "moduleIcrData")) {
    stop("icrData must be of type moduleIcrData")
  }
  
  result <- .subset.icrData.internal(icrData, samples=samples, groups=groups)
  attr(result, "class") <- class(icrData)
  attr(result, "cnames") <- attr(icrData, "cnames")
  attr(result, "DB") <- attr(icrData, "DB")
  
  if(check_rows){
    molfilt <- molecule_filter(result)
    if(any(molfilt$Num_Observations == 0)) result <- applyFilt(molfilt, result, min_num = 1)
  }
  attr(result, "filters") <- attr(icrData, "filters")
  
  attr(result, "data_info") <- attr(icrData, "data_info")
  attr(result, "instrument_type") <- attr(icrData, "instrument_type")
  
  return(result)
}

# Internal only function that performs the bulk of the subsetting operation,
# leaving the attributes of the result to be filled in by the individual
# subset.*IcrData functions.
.subset.icrData.internal <- function(icrData, samples=NA, groups=NA) {
  
  group_DF <- attr(icrData, "group_DF")
  
  if (!all(is.na(samples))) {
    ind <- icrData$f_data[, getFDataColName(icrData)] %in% samples
    fdata <- icrData$f_data[ind, ]
    ss <- as.character(fdata[,getFDataColName(icrData)])
  } else if (!all(is.na(groups))) {
    if (!is.null(group_DF)) {
      ind <- group_DF[, "Group"] %in% groups
      group_DF <- group_DF[ind, ]
      ss <- as.character(group_DF[, getFDataColName(icrData)])
      
      ind <- icrData$f_data[, getFDataColName(icrData)] %in% ss
      fdata <- icrData$f_data[ind, ]
    } else {
      stop("This object does not have a group specification attribute, cannot subset by groups")
    }
  } else {
    stop("Must specify exactly one of 'samples' or 'groups'")
  }
  
  edata <- icrData$e_data[, c(getEDataColName(icrData), ss)]
  emeta <- icrData$e_meta
  result <- list(e_data=edata, f_data=fdata, e_meta=emeta)
  
  # subset group_DF if applicable
  if (!is.null(group_DF)) {
    ind2 <- group_DF[, getFDataColName(icrData)] %in% ss
    group_DF <- group_DF[ind2, ]
    attr(result, "group_DF") <- group_DF
  }
  
  return(result)
}



### TESTS ###

# library(icRanalysis)
# data(exIcrData)
# 
# subset1 <- subset(exIcrData, samples="10c_m")
# str(subset1)
# 
# subset2 <- subset(exIcrData, samples=c("10c_m", "10c_w"))
# str(subset2)
# 
# subset3 <- subset(exIcrData, groups="LL")
# str(subset3)
# 
# subset4 <- subset(exIcrData, groups=c("LL", "LN", "LU"))
# str(subset4)
