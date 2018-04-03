#' Subset an icrData object according to sample or group
#' 
#' Constructs a new icrData object that is a subset of the original
#' with only the specified samples or group. Exactly one of \code{samples}
#' or \code{group} must be specified.
#'
#' @param icrDataObj icrData object
#' @param samples sample ID or vector of sample IDs
#' @param groups group name or vector of group names
#' @return icrData object that contains only the subset of the original
#'         data related to the samples or groups provided
#' @rdname subset
#' @export
subset.peakIcrData <- function(icrDataObj, samples=NA, groups=NA) {
  if (!inherits(icrDataObj, "peakIcrData")) {
    stop("icrDataObj must be of type peakIcrData")
  }
  
  result <- .subset.icrData.internal(icrDataObj, samples=samples, groups=groups)
  attr(result, "class") <- class(icrDataObj)
  attr(result, "cnames") <- attr(icrDataObj, "cnames")
  attr(result, "DB") <- attr(icrDataObj, "DB")
  attr(result, "filters") <- attr(icrDataObj, "filters")
  attr(result, "instrument_type") <- attr(icrDataObj, "instrument_type")
  
  return(result)
}

# Internal only function that performs the bulk of the subsetting operation,
# leaving the attributes of the result to be filled in by the individual
# subset.*IcrData functions.
.subset.icrData.internal <- function(icrDataObj, samples=NA, groups=NA) {
  
  group_DF <- attr(icrDataObj, "group_DF")
  
  if (!all(is.na(samples))) {
    ind <- icrDataObj$f_data[, getFDataColName(icrDataObj)] %in% samples
    fdata <- icrDataObj$f_data[ind, ]
    ss <- as.character(fdata[,getFDataColName(icrDataObj)])
  } else if (!all(is.na(groups))) {
    if (!is.null(group_DF)) {
      ind <- group_DF[, "Group"] %in% groups
      group_DF <- group_DF[ind, ]
      ss <- as.character(group_DF[, getFDataColName(icrDataObj)])
      
      ind <- icrDataObj$f_data[, getFDataColName(icrDataObj)] %in% ss
      fdata <- icrDataObj$f_data[ind, ]
    } else {
      stop("This object does not have a group specification attribute, cannot subset by groups")
    }
  } else {
    stop("Must specify exactly one of 'samples' or 'groups'")
  }
  
  edata <- icrDataObj$e_data[, c(getEDataColName(icrDataObj), ss)]
  emeta <- icrDataObj$e_meta
  result <- list(e_data=edata, f_data=fdata, e_meta=emeta)
  
  # subset group_DF if applicable
  if (!is.null(group_DF)) {
    ind2 <- group_DF[, getFDataColName(icrDataObj)] %in% ss
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
