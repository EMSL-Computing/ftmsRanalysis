#' Divide icrData by group to form a ddo
#' 
#' Construct a \code{\link{ddo}} from an \code{icrData} object by dividing
#' by group. The resulting object may be used with Trelliscope to make
#' plots for each group. The input data must have a \code{group_DF} attribute
#' defining the groups.
#' 
#' @param icrData icrData object
#' @return a ddo where each division is a subset of \code{icrData} corresponding
#'         to a single group
#' @seealso \code{\link{ddo}}
#' @export
divideByGroup <- function(icrData) {
  require(datadr)
  sample.colname <- getFDataColName(icrData)
  samples <- as.character(icrData$f_data[, sample.colname])
  groupDF <- getGroupDF(icrData)
  if (is.null(groupDF)) stop("This object does not have group designation information")

  edata_nonsample_cols <- setdiff(colnames(icrData$e_data), samples)
  
  result <- lapply(unique(groupDF$Group), function(group_name) {

    val <- subset(icrData, groups=group_name)
    
    # datadr attributes:
    attr(val, "split") <- data.frame(Group=group_name, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- "Group"
    
    key <- paste0("Group=", group_name)
    return(kvPair(key, val))
  })
  
  result <- ddo(result)
  return(result)
}