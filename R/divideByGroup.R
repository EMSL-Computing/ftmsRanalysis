#' Divide an ftmsData object by group to form a ddo
#' 
#' Construct a \code{\link[datadr:ddo]{ddo}} from an \code{ftmsData} object by dividing
#' by group. The resulting object may be used with Trelliscope to make
#' plots for each group. The input data must have a \code{group_DF} attribute
#' defining the groups.
#' 
#' @param ftmsObj ftmsData object
#' @return a ddo where each division is a subset of \code{ftmsObj} corresponding
#'         to a single group
#' @seealso \code{\link[datadr:ddo]{ddo}}
#' @export
divideByGroup <- function(ftmsObj) {
  require(datadr)
  sample.colname <- getFDataColName(ftmsObj)
  samples <- as.character(ftmsObj$f_data[, sample.colname])
  groupDF <- getGroupDF(ftmsObj)
  if (is.null(groupDF)) stop("This object does not have group designation information")

  edata_nonsample_cols <- setdiff(colnames(ftmsObj$e_data), samples)
  
  result <- lapply(unique(groupDF$Group), function(group_name) {

    val <- subset(ftmsObj, groups=group_name)
    
    # datadr attributes:
    attr(val, "split") <- data.frame(Group=group_name, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- "Group"
    
    key <- paste0("Group=", group_name)
    return(kvPair(key, val))
  })
  
  result <- ddo(result)
  return(result)
}