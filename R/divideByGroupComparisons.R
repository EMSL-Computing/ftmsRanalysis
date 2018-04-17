#' Construct a ddo of group comparisons
#' 
#' Construct a \code{\link{ddo}} where each subset consists of data for
#' a pair of groups. This is used to facilitate analysis and visualizations
#' of group comparisons.
#' 
#' @param icrData icrData object
#' @param comparisons comparisons matrix, i.e. the output of \code{\link{comparisonMatrix}}
#' @return a distributed data object where each division consists of a subset of \code{icrData} with
#' data from just two groups, as defined in the \code{comparisons} matrix.
#' @export
divideByGroupComparisons <- function(icrData, comparisons) {
  if (missing(icrData)) stop("icrData is missing")
  if (missing(comparisons)) stop("comparisons is missing")
  if (!inherits(icrData, "icrData")) stop("icrData must be of type icrData")
  if (!is.matrix(comparisons) & !is.data.frame(comparisons)) stop("comparisons must be either a matrix or a data frame")
  
  require(datadr)  
  
  fdata.colname <- getFDataColName(icrData)
  
  groupDF <- fticRanalysis:::getGroupDF(icrData)
  if (is.null(groupDF)) {  ## this means each sample is its own group so construct a dummy groupDF 
    samp.names <- unique(icrData$f_data[, getFDataColName(icrData)])
    groupDF <- data.frame(Sample=samp.names, Group=samp.names)
    colnames(groupDF)[1] <- getFDataColName(icrData)
  }
  
  groups <- as.character(groupDF$Group)
  samples <- unique(as.character(icrData$f_data[, fdata.colname]))
  edata_nonsample_cols <- setdiff(colnames(icrData$e_data), groups)
  
  result <- lapply(1:ncol(comparisons), function(i) {
    grp.names <- comparisons[,i]

    val <- subset(icrData, groups = grp.names)  
    class(val) <- c("groupComparison", class(val))

    comp_name <- paste(grp.names, collapse=" vs ")
    attr(val, "split") <- data.frame(Group_Comparison=comp_name, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- "Group_Comparison"
    
    key <- paste0("Group_Comparison=", comp_name)
    return(kvPair(key, val))
  })
  result <- ddo(result)
  attr(result, "e_meta") <- icrData$e_meta
  return(result)
}