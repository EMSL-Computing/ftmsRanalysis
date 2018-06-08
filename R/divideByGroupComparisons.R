#' Construct a ddo of group comparisons
#' 
#' Construct a \code{\link{ddo}} where each subset consists of data for
#' a pair of groups. This is used to facilitate analysis and visualizations
#' of group comparisons.
#' 
#' @param icrData icrData object
#' @param comparisons dictates which pairwise comparisons to make. 'all' will create a matrix for all pairwise comparisons, 'control' will create a matrix for all comparisons against a specified control group, 'one-factor' will create a matrix of pairwise comparisons to be made where only one 'main_effect' changes between the two groups, or a list of specific comparisons to be made (e.g., list(c("Group1","Group2"),c("Group3","Group4"))) can be given.
#' @param control if wanting to only compare against a control, must specify which group or sample is the control
#' @return a distributed data object where each division consists of a subset of \code{icrData} with
#' data from just two groups.
#' @export
divideByGroupComparisons <- function(icrData, comparisons, control=NULL) {
  if (missing(icrData)) stop("icrData is missing")
  if (missing(comparisons)) stop("comparisons is missing")
  if (!inherits(icrData, "icrData")) stop("icrData must be of type icrData")
#  if (!is.matrix(comparisons) & !is.data.frame(comparisons)) stop("comparisons must be either a matrix or a data frame")
  if (missing(comparisons)) stop("comparisons is missing")
  
  require(datadr)  
  
  fdata.colname <- getFDataColName(icrData)
  
  groupDF <- fticRanalysis:::getGroupDF(icrData)
  if (is.null(groupDF)) {  ## this means each sample is its own group so construct a dummy groupDF 
    samp.names <- unique(icrData$f_data[, getFDataColName(icrData)])
    groupDF <- data.frame(Sample=samp.names, Group=samp.names)
    colnames(groupDF)[1] <- getFDataColName(icrData)
    icrData <- fticRanalysis:::setGroupDF(icrData, groupDF)
  }
  
  compMatrix <- fticRanalysis:::comparisonMatrix(icrData, comparisons, control=control)
  
  groups <- as.character(groupDF$Group)
  samples <- unique(as.character(icrData$f_data[, fdata.colname]))
  edata_nonsample_cols <- setdiff(colnames(icrData$e_data), groups)
  
  result <- lapply(1:ncol(compMatrix), function(i) {
    grp.names <- compMatrix[,i]

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