#' Construct a list of group comparisons
#' 
#' Construct a list where each element is a subset consisting of data for
#' a pair of groups. This is used to facilitate analysis and visualizations
#' of group comparisons.
#' 
#' @param ftmsObj ftmsData object
#' @param comparisons dictates which pairwise comparisons to make. 'all' will create a matrix for all pairwise comparisons, 'control' will create a matrix for all comparisons against a specified control group, 'one-factor' will create a matrix of pairwise comparisons to be made where only one 'main_effect' changes between the two groups, or a list of specific comparisons to be made (e.g., list(c("Group1","Group2"),c("Group3","Group4"))) can be given.
#' @param control if wanting to only compare against a control, must specify which group or sample is the control
#' @return a named list where each element consists of a subset of \code{ftmsObj} with
#' data from just two groups.
#' @export
divideByGroupComparisons <- function(ftmsObj, comparisons, control=NULL) {
  
  if (missing(ftmsObj)) stop("ftmsObj is missing")
  if (missing(comparisons)) stop("comparisons is missing")
  if (!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be of type ftmsData")
#  if (!is.matrix(comparisons) & !is.data.frame(comparisons)) stop("comparisons must be either a matrix or a data frame")
  if (missing(comparisons)) stop("comparisons is missing")
  
  fdata.colname <- getFDataColName(ftmsObj)
  
  groupDF <- ftmsRanalysis:::getGroupDF(ftmsObj)
  if (is.null(groupDF)) {  ## this means each sample is its own group so construct a dummy groupDF 
    samp.names <- unique(ftmsObj$f_data[, getFDataColName(ftmsObj)])
    groupDF <- data.frame(Sample=samp.names, Group=samp.names)
    colnames(groupDF)[1] <- getFDataColName(ftmsObj)
    ftmsObj <- ftmsRanalysis:::setGroupDF(ftmsObj, groupDF)
  }
  
  compMatrix <- ftmsRanalysis:::comparisonMatrix(ftmsObj, comparisons, control=control)
  
  groups <- as.character(groupDF$Group)
  samples <- unique(as.character(ftmsObj$f_data[, fdata.colname]))
  edata_nonsample_cols <- setdiff(colnames(ftmsObj$e_data), groups)
  
  # lapply over each col in matrix and return list of subset data
  result <- lapply(1:ncol(compMatrix), function(i) {
    grp.names <- compMatrix[,i]

    val <- subset(ftmsObj, groups = grp.names)  
    class(val) <- c("groupComparison", class(val))

    comp_name <- paste(grp.names, collapse=" vs ")
    attr(val, "split") <- data.frame(Group_Comparison=comp_name, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- "Group_Comparison"
    
    return(val)
  })
  
  # Loop over cols of matrix and create vector of names for results above
  result_names <- lapply(1:ncol(compMatrix), function(i) {
    grp.names <- compMatrix[,i]
    comp_name <- paste(grp.names, collapse=" vs ")
    key <- paste0("Group_Comparison=", comp_name)
    return(key)
  })
  
  names(result) <- result_names
  
  attr(result, "e_meta") <- ftmsObj$e_meta
  return(result)
}