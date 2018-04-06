#' Generate a matrix of comparisons
#' 
#' Create a matrix of group or sample comparisons as input to \code{\link{divideByGroupComparisons}}.
#' 
#' @param icrObj icrData object
#' @param comparisons one of "all", "control" or a list containing two-element vectors of groups to compare
#' @param control the name of the control group, if \code{comparisons="control"}
#' @export 
comparisonMatrix <- function(icrObj, comparisons, control=NULL) {
  if (missing(icrObj)) stop("icrObj is missing")
  if (missing(comparisons)) stop("comparisons is missing")
  if (!inherits(icrObj, "icrData")) stop("icrObj must be of type icrData")
  
  groupDF <- getGroupDF(icrObj)
  if (is.null(groupDF)) {  ## this means each sample is its own group so construct a dummy groupDF 
    samp.names <- unique(icrObj$f_data[, getFDataColName(icrObj)])
    groupDF <- data.frame(Sample=samp.names, Group=samp.names)
    colnames(groupDF)[1] <- getFDataColName(icrObj)
  }
  
  if (!missing(control)) {
    if (!(control %in% groupDF$Group)) {
      stop(paste0("control column '", control, "' not found in the data"))
    }
  }
  
  # determine which pairwise comparisons to make
  if(tolower(comparisons) == "all"){
    pairs <- combn(levels(groupDF$Group),2)
  }else if(tolower(comparisons) == "control"){
    pairs <- combn(levels(group_DF$Group),2)
    if(ncol(pairs) > 1){
      if(length(unique(c(grep(control,pairs[1,]),grep(control,pairs[2,])))) > 1){
        pairs <- pairs[,unique(c(grep(control, pairs[1,]),grep(control, pairs[2,])))]
      }
      if(any(pairs[1,]==control)){
        pairs[,which(pairs[1,] == control)] <- apply(pairs[,which(pairs[1,] == control)], 2, rev)
      }
    }
  }else if(is.list(comparisons)){
    if (!all(unlist(pairs) %in% groupDF$Group)) {
      stop("not all groups specified in pairs parameter were found in the data")
    }
    
    pairs <- do.call(cbind, comparisons)
  }else{
    stop("check that comparisons argument is either 'all', 'control', or a list of specific comparisons or terms for a paired test")
  }
  
}