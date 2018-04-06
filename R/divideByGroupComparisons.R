divideByGroupComparisons <- function(icrObj, comparisons) {
  if (missing(icrObj)) stop("icrObj is missing")
  if (missing(comparisons)) stop("comparisons is missing")
  if (!inherits(icrObj, "icrData")) stop("icrObj must be of type icrData")
  if (!is.matrix(comparisons) & !is.data.frame(comparisons)) stop("comparisons must be either a matrix or a data frame")
  
  require(datadr)  
  
  fdata.colname <- getFDataColName(icrObj)
  
  groupDF <- fticRanalysis:::getGroupDF(icrObj)
  if (is.null(groupDF)) {  ## this means each sample is its own group so construct a dummy groupDF 
    samp.names <- unique(icrObj$f_data[, getFDataColName(icrObj)])
    groupDF <- data.frame(Sample=samp.names, Group=samp.names)
    colnames(groupDF)[1] <- getFDataColName(icrObj)
  }
  
  groups <- as.character(groupDF$Group)
  samples <- unique(as.character(icrObj$f_data[, fdata.colname]))
  edata_nonsample_cols <- setdiff(colnames(icrObj$e_data), groups)
  
  result <- lapply(1:ncol(comparisons), function(i) {
    grp.names <- comparisons[,i]

    val <- subset(icrObj, groups = grp.names)    

    comp_name <- paste(grp.names, collapse=" vs ")
    attr(val, "split") <- data.frame(Group_Comparison=comp_name, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- "Group_Comparison"
    
    key <- paste0("Group_Comparison=", comp_name)
    return(kvPair(key, val))
  })
  result <- ddo(result)
  attr(result, "e_meta") <- icrObj$e_meta
  return(result)
}