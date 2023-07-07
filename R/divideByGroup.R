#' Divide an ftmsData object by group to form a list of ftmsData objects
#' 
#' Construct a list of subsetted ftmsData objects from an \code{ftmsData} object by dividing
#' by group. The resulting object may be used with Trelliscope to make
#' plots for each group. The input data must have a \code{group_DF} attribute
#' defining the groups.
#' 
#' @param ftmsObj ftmsData object
#' @return a list where each element is a subset of \code{ftmsObj} corresponding
#'         to a single group
#' @export
divideByGroup <- function(ftmsObj) {
  sample.colname <- getFDataColName(ftmsObj)
  samples <- as.character(ftmsObj$f_data[, sample.colname])
  groupDF <- getGroupDF(ftmsObj)
  if (is.null(groupDF)) stop("This object does not have group designation information")
  
  edata_nonsample_cols <- setdiff(colnames(ftmsObj$e_data), samples)
  
  result <- lapply(unique(groupDF$Group), function(group_name) {
    
    val <- subset(ftmsObj, groups=group_name)
    
    attr(val, "split") <- data.frame(Group=group_name, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- "Group"
    
    return(val)
  })
  
  result_names <- lapply(unique(groupDF$Group), function(group_name) {
    paste0("Group=", group_name)
  })
  names(result) <- result_names
  
  return(result)
}