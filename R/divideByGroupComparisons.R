divideByGroupComparisons <- function(icrObj, comparisons) {
  if (missing(icrObj)) stop("icrObj is missing")
  if (missing(comparisons)) stop("comparisons is missing")
  if (!inherits(icrObj, "icrData")) stop("icrObj must be of type icrData")
  if (!is.matrix(comparisons) & !is.data.frame(comparisons)) stop("comparisons must be either a matrix or a data frame")
  
  require(datadr)  
  
  fdata.colname <- getFDataColName(icrObj)
  groups <- as.character(icrObj$f_data[, fdata.colname])
  edata_nonsample_cols <- setdiff(colnames(icrObj$e_data), groups)
  
  result <- lapply(1:ncol(comparisons), function(i) {
    grp.names <- comparisons[,i]
    
    f_data <- dplyr::rename(icrObj$f_data, tmp9r038519=UQ(fdata.colname)) %>%
      dplyr::filter(tmp9r038519 %in% grp.names)
    colnames(f_data)[colnames(f_data) == "tmp9r038519"] <- fdata.colname
    
    e_data <- icrObj$e_data[, c(edata_nonsample_cols, grp.names)]
    #e_meta <- icrObj$e_meta
    
    val <- list(e_data=e_data, f_data=f_data)
    # attributes(val) <- attributes(icrObj)

    # datadr attributes:
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