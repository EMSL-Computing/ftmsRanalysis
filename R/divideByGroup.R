#' Divide icrData by group to form a ddo
#' 
#' Construct a \code{\link{ddo}} from an \code{icrData} object by dividing
#' by group. The resulting object may be used with Trelliscope to make
#' plots for each group. The input data must have a \code{group_DF} attribute
#' defining the groups.
#' 
#' @param icrDataObj icrData object
#' @return a ddo where each division is a subset of \code{icrDataObj} corresponding
#'         to a single group
#' @seealso \code{\link{ddo}}
#' @export
divideByGroup <- function(icrDataObj) {
  require(datadr)
  sample.colname <- getFDataColName(icrDataObj)
  samples <- as.character(icrDataObj$f_data[, sample.colname])
  groupDF <- getGroupDF(icrDataObj)
  ## TODO working here 1/17/17
  
  edata_nonsample_cols <- setdiff(colnames(icrDataObj$e_data), samples)
  
  result <- lapply(unique(groupDF$Group), function(group_name) {
    group.samples <- as.character(groupDF[groupDF$Group == group_name, "Sample"])
    
    f_data <- dplyr::rename(icrDataObj$f_data, tmp9r038519=UQ(sample.colname)) %>%
      dplyr::filter(tmp9r038519 %in% group.samples)
    colnames(f_data)[colnames(f_data) == "tmp9r038519"] <- sample.colname
    
    e_data <- icrDataObj$e_data[, c(edata_nonsample_cols, group.samples)]
    e_meta <- icrDataObj$e_meta
    
    tmp_group_DF <- dplyr::filter(groupDF, Group == group_name)

    val <- list(e_data=e_data, f_data=f_data, e_meta=e_meta)
    attributes(val) <- attributes(icrDataObj)
    attr(val, "group_DF") <- tmp_group_DF

    # datadr attributes:
    attr(val, "split") <- data.frame(Group=group_name, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- "Group"
    
    key <- paste0("Group=", group_name)
    return(kvPair(key, val))
  })
  
  result <- ddo(result)
  return(result)
}