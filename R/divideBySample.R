#' Divide icrData by sample to form a ddo
#' 
#' Construct a \code{\link{ddo}} from an \code{icrData} object by dividing
#' by sample. The resulting object may be used with Trelliscope to make
#' plots for each sample.
#' 
#' @param icrData icrData object
#' @return a ddo where each division is a subset of \code{icrData} corresponding
#'         to a single sample
#' @seealso \code{\link{ddo}}
#' @export
divideBySample <- function(icrData) {
  if (!inherits(icrData, "icrData")) stop("Not an icrData object")
  require(datadr)
  sample.colname <- getFDataColName(icrData)
  samples <- as.character(icrData$f_data[, sample.colname])
  
  edata_nonsample_cols <- setdiff(colnames(icrData$e_data), samples)
  
  result <- lapply(samples, function(ss) {
    ind <- samples == ss
    f_data <- icrData$f_data[ind, ]
    e_data <- icrData$e_data[, c(edata_nonsample_cols, ss)]
    e_meta <- icrData$e_meta
    group_DF <- NULL
    if (!is.null(attr(icrData, "group_DF"))) {
      group_DF <- attr(icrData, "group_DF")
      ind <- group_DF[, sample.colname] == ss
      group_DF <- group_DF[ind, ]
    }
    
    val <- list(e_data=e_data, f_data=f_data, e_meta=e_meta)
    attr(val, "class") <- class(icrData)
    attr(val, "cnames") <- attr(icrData, "cnames")
    if (!is.null(group_DF)) {
      attr(val, "group_DF") <- group_DF
    }
    attr(val, "split") <- data.frame(Sample=ss, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- sample.colname
    
    key <- paste0(sample.colname, "=", ss)
    return(kvPair(key, val))
  })
 
  result <- ddo(result)
  return(result)
}

