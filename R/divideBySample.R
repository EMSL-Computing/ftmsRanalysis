#' Divide icrData by sample to form a ddo
#' 
#' Construct a \code{\link{ddo}} from an \code{icrData} object by dividing
#' by sample. The resulting object may be used with Trelliscope to make
#' plots for each sample.
#' 
#' @param icrDataObj icrData object
#' @return a ddo where each division is a subset of \code{icrDataObj} corresponding
#'         to a single sample
#' @seealso \code{\link{ddo}}
#' @export
divideBySample <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) stop("Not an icrData object")
  require(datadr)
  sample.colname <- getFDataColName(icrDataObj)
  samples <- as.character(icrDataObj$f_data[, sample.colname])
  
  edata_nonsample_cols <- setdiff(colnames(icrDataObj$e_data), samples)
  
  result <- lapply(samples, function(ss) {
    ind <- samples == ss
    f_data <- icrDataObj$f_data[ind, ]
    e_data <- icrDataObj$e_data[, c(edata_nonsample_cols, ss)]
    e_meta <- icrDataObj$e_meta
    group_DF <- NULL
    if (!is.null(attr(icrDataObj, "group_DF"))) {
      group_DF <- attr(icrDataObj, "group_DF")
      ind <- group_DF[, sample.colname] == ss
      group_DF <- group_DF[ind, ]
    }
    
    val <- list(e_data=e_data, f_data=f_data, e_meta=e_meta)
    attr(val, "class") <- class(icrDataObj)
    attr(val, "cnames") <- attr(icrDataObj, "cnames")
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

