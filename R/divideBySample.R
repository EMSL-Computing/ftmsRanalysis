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

    val <- subset(icrData, samples=ss)
    
    # datadr attributes
    attr(val, "split") <- data.frame(Sample=ss, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- sample.colname
    
    key <- paste0(sample.colname, "=", ss)
    return(kvPair(key, val))
  })
 
  result <- ddo(result)
  return(result)
}

