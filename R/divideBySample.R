#' Divide an ftmsData objecdt by sample to form a ddo
#' 
#' Construct a \code{\link[datadr:ddo]{ddo}} from an \code{ftmsData} object by dividing
#' by sample. The resulting object may be used with Trelliscope to make
#' plots for each sample.
#' 
#' @param ftmsObj ftmsData object
#' @return a ddo where each division is a subset of \code{ftmsObj} corresponding
#'         to a single sample
#' @seealso \code{\link[datadr:ddo]{ddo}}
#' @export

divideBySample <- function(ftmsObj) {
  
  if (!inherits(ftmsObj, "ftmsData")) stop("Not an ftmsData object")
  
  sample.colname <- getFDataColName(ftmsObj)
  samples <- as.character(ftmsObj$f_data[, sample.colname])
  
  edata_nonsample_cols <- setdiff(colnames(ftmsObj$e_data), samples)
  
  result <- lapply(samples, function(ss) {
    
    val <- subset(ftmsObj, samples=ss)
    
    attr(val, "split") <- data.frame(Sample=ss, stringsAsFactors = FALSE)
    colnames(attr(val, "split")) <- sample.colname
    
    return(val)
  })
  
  result_names <- lapply(samples, function(ss) {
    paste0(sample.colname, "=", ss)
  })
  names(result) <- result_names
  
  return(result)
}