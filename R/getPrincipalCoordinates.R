#' Get principal coordinates for the samples in an ftmsData object
#'
#' Calculate the principal coordinates of the samples in an ftmsData object. Uses \code{\link{vegdist}} to calculate pairwise distances between samples, and \code{\link{pcoa}} to calculate the principal coordinates.
#' 
#' @param ftmsObj object of type \code{ftmsData} 
#' @param dist_metric distance metric for computing pairwise distances between all samples. See \code{\link{vegdist}} for options. Default is "bray".
#' @param n_dims number of dimensions to return, default is 5. Must be less than the number of samples in \code{ftmsObj}
#'
#' @return data.frame of coordinates where each sample is a row
#' @export
#' @seealso \link{vegdist}, \link{pcoa}
#'
#' @examples
#' coords <- getPrincipalCoordinates(exampleProcessedPeakData, dist_metric="jaccard", n_dims=7)
getPrincipalCoordinates <- function(ftmsObj, dist_metric="bray", n_dims=5) {
  
  # test inputs
  if (!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be an object of type 'ftmsData'")
  if (n_dims < 0 | abs(n_dims - round(n_dims)) > .Machine$double.eps^0.5) stop("n_dims must be a positive integer")
  
  # transform to presence/absence data if necessary
  if (getDataScale(ftmsObj) != "pres") {
    ftmsObj <- edata_transform(ftmsObj, "pres")
  }
  
  samp_cols <- as.character(dplyr::pull(ftmsObj$f_data, getFDataColName(ftmsObj)))
  samp_cols <- samp_cols[samp_cols %in% colnames(ftmsObj$e_data)]
  
  if (n_dims >= length(samp_cols)) 
    stop(sprintf("n_dims must be a positive integer less than the number of samples (%d)", length(samp_cols)))
  
  # need to transpose data before calling vegdist
  vals <- t(as.matrix(ftmsObj$e_data[, samp_cols]))
  dist_mat <- vegan::vegdist(vals, method=dist_metric, binary=TRUE)
  
  pcoa_res <- ape::pcoa(dist_mat)
  pcoa_vec <- pcoa_res$vectors[, 1:n_dims]
  attr(pcoa_vec, "R^2") <- pcoa_res$values$Relative_eig[1:n_dims]

  return(pcoa_vec)
}
