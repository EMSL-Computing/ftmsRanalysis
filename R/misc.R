########## CONVENIENCE FUNCTIONS ############



#' Boolean vector indicating which peaks were observed
#' 
#' Returns a boolean vector the same length as the number of peaks in the
#' data indicating which peaks were observed (i.e. had nonzero values) for 
#' any column of data.
#' 
#' @param ftmsObj ftmsData object
#' @return vector of TRUE/FALSE values the same length as nrow(data.obj$e_data)
#' 
peaksObserved <- function(ftmsObj) {
    obs.peaks <- rowSums(ftmsObj$e_data)-ftmsObj$e_data[,getEDataColName(ftmsObj)] > 0
    return(obs.peaks)
}

