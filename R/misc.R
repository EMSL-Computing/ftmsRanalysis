########## CONVENIENCE FUNCTIONS ############



#' Boolean vector indicating which peaks were observed
#' 
#' Returns a boolean vector the same length as the number of peaks in the
#' data indicating which peaks were observed (i.e. had nonzero values) for 
#' any column of data.
#' 
#' @param data.obj icrData object
#' @return vector of TRUE/FALSE values the same length as nrow(data.obj$e_data)
#' 
peaksObserved <- function(data.obj) {
    obs.peaks <- rowSums(data.obj$e_data)-data.obj$e_data[,getEDataColName(data.obj)] > 0
    return(obs.peaks)
}

