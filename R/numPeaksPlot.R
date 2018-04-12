#' Plot to look at the number of peaks per sample 
#' 
#' Constructs scatter plot to look at the number of peaks per sample.
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#' @param xaxis x axis variable. If NULL, will use attr(icrData, "cnames")$fdata_cname. Must be one of
#'                colnames(icrData$f_data) or colnames(attr(icrData, "group_DF")).
#' @param ylabel y axis label, default is "Density"
#' @param title plot title, default is NULL
#' 
#' @return a plotly object
#' 
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Allison Thompson
#' 
#' @export
numPeaksPlot <- function(icrData, xaxis=NULL, ylabel="Number of Peaks", title=NULL) {
  
  # Initial Checks #
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  
  if(is.null(xaxis)){
    xaxis <- getFDataColName(icrData)
  }
  
  if(!(xaxis %in% c(colnames(icrData$f_data), colnames(attr(icrData, "group_DF"))))){
    stop("xaxis must be one of the column names of icrData$f_data or attr(icrData, 'group_DF').")
  }
  
  # End Initial Checks #
  
  # calculate the number of peaks per sample
  peaks <- apply(icrData$e_data[,-which(colnames(icrData$e_data) == getMassColName(icrData))], 2, function(x) length(which(!is.na(x) & x > 0)))
  peaks <- data.frame(Sample=names(peaks), Peaks=peaks)
  colnames(peaks)[1] <- getFDataColName(icrData)
  
  # merge peaks with metadata or group_DF
  if(xaxis %in% colnames(icrData$f_data)){
    peaks <- merge(peaks, icrData$f_data, by=getFDataColName(icrData))
  }else{
    peaks <- merge(peaks, attr(icrData, "group_DF"), by=getFDataColName(icrData))
  }
  
  # Plot
  map <- ggplot2::aes_string(x=xaxis, y="Peaks", label=getFDataColName(icrData))
  p <- ggplot2::ggplot(peaks, map)+
    ggplot2::geom_point(cex=3)+
    ggplot2::theme_bw()+
    ggplot2::labs(x=xaxis, y=ylabel, title=title)+
    ggplot2::theme(axis.text.x = element_text(angle=90))
  
  plotly::ggplotly(p)
  
}
