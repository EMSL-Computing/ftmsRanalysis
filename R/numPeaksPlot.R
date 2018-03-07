#' Plot to look at the number of peaks per sample 
#' 
#' Constructs scatter plot to look at the number of peaks per sample.
#' 
#' @param dataObj icrData object
#' @param xaxis x axis variable. If NULL, will use attr(dataObj, "cnames")$fdata_cname. Must be one of
#'                colnames(dataObj$f_data) or colnames(attr(dataObj, "group_DF")).
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
numPeaksPlot <- function(dataObj, xaxis=NULL, ylabel="Number of Peaks", title=NULL) {
  
  # Initial Checks #
  if (!inherits(dataObj, "icrData")) {
    stop("dataObj must be of type icrData")
  }
  
  if(is.null(xaxis)){
    xaxis <- getFDataColName(dataObj)
  }
  
  if(!(xaxis %in% c(colnames(dataObj$f_data), colnames(attr(dataObj, "group_DF"))))){
    stop("xaxis must be one of the column names of dataObj$f_data or attr(dataObj, 'group_DF').")
  }
  
  # End Initial Checks #
  
  # calculate the number of peaks per sample
  peaks <- apply(dataObj$e_data[,-which(colnames(dataObj$e_data) == getMassColName(dataObj))], 2, function(x) sum(which(!is.na(x) & x > 0)))
  peaks <- data.frame(Sample=names(peaks), Peaks=peaks)
  colnames(peaks)[1] <- getFDataColName(dataObj)
  
  # merge peaks with metadata or group_DF
  if(xaxis %in% colnames(dataObj$f_data)){
    peaks <- merge(peaks, dataObj$f_data, by=getFDataColName(dataObj))
  }else{
    peaks <- merge(peaks, attr(dataObj, "group_DF"), by=getFDataColName(dataObj))
  }
  
  # Plot
  map <- ggplot2::aes_string(x=xaxis, y="Peaks", label=getFDataColName(dataObj))
  p <- ggplot2::ggplot(peaks, map)+
    ggplot2::geom_point(cex=3)+
    ggplot2::theme_bw()+
    ggplot2::labs(x=xaxis, y=ylabel, title=title)+
    ggplot2::theme(axis.text.x = element_text(angle=90))
  
  plotly::ggplotly(p)
  
}
