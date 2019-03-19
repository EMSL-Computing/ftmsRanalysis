#' Plot to look at the number of peaks per sample 
#' 
#' Constructs scatter plot to look at the number of peaks per sample.
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}.
#' @param xaxis x axis variable. If NULL, will use attr(ftmsObj, "cnames")$fdata_cname. Must be one of
#'                colnames(ftmsObj$f_data) or colnames(attr(ftmsObj, "group_DF")).
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
numPeaksPlot <- function(ftmsObj, xaxis=NULL, ylabel="Number of Peaks", title=NULL) {
  
  # Initial Checks #
  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  
  if(is.null(xaxis)){
    xaxis <- getFDataColName(ftmsObj)
  }
  
  if(!(xaxis %in% c(colnames(ftmsObj$f_data), colnames(attr(ftmsObj, "group_DF"))))){
    stop("xaxis must be one of the column names of ftmsObj$f_data or attr(ftmsObj, 'group_DF').")
  }
  
  # End Initial Checks #
  
  # calculate the number of peaks per sample
  peaks <- apply(ftmsObj$e_data[,-which(colnames(ftmsObj$e_data) == getMassColName(ftmsObj))], 2, function(x) length(which(!is.na(x) & x > 0)))
  peaks <- data.frame(Sample=names(peaks), Peaks=peaks)
  colnames(peaks)[1] <- getFDataColName(ftmsObj)
  
  # merge peaks with metadata or group_DF
  if(xaxis %in% colnames(ftmsObj$f_data)){
    peaks <- merge(peaks, ftmsObj$f_data, by=getFDataColName(ftmsObj))
  }else{
    peaks <- merge(peaks, attr(ftmsObj, "group_DF"), by=getFDataColName(ftmsObj))
  }
  
  # Plot
  map <- ggplot2::aes_string(x=xaxis, y="Peaks", label=getFDataColName(ftmsObj))
  p <- ggplot2::ggplot(peaks, map)+
    ggplot2::geom_point(cex=3)+
    ggplot2::theme_bw()+
    ggplot2::labs(x=xaxis, y=ylabel, title=title)+
    ggplot2::theme(axis.text.x = element_text(angle=90))
  
  plotly::ggplotly(p)
  
}
