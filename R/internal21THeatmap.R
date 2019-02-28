# Internal only function to produce a heatmap for 21T data (Kendrick and Van Krevelen Plots)
# ftmsObj: ftmsData object
# xCName/yCName: column names for x and y data
# xBreaks/yBreaks: either a number of breaks, or a vector of break endpoints
# colorPal: either a name of an RColorBrewer palette or a color palette function (e.g. scales package)
#     that maps the range [0,1] to colors
# title/xlabel/ylabel: plot and axis labels
# mouseovers: TRUE/FALSE, should mouseovers be constructed? Uses xlabel, ylabel values.
.internal21THeatmap <- function(ftmsObj, xCName, yCName, xBreaks=100, yBreaks=100, colorPal=NA, 
                                title=NA, xlabel=NA, ylabel=NA, mouseovers=TRUE) {
  
  # Get the two columns of data to plot from ftmsObj
  if (xCName %in% colnames(ftmsObj$e_data)) {
    plot_data <- ftmsObj$e_data[, c(getEDataColName(ftmsObj), xCName)]
  } else if (xCName %in% colnames(ftmsObj$e_meta)) {
    plot_data <- ftmsObj$e_meta[, c(getEDataColName(ftmsObj), xCName)]
  } else {
    stop("xCName must be a column name in e_data or e_meta")
  }
  if (!is.numeric(plot_data[, xCName])) stop("xCName must be a numeric column")
  
  if (yCName %in% colnames(ftmsObj$e_data)) {
    plot_data <- dplyr::full_join(plot_data, ftmsObj$e_data[, c(getEDataColName(ftmsObj), yCName)], by=getEDataColName(ftmsObj))
  } else if (yCName %in% colnames(ftmsObj$e_meta)) {
    plot_data <- dplyr::full_join(plot_data, ftmsObj$e_meta[, c(getEDataColName(ftmsObj), yCName)], by=getEDataColName(ftmsObj))
  } else {
    stop("yCName must be a column name in e_data or e_meta")
  }
  if (!is.numeric(plot_data[, yCName])) stop("yCName must be a numeric column")
  
  # Include only rows (peaks) where that are observed in at least one column of e_data
  samp_cnames <- as.character(ftmsObj$f_data[, getFDataColName(ftmsObj)])
  ind <- ftmsRanalysis:::n_present(ftmsObj$e_data[,samp_cnames], getDataScale(ftmsObj))[,1] > 0
  
  obs_peaks <- as.character(ftmsObj$e_data[ind, getEDataColName(ftmsObj)])
  plot_data <- plot_data[which(plot_data[,getEDataColName(ftmsObj)] %in% obs_peaks), ]
  xData <- dplyr::pull(plot_data, xCName)
  yData <- dplyr::pull(plot_data, yCName)
  
  # Bin data and get cross-table of frequencies
  if (length(xBreaks) == 1) { 
    xBreaks <- seq(from=min(xData, na.rm=TRUE), to=max(xData, na.rm=TRUE), length=xBreaks+1)
  }
  if (length(yBreaks) == 1) {
    yBreaks <- seq(from=min(yData, na.rm=TRUE), to=max(yData, na.rm=TRUE), length=yBreaks+1)
  }  
  xBins <- cut(xData, breaks=xBreaks, labels=FALSE, include.lowest = TRUE)
  yBins <- cut(yData, breaks=yBreaks, labels=FALSE, include.lowest = TRUE)
  counts <- as.data.frame(table(xBins, yBins))
  
  xMids <- xBreaks[-1]-diff(xBreaks)[1]/2
  yMids <- yBreaks[-1]-diff(yBreaks)[1]/2
  counts$xMids <- xMids[counts$xBins]
  counts$yMids <- yMids[counts$yBins]
  
  # Generate mouseover text
  if (mouseovers) {
    tmpXlab <- xlabel
    tmpYlab <- ylabel
    if (is.na(xlabel)) tmpXlab <- "X Range"
    if (is.na(ylabel)) tmpYlab <- "Y Range"
    counts$label <- sprintf("%s: %.3f-%.3f<br>%s: %.3f-%.3f<br>Count: %d", 
                            tmpXlab, counts$xMids-diff(xBreaks)[1]/2, counts$xMids+diff(xBreaks)[1]/2,
                            tmpYlab, counts$yMids-diff(yBreaks)[1]/2, counts$yMids+diff(yBreaks)[1]/2,
                            counts$Freq)
  }

  # Get color palette  
  if (is.na(colorPal)) colorPal <- "YlOrRd"
  if (is.character(colorPal)) {
    palFunction <- scales::col_numeric(colorPal, c(0,1), na.color="#FFFFFF")
  } else if (is.function(colorPal)) {
    palFunction <- colorPal
  } else {
    stop("'colorPal' must be a name of an RColorBrewer palette or a palette function (e.g. scales::col_numeric)")
  }
  
  # Construct plot
  if (mouseovers) {
    p <- plotly::plot_ly(x=counts$xMids, y=counts$yMids, z=counts$Freq, type="heatmap", colors=palFunction,
                         zmin=min(counts$Freq, na.rm=TRUE), zmax=max(counts$Freq, na.rm=TRUE), 
                         text=counts$label, hoverinfo="text")
  } else {
    p <- plotly::plot_ly(x=counts$xMids, y=counts$yMids, z=counts$Freq, type="heatmap", colors=palFunction,
                         zmin=min(counts$Freq, na.rm=TRUE), zmax=max(counts$Freq, na.rm=TRUE), hoverinfo="none")
  } 
  
  # Add titles if applicable
  if (!is.na(title)) p <- p %>% plotly::layout(title=title)
  if (!is.na(xlabel)) p <- p %>% plotly::layout(xaxis=list(title=xlabel))
  if (!is.na(ylabel)) p <- p %>% plotly::layout(yaxis=list(title=ylabel))
  
  return(p)
}
