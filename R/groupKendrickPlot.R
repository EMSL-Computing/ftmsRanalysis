#' Kendrick plot of icrData object
#' 
#' Constructs a Kendrick plot (Kendrick defect vs Kendrick mass) for an
#' object of type icrData.
#' 
#' @param icrData icrData object of class 'groupSummary' as a result of calling \code{\link{summarizeGroups}}
#' @param title plot title
#' @param colorPal color palette function, one of \code{\link{col_numeric}}, \code{\link{col_factor}} or similar \code{scales} palette function
#' @param colorCName column name of \code{icrData$e_data} to use for coloring the points
#' @param xlabel x axis label, default is "Kendrick Mass"
#' @param ylabel y axis label, default is "Kendrick Defect"
#' @param legendTitle title for the legend, only used when coloring points according to a numeric scale
#' @return a plotly object
#' @seealso \code{\link{plot_ly}}
#' @export
groupKendrickPlot <- function(icrData, title=NA, colorPal=NA, colorCName=NA,
                         xlabel="Kendrick Mass", ylabel="Kendrick Defect", legendTitle=colorCName) {
  require(dplyr)
  require(plotly)
  require(scales)
  
  # Test inputs
  if (!inherits(icrData, "groupSummary")) {
    stop("icrData must be of type groupSummary")
  }
  if ( inherits(icrData, "groupComparison")) {
    stop("icrData cannot be a groupComparison object for this function")
  }
  if (is.na(colorCName) | is.null(colorCName)) {
    stop("colorCName must be specified")
  }
  if (! (colorCName %in% colnames(icrData$e_data))) {
    stop("colorCName must be a column of the e_data element")
  }
  
  return(fticRanalysis:::.kendrickPlotInternal(icrData, title=title, colorPal=colorPal, colorCName=colorCName, 
                           xlabel=xlabel, ylabel=ylabel, legendTitle=legendTitle))
  
}

#kendrickPlot(icrData, title=colnames(icrData$e_data)[2])
