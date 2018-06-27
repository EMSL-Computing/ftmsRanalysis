#' Van Krevelen plot of a group comparison icrData object
#' 
#' Create a Van Krevelen plot (H:C ratio vs O:C ratio) for a group comparison summary icrData object using Plotly.
#' 
#' @param icrData icrData object of class 'comparisonSummary' as a result of calling \code{\link{summarizeGroupComparisons}}
#' @param title plot title
#' @param colorPal color palette function, one of \code{\link{col_numeric}}, \code{\link{col_factor}} or similar \code{scales} palette function
#' @param colorCName column name of \code{icrData$e_data} to use for coloring the points. 
#' @param vkBoundarySet character vector specifying which boundary set to use when determining class. Valid options are currently "bs1" and "bs2" and defaults to "bs1". See details of \code{\link{assign_class}} for differences in sets.
#' @param showVKBounds TRUE/FALSE, should the Van Krevelen bounds be plotted?
#' @param xlabel x axis label, default is "O:C Ratio"
#' @param ylabel y axis label, default is "H:C Ratio"
#' @param legendTitle title for the legend, only used when coloring points according to a numeric scale
#' @return a plotly object
#' 
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Amanda White
comparisonVanKrevelenPlot <- function(icrData, title=NA, colorPal=NA, colorCName, vkBoundarySet = "bs1", showVKBounds=TRUE, 
                                           xlabel="O:C Ratio", ylabel="H:C Ratio") {
  
  require(dplyr)
  require(plotly)
  require(scales)
  
  # Test inputs
  if (!inherits(icrData, "comparisonSummary")) {
    stop("icrData must be of type comparisonSummary")
  }
  if (is.na(colorCName) | is.null(colorCName)) {
    stop("colorCName must be specified")
  }
  if (! (colorCName %in% colnames(icrData$e_data))) {
    stop("colorCName must be a column of the e_data element")
  }
  
  return(fticRanalysis:::.vanKrevelenPlotInternal(icrData, title=title, colorPal=colorPal, colorCName=colorCName, 
                                                  vkBoundarySet = vkBoundarySet, showVKBounds=showVKBounds, 
                                                  xlabel=xlabel, ylabel=ylabel, legendTitle=legendTitle))
  
}