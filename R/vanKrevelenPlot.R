#' Van Krevelen plot of icrData object
#' 
#' Create a Van Krevelen plot (H:C ratio vs O:C ratio) for an icrData object using Plotly.
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', (output of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}),
#' or a 'groupSummary' object (output of \code{\link{summarizeGroups}}) or a 'comparisonSummary' object (output of \code{\link{summarizeGroupComparisons}}).
#' @param title plot title
#' @param colorPal color palette function, one of \code{\link{col_numeric}}, \code{\link{col_factor}} or similar \code{scales} palette function
#' @param colorCName column name of \code{icrData$e_data} or \code{icrData$e_meta} to use for coloring the points. If NA and vkBoundarySet is provided, the points will be colored according to the Van Krevelen category.
#' @param vkBoundarySet character vector specifying which boundary set to use when determining class. Valid options are currently "bs1" and "bs2" and defaults to "bs1". See details of \code{\link{assign_class}} for differences in sets.
#' @param showVKBounds TRUE/FALSE, should the Van Krevelen bounds be plotted?
#' @param xlabel x axis label, default is "O:C Ratio"
#' @param ylabel y axis label, default is "H:C Ratio"
#' @param legendTitle title for the legend, only used when coloring points according to a numeric scale
#' @return a plotly object
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Amanda White
#' 
#' @export
vanKrevelenPlot <- function(icrData, title=NA, colorPal=NA, colorCName=NA, vkBoundarySet = "bs1", showVKBounds=TRUE, 
                            xlabel="O:C Ratio", ylabel="H:C Ratio", legendTitle=colorCName) {

  # here's an R quirk: legendTitle=colorCName by default. legendTitle value is not 'fixed' until the first
  # time it's evaluated. So if legendTitle is not explicitly set and I change colorCName in this function
  # before accessing legendTitle, legendTitle will be given the new colorCName value!
  tmp <- legendTitle
  
  logColorCol <- FALSE
  if (!is.na(colorCName)) {
    if (colorCName =="Intensity") {
      if (ncol(icrData$e_data) > 2) {
        stop("If colorCName == 'Intensity' then only one sample column may be present in icrData$e_data")
      }
      colorCName <- setdiff(colnames(icrData$e_data), getEDataColName(icrData)) # otherwise get the one sample column
    } 
    if (colorCName %in% colnames(icrData$e_data) & getDataScale(icrData) == "abundance") {
      logColorCol <- TRUE
      message("Abundance values will be log-transformed before plotting")
      if (legendTitle == "Intensity" | legendTitle == colorCName) legendTitle <- paste("Log", legendTitle)
    }
  } 
  
  return(fticRanalysis:::.vanKrevelenPlotInternal(icrData, title=title, colorPal=colorPal, colorCName=colorCName, 
                                  vkBoundarySet=vkBoundarySet, showVKBounds=showVKBounds,
                                  xlabel=xlabel, ylabel=ylabel, legendTitle=legendTitle, logColorCol=logColorCol))
}

# internal only function to do the work for both sample and group level VK plots
.vanKrevelenPlotInternal <- function(icrData, title=NA, colorPal=NA, colorCName=NA, vkBoundarySet = "bs1", showVKBounds=TRUE, 
                                             xlabel="O:C Ratio", ylabel="H:C Ratio", legendTitle=colorCName, logColorCol=FALSE) {
  
  # Test inputs
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  OC.col <- getOCRatioColName(icrData)
  if (is.null(OC.col) | !isTRUE(is.element(OC.col, colnames(icrData$e_meta)))) {
    stop("O:C ratio column attribute is not set or is not present in icrData$e_meta")
  }
  HC.col <- getHCRatioColName(icrData)
  if (is.null(HC.col) | !isTRUE(is.element(HC.col, colnames(icrData$e_meta)))) {
    stop("H:C ratio column attribute is not set or is not present in icrData$e_meta")
  }
  
  if (is.na(colorCName) & is.na(vkBoundarySet)) {
    stop("at least one of colorCName or vkBoundarySet must be specified")
  }
  
  # If data is not 12T then do a heatmap instead of points
  if (getInstrumentType(icrData) != "12T") {
    return(.internal21THeatmap(icrData, xCName=OC.col, yCName=HC.col, xBreaks=100, yBreaks=100, 
                               colorPal=colorPal, xlabel=xlabel, ylabel=ylabel))
  }
  
  vk_color_different_than_pts <- FALSE
  if (!is.na(vkBoundarySet)) {
    icrData <- assign_class(icrData, boundary_set = vkBoundarySet)
    icrData$e_meta$Class <- gsub(";.*", "", icrData$e_meta[,paste0(vkBoundarySet, "_class")])
    
    # data.frame of van krevelen bounds for plotting purposes
    vankrev_categories <- getVanKrevelenPlottingDF(vkBoundarySet)
    
    if (showVKBounds & !is.na(colorCName)) {
      vk_color_different_than_pts <- TRUE
    }  
    
    if (is.na(colorCName)) colorCName <- "Class"
    
  }
  
  xrange <- nice_axis_limits(icrData$e_meta[, OC.col], zero.min=TRUE)
  yrange <- nice_axis_limits(icrData$e_meta[, HC.col], zero.min=TRUE)

  # Show Mass and Molecular Formula
  hovertext <- paste("Molecular Formula: ", icrData$e_meta[, getMFColName(icrData)],"<br>", getEDataColName(icrData),": ", icrData$e_meta[,getEDataColName(icrData)], sep = "")
  icrData$e_meta$Hover <- hovertext
  
  p <- scatterPlot(icrData, OC.col, HC.col, colorCName = colorCName, colorPal=colorPal, xlabel=xlabel, ylabel=ylabel,
                   legendTitle=legendTitle, title=title, xrange=xrange, yrange=yrange, logColorCol=logColorCol, hoverTextCName="Hover")
  
  if (showVKBounds) {
    if (vk_color_different_than_pts) {
      # if vk boxes are drawn but points are not colored according to vk category, color boxes black
      p <- p %>%
        plotly::add_segments(x=~x0, y=~y0, xend=~x1, yend=~y1, data=vankrev_categories, 
                     text=vankrev_categories$category, hoverinfo="text", line=list(color="#000000"), 
                     showlegend = FALSE, inherit=FALSE) 
    } else {
      p <- p %>%
        plotly::add_segments(x=~x0, y=~y0, xend=~x1, yend=~y1, data=vankrev_categories, color=~category,
                     text=vankrev_categories$category, hoverinfo="text", showlegend = FALSE, 
                     inherit=FALSE) 
    }
  }

  p
}
