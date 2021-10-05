#' Van Krevelen plot of an ftmsData object
#' 
#' Create a Van Krevelen plot (H:C ratio vs O:C ratio) for an ftmsData object using Plotly.
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', (output of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}),
#' or a 'groupSummary' object (output of \code{\link{summarizeGroups}}) or a 'comparisonSummary' object (output of \code{\link{summarizeGroupComparisons}}).
#' @param title plot title
#' @param colorPal color palette function, one of \code{\link{col_numeric}}, \code{\link{col_factor}} or similar \code{scales} palette function
#' @param colorCName column name of \code{ftmsObj$e_data} or \code{ftmsObj$e_meta} to use for coloring the points. If NA and vkBoundarySet is provided, the points will be colored according to the Van Krevelen category.
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
vanKrevelenPlot <- function(ftmsObj, title=NA, colorPal=NA, colorCName=NA, vkBoundarySet = "bs1", showVKBounds=TRUE, 
                            xlabel="O:C Ratio", ylabel="H:C Ratio", legendTitle=colorCName) {

  # here's an R quirk: legendTitle=colorCName by default. legendTitle value is not 'fixed' until the first
  # time it's evaluated. So if legendTitle is not explicitly set and I change colorCName in this function
  # before accessing legendTitle, legendTitle will be given the new colorCName value!
  tmp <- legendTitle
  
  logColorCol <- FALSE
  if (!is.na(colorCName)) {
    if (colorCName =="Intensity") {
      if (ncol(ftmsObj$e_data) > 2) {
        stop("If colorCName == 'Intensity' then only one sample column may be present in ftmsObj$e_data")
      }
      colorCName <- setdiff(colnames(ftmsObj$e_data), getEDataColName(ftmsObj)) # otherwise get the one sample column
    } 
    if (colorCName %in% colnames(ftmsObj$e_data) & getDataScale(ftmsObj) == "abundance") {
      logColorCol <- TRUE
      message("Abundance values will be log-transformed before plotting")
      if (legendTitle == "Intensity" | legendTitle == colorCName) legendTitle <- paste("Log", legendTitle)
    }
  } 
  
  return(ftmsRanalysis:::.vanKrevelenPlotInternal(ftmsObj, title=title, colorPal=colorPal, colorCName=colorCName, 
                                  vkBoundarySet=vkBoundarySet, showVKBounds=showVKBounds,
                                  xlabel=xlabel, ylabel=ylabel, legendTitle=legendTitle, logColorCol=logColorCol))
}

# internal only function to do the work for both sample and group level VK plots
.vanKrevelenPlotInternal <- function(ftmsObj, title=NA, colorPal=NA, colorCName=NA, vkBoundarySet = "bs1", showVKBounds=TRUE, 
                                             xlabel="O:C Ratio", ylabel="H:C Ratio", legendTitle=colorCName, logColorCol=FALSE) {
  
  # Test inputs
  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  OC.col <- getOCRatioColName(ftmsObj)
  if (is.null(OC.col) | !isTRUE(is.element(OC.col, colnames(ftmsObj$e_meta)))) {
    stop("O:C ratio column attribute is not set or is not present in ftmsObj$e_meta")
  }
  HC.col <- getHCRatioColName(ftmsObj)
  if (is.null(HC.col) | !isTRUE(is.element(HC.col, colnames(ftmsObj$e_meta)))) {
    stop("H:C ratio column attribute is not set or is not present in ftmsObj$e_meta")
  }
  
  if (is.na(colorCName) & is.na(vkBoundarySet)) {
    stop("at least one of colorCName or vkBoundarySet must be specified")
  }
  
  # If too much data then do a heatmap instead of points
  if (prod(nrow(ftmsObj$e_data), ncol(ftmsObj$e_data)-1) > 10^6) {
    message("Too much data in e_data to make a scatter plot, producing a heatmap instead")
    return(heatmap(ftmsObj, xCName=OC.col, yCName=HC.col, xBreaks=100, yBreaks=100, 
                               colorPal=colorPal, xlabel=xlabel, ylabel=ylabel))
  }
  
  vk_color_different_than_pts <- FALSE
  if (!is.na(vkBoundarySet)) {
    ftmsObj <- assign_class(ftmsObj, boundary_set = vkBoundarySet)
    ftmsObj$e_meta$Class <- gsub(";.*", "", ftmsObj$e_meta[,paste0(vkBoundarySet, "_class")])
    
    # data.frame of van krevelen bounds for plotting purposes
    vankrev_categories <- getVanKrevelenPlottingDF(vkBoundarySet)
    
    if (showVKBounds & !is.na(colorCName)) {
      vk_color_different_than_pts <- TRUE
    }  
    
    if (is.na(colorCName)) colorCName <- "Class"
    
  }
  
  # if ftmsObj is a comparison summary object, remove all NA rows from e_data/e_meta
  if (inherits(ftmsObj, "comparisonSummary")) {
    ind <- !is.na(dplyr::pull(ftmsObj$e_data, colorCName))
    ftmsObj$e_data <- ftmsObj$e_data[ind, ]
    masses <- dplyr::pull(ftmsObj$e_data, getEDataColName(ftmsObj))
    ind <- dplyr::pull(ftmsObj$e_meta, getEDataColName(ftmsObj)) %in% masses
    ftmsObj$e_meta <- ftmsObj$e_meta[ind, ]
  }
  
  # Show Mass and Molecular Formula
  hovertext <- paste("Molecular Formula: ", ftmsObj$e_meta[, getMFColName(ftmsObj)],"<br>", getEDataColName(ftmsObj),": ", ftmsObj$e_meta[,getEDataColName(ftmsObj)], sep = "")
  ftmsObj$e_meta$Hover <- hovertext
  
  p <- scatterPlot(ftmsObj, OC.col, HC.col, colorCName = colorCName, colorPal=colorPal, xlabel=xlabel, ylabel=ylabel,
                   legendTitle=legendTitle, title=title, logColorCol=logColorCol, hoverTextCName="Hover", zero.min=TRUE)
  
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
