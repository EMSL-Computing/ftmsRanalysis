#' Kendrick plot of icrData object
#' 
#' Constructs a Kendrick plot (Kendrick defect vs Kendrick mass) for an
#' object of type icrData.
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#' @param title plot title
#' @param colorPal color palette function, one of \code{\link{col_numeric}}, \code{\link{col_factor}} or similar \code{scales} palette function
#' @param colorCName column name of \code{icrData$e_meta} to use for coloring the points. If NA and vkBoundarySet is provided, the points will be colored according to the Van Krevelen category.
#' @param vkBoundarySet character vector specifying which boundary set to use when determining class. Valid options are currently "bs1" and "bs2" and defaults to "bs1". See details of \code{\link{assign_class}} for differences in sets.
#' @param xlabel x axis label, default is "Kendrick Mass"
#' @param ylabel y axis label, default is "Kendrick Defect"
#' @param legendTitle title for the legend, only used when coloring points according to a numeric scale
#' @return a plotly object
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Amanda White
#'
#' @export
kendrickPlot <- function(icrData, title=NA, colorPal=NA, colorCName=NA, vkBoundarySet = "bs1", 
                         xlabel="Kendrick Mass", ylabel="Kendrick Defect", legendTitle=colorCName) {
  require(dplyr)
  require(plotly)
  require(scales)

  # here's an R quirk: legendTitle=colorCName by default. legendTitle value is not 'fixed' until the first
  # time it's evaluated. So if legendTitle is not explicitly set and I change colorCName in this function
  # before accessing legendTitle, legendTitle will be given the new colorCName value!
  tmp <- legendTitle
  
  logColorCol <- FALSE
  if (!is.na(colorCName) & getDataScale(icrData) == "12T") {
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
  
  return(fticRanalysis:::.kendrickPlotInternal(icrData, title=title, colorPal=colorPal, colorCName=colorCName, 
                               vkBoundarySet=vkBoundarySet, xlabel=xlabel, ylabel=ylabel, 
                               legendTitle=legendTitle, logColorCol=logColorCol))
} 

.kendrickPlotInternal <- function(icrData, title=NA, colorPal=NA, colorCName=NA, vkBoundarySet = "bs1", 
                                  xlabel="Kendrick Mass", ylabel="Kendrick Defect", legendTitle=colorCName, 
                                  logColorCol=FALSE) {
  # Test inputs
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  km_col <- getKendrickMassColName(icrData)
  if (is.null(km_col) | !is.element(km_col, colnames(icrData$e_meta))) {
    stop("Kendrick mass column attribute is not set or is not present in icrData$e_meta")
  }
  kd_col <- getKendrickDefectColName(icrData)
  if (is.null(kd_col) | !is.element(kd_col, colnames(icrData$e_meta))) {
    stop("Kendrick defect column attribute is not set or is not present in icrData$e_meta")
  }
  
  if (is.na(colorCName) & is.na(vkBoundarySet)) {
    stop("at least one of colorCName or vkBoundarySet must be specified")
  }
  
  # Van Krevelen categories
  if (!is.na(vkBoundarySet) & is.na(colorCName)) {
    icrData <- assign_class(icrData, boundary_set = vkBoundarySet)
    icrData$e_meta$Class <- gsub(";.*", "", icrData$e_meta[,paste0(vkBoundarySet, "_class")])
    colorCName <- "Class"
  }

  xrange <- nice_axis_limits(icrData$e_meta[, km_col])
  yrange <- nice_axis_limits(icrData$e_meta[, kd_col])
  
  # Show Mass and Molecular Formula
  hovertext <- paste("Molecular Formula: ", icrData$e_meta[, getMFColName(icrData)],"<br>", getEDataColName(icrData),": ", icrData$e_meta[,getEDataColName(icrData)], sep = "")
  icrData$e_meta$Hover <- hovertext
  
  p <- scatterPlot(icrData, km_col, kd_col, colorCName = colorCName, colorPal=colorPal, xlabel=xlabel, ylabel=ylabel,
                          legendTitle=legendTitle, title=title, xrange=xrange, yrange=yrange, logColorCol=logColorCol, hoverTextCName="Hover")
  
  p
  
}
