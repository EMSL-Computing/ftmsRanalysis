#' Kendrick plot of icrData object
#' 
#' Constructs a Kendrick plot (Kendrick defect vs Kendrick mass) for an
#' object of type icrData.
#' 
#' @param data.obj icrData object
#' @param title (optional) plot title
#' @param colors (optional) named vector of colors or hex codes corresponding to Van Krevelen categories
#' @param xlabel x axis label, default is "Kendrick Mass"
#' @param ylabel y axis label, default is "Kendrick Defect"
#' @return a plotly object
#' @seealso \code{\link{plot_ly}}
#' @export
kendrickPlot <- function(data.obj, title=NA, colors=NA, xlabel="Kendrick Mass", ylabel="Kendrick Defect", boundary_set = "bs1") {
  require(dplyr)
  require(plotly)
  require(scales)
  
  # Test inputs
  if (!inherits(data.obj, "icrData")) {
    stop("data.obj must be of type icrData")
  }
  if (is.null(data.obj$e_meta)) {
    stop("data.obj must have e_meta element")
  }
  
  km.col <- getKendrickMassColName(data.obj)
  if (is.null(km.col) | !is.element(km.col, colnames(data.obj$e_meta))) {
    stop("Kendrick mass column attribute is not set or is not present in data.obj$e_meta")
  }
  kd.col <- getKendrickDefectColName(data.obj)
  if (is.null(kd.col) | !is.element(kd.col, colnames(data.obj$e_meta))) {
    stop("Kendrick defect column attribute is not set or is not present in data.obj$e_meta")
  }
  
  # Van Krevelen categories for coloring points
  emeta.df <- data.obj$e_meta
  emeta.df[,getEDataColName(data.obj)] = as.character(emeta.df[,getEDataColName(data.obj)])

  emeta.df$Class <- getVanKrevelenCategories(data.obj, boundary_set = boundary_set)
  
  # Include only rows (peaks) where that are observed in at least one column of e_data
  obs.peaks <- as.character(data.obj$e_data[data.obj$e_data[,-which(names(data.obj$e_data) == getEDataColName(data.obj))] > 0, getEDataColName(data.obj)])
  emeta.df <- emeta.df[which(emeta.df[,getEDataColName(data.obj)] %in% obs.peaks), ]
  
  # Color palette
  if (is.na(colors)) {
    col.pal<- scales::col_factor("Set1", levels=rownames(getVanKrevelenCategoryBounds(boundary_set)))   
    col.vec <- col.pal(rownames(getVanKrevelenCategoryBounds(boundary_set)))
    names(col.vec) <- rownames(getVanKrevelenCategoryBounds(boundary_set))
  } else if (length(colors) == 9) {
    col.vec <- colors
  }
  
  # get some column names #
  oc_id = getOCRatioColName(data.obj)
  hc_id = getHCRatioColName(data.obj)
  mf_id = getMFColName(data.obj)
  # Mouseover text (TODO add function parameter for this)
  hovertext <- sprintf("%s<br>%s<br>O:C=%1.3f<br>H:C=%1.3f", emeta.df[,mf_id], emeta.df$Class, emeta.df[,oc_id], emeta.df[,hc_id])
  
  p <- plot_ly(emeta.df, x=emeta.df[,km.col], y=emeta.df[,kd.col]) %>%
    add_markers(color=~Class, colors=col.vec, text=hovertext, hoverinfo="text") %>%
    layout(xaxis=list(title="Kendrick Mass", range=nice_axis_limits(emeta.df[, km.col])), 
           yaxis=list(title="Kendrick Defect", range=nice_axis_limits(emeta.df[, kd.col])))
  if (!is.na(title)) {
    p <- p %>% layout(title=title)
  }
  p
  
}

#kendrickPlot(data.obj, title=colnames(data.obj$e_data)[2])
