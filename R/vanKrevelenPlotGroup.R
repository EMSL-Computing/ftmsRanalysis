#' Van Krevelen plot of icrData object
#' 
#' Create a Van Krevelen plot (H:C ratio vs O:C ratio) for an icrData object using Plotly.
#' 
#' @param data.obj icrData object
#' @param title (optional) plot title
#' @param colors (optional) named vector of colors or hex codes corresponding to Van Krevelen categories
#' @param xlabel x axis label, default is "O:C Ratio"
#' @param ylabel y axis label, default is "H:C Ratio"
#' @param vkBoundarySet character vector specifying which boundary set to use when determining class. Valid options are currently "bs1" and "bs2" and defaults to "bs1". See details of \code{\link{assign_class}} for differences in sets.
#' @return a plotly object
#' @seealso \code{\link{plot_ly}}
#' @export
vanKrevelenPlotGroup <- function(data.obj, title=NA, colors=NA, xlabel="O:C Ratio", ylabel="H:C Ratio", vkBoundarySet="bs1") {
  require(dplyr)
  require(plotly)
  require(scales)
  # 
  # # Test inputs
  # if (!inherits(data.obj, "icrData")) {
  #   stop("data.obj must be of type icrData")
  # }
  if (is.null(data.obj$e_meta)) {
    stop("data.obj must have e_meta element")
  }
  
  OC.col <- fticRanalysis:::getOCRatioColName(data.obj)
  if (is.null(OC.col) | !is.element(OC.col, colnames(data.obj$e_meta))) {
    stop("O:C ratio column attribute is not set or is not present in data.obj$e_meta")
  }
  HC.col <- fticRanalysis:::getHCRatioColName(data.obj)
  if (is.null(HC.col) | !is.element(HC.col, colnames(data.obj$e_meta))) {
    stop("H:C ratio column attribute is not set or is not present in data.obj$e_meta")
  }
  
  # Van Krevelen categories
  emeta.df <- data.obj$e_meta
  emeta.df$Class <- fticRanalysis:::getVanKrevelenCategories(data.obj, boundary_set=vkBoundarySet)
  emeta.df[,fticRanalysis:::getEDataColName(data.obj)] = as.character(emeta.df[,fticRanalysis:::getEDataColName(data.obj)])
  
  # Include only rows (peaks) where that are observed in at least one column of e_data
  emeta.df <- merge(emeta.df, data.obj$e_data, by=fticRanalysis:::getEDataColName(data.obj))
  emeta.df$Pres <- rowSums(data.obj$e_data[,-which(colnames(data.obj$e_data) == getEDataColName(data.obj))] > 0, na.rm=TRUE)
  emeta.df$PercPres <- rowSums(data.obj$e_data[,-which(colnames(data.obj$e_data) == getEDataColName(data.obj))] > 0, na.rm=TRUE) / (ncol(data.obj$e_data) - 1)
  emeta.df <- emeta.df[which(rowSums(data.obj$e_data[,-which(colnames(data.obj$e_data) == getEDataColName(data.obj))], na.rm=TRUE) > 0),]
  
  # obs.peaks <- as.character(data.obj$e_data[data.obj$e_data[,-which(names(data.obj$e_data) == icRanalysis:::getEDataColName(data.obj))] > 0, icRanalysis:::getEDataColName(data.obj)])
  # emeta.df <- emeta.df[which(emeta.df[,icRanalysis:::getEDataColName(data.obj)] %in% obs.peaks), ]
  
  # data.frame of van krevelen bounds for plotting purposes
  vankrev_categories <- fticRanalysis:::getVanKrevelenPlottingDF(boundary_set=vkBoundarySet)
  
  # color palette
  if (is.na(colors)) {
    col.pal<- scales::col_factor("Set1", levels=rownames(fticRanalysis:::getVanKrevelenCategoryBounds(boundary_set=vkBoundarySet)))   
    col.vec <- col.pal(rownames(fticRanalysis:::getVanKrevelenCategoryBounds(boundary_set=vkBoundarySet)))
    names(col.vec) <- rownames(fticRanalysis:::getVanKrevelenCategoryBounds(boundary_set=vkBoundarySet))
  } else if (length(colors) == 9) {
    col.vec <- colors
  }
  
  ind.na <- is.na(emeta.df[,OC.col]) | is.na(emeta.df[,HC.col])
  emeta.df <- emeta.df[!ind.na, ]
  
  hovertext <- emeta.df[, fticRanalysis:::getMFColName(data.obj)]
  # p <- plot_ly(emeta.df, x=emeta.df[,OC.col], y=emeta.df[,HC.col]) %>%
  #   add_markers(color=~Pres, colors=grpcol, text=hovertext, hoverinfo="text") %>%
  #   add_segments(x=~x0, y=~y0, xend=~x1, yend=~y1, data=vankrev_categories, color=~category, colors=col.vec, showlegend = FALSE, inherit=FALSE) %>% #, width = 2, legend = FALSE) %>%
  #   layout(xaxis=list(title=xlabel, range=nice_axis_limits(emeta.df[, OC.col], zero.min=TRUE)), 
  #          yaxis=list(title=ylabel, range=nice_axis_limits(emeta.df[, HC.col], zero.min=TRUE)))
  # if (!is.na(title)) {
  #   p <- p %>% layout(title=title)
  # }
  # p
  
  
  p <- ggplot(emeta.df, aes(x=emeta.df[,OC.col], y=emeta.df[,HC.col])) +
    geom_point(aes(fill=as.factor(Pres)),pch=21, size=4)+
    geom_segment(data=vankrev_categories, aes(x=x0, xend=x1, y=y0, yend=y1, colour=category), guides="none", cex=2)+
    scale_color_manual(values=col.vec)+
    scale_fill_brewer()+
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour="black"))
  ggplotly(p)
}
#vanKrevelenPlot(data.obj, title=colnames(data.obj$e_data)[2])

# Function for calculating pretty axis limits for plots
# 
# Internal only convenience function for calculating axis limits
# that are extended by 5% outside the bounds of the observed values (or
# that start at 0 if specified). This is useful because Plotly adjusts
# axis limits when groups of points are de-selected unless fixed range
# limits are set in the layout function.
# @param values vector of values
# @param zero.min boolean, if TRUE the first element of the returned pair will be 0
# @return length 2 numeric vector suitable to serve as axis limits for plotly 
nice_axis_limits <- function(values, zero.min=FALSE) {
  lim <- range(values, na.rm=TRUE)
  d <- diff(lim)
  lim[1] <- ifelse(zero.min, 0, lim[1]-.05*d)
  lim[2] <- lim[2]+.05*d
  return(lim)
}
