#' Van Krevelen plot of icrData object
#' 
#' Create a Van Krevelen plot (H:C ratio vs O:C ratio) for an icrData object using Plotly.
#' 
#' @param dataObj icrData object
#' @param title plot title
#' @param colorPal color palette function, one of \code{\link{col_numeric}}, \code{\link{col_factor}} or similar \code{scales} palette function
#' @param colorCName column name of \code{dataObj$e_meta} to use for coloring the points. If NA and vkBoundarySet is provided, the points will be colored according to the Van Krevelen category.
#' @param vkBoundarySet character vector specifying which boundary set to use when determining class. Valid options are currently "bs1" and "bs2" and defaults to "bs1". See details of \code{\link{assign_class}} for differences in sets.
#' @param showVKBounds TRUE/FALSE, should the Van Krevelen bounds be plotted?
#' @param xlabel x axis label, default is "O:C Ratio"
#' @param ylabel y axis label, default is "H:C Ratio"
#' @param legendTitle title for the legend, only used when coloring points according to a numeric scale
#' @return a plotly object
#' @seealso \code{\link{plot_ly}}
#' @export
vanKrevelenPlot <- function(dataObj, title=NA, colorPal=NA, colorCName=NA, vkBoundarySet = "bs1", showVKBounds=TRUE, 
                            xlabel="O:C Ratio", ylabel="H:C Ratio", legendTitle=colorCName) {
  require(dplyr)
  require(plotly)
  require(scales)
  
  # Test inputs
  if (!inherits(dataObj, "icrData")) {
    stop("dataObj must be of type icrData")
  }
  if (is.null(dataObj$e_meta)) {
    stop("dataObj must have e_meta element")
  }
  if (inherits(dataObj, "groupSummary") | inherits(dataObj, "groupComparison")) {
    stop("dataObj cannot be a groupSummary or groupComparison object for this function")
  }
  
  OC.col <- getOCRatioColName(dataObj)
  if (is.null(OC.col) | !is.element(OC.col, colnames(dataObj$e_meta))) {
    stop("O:C ratio column attribute is not set or is not present in dataObj$e_meta")
  }
  HC.col <- getHCRatioColName(dataObj)
  if (is.null(HC.col) | !is.element(HC.col, colnames(dataObj$e_meta))) {
    stop("H:C ratio column attribute is not set or is not present in dataObj$e_meta")
  }
  
  if (is.na(colorCName) & is.na(vkBoundarySet)) {
    stop("at least one of colorCName or vkBoundarySet must be specified")
  }

  emeta.df <- dataObj$e_meta
  emeta.df[,getEDataColName(dataObj)] = as.character(emeta.df[,getEDataColName(dataObj)])
  
  vk_color_different_than_pts <- FALSE
  # Van Krevelen categories
  if (!is.na(vkBoundarySet)) {
    vk_cat <- factor(getVanKrevelenCategories(dataObj, vkBoundarySet), levels=rownames(getVanKrevelenCategoryBounds(vkBoundarySet)))
    emeta.df$vk_categories_really_long_name1234 <- vk_cat
    
    # data.frame of van krevelen bounds for plotting purposes
    vankrev_categories <- getVanKrevelenPlottingDF(vkBoundarySet)

    if (showVKBounds & !is.na(colorCName)) {
      vk_color_different_than_pts <- TRUE
    }  
    
    if (is.na(colorCName)) colorCName <- "vk_categories_really_long_name1234"
  }
  
  # color palette
  if (is.na(colorPal)) {
    if (is.numeric(emeta.df[, colorCName])) {
      val_range <- range(emeta.df[, colorCName], na.rm=TRUE)
      pal = RColorBrewer::brewer.pal(n = 9, "YlOrRd")[3:9]
      colorPal <- scales::col_numeric(palette=pal, domain=val_range)
      vals <- seq(val_range[1], val_range[2], length=100)
      col_vec <- colorPal(vals)
      names(col_vec) <- vals
    } else if (is.factor(emeta.df[, colorCName])) {
      cc <- levels(emeta.df[, colorCName])
      colorPal<- getFactorColorPalette(cc)  
      col_vec <- colorPal(cc)
      names(col_vec) <- cc
    } else if (is.character(emeta.df[, colorCName])) {
      cc <- sort(unique(emeta.df[, colorCName]))
      emeta.df[, colorCName] <- factor(emeta.df[, colorCName], levels = cc)
      colorPal<- getFactorColorPalette(cc)  
      col_vec <- colorPal(cc)
      names(col_vec) <- cc
    } else {
      stop(sprintf("invalid data type for column '%s': %s (must be numeric, factor or character)", colorCName, data.class(emeta.df[, colorCName])))
    }
    
  } else { # check that supplied color palette is applicable to the colorCName
    if (is.numeric(emeta.df[, colorCName])) {
      if (!(attr(colorPal, "colorType") %in% c("numeric", "bin", "quantile"))) {
        stop("the provided color palette is not applicable to the chosen color column")
      }
      pal_domain <- sort(get("domain", environment(colorPal)))
      vals <- seq(pal_domain[1], pal_domain[2], length=100)
    } else if (class(emeta.df[, colorCName]) %in% c("factor", "character")) {
      if (attr(colorPal, "colorType") != "factor") {
        stop("the provided color palette is not applicable to the chosen color column")
      } 
      vals <- get("domain", environment(colorPal))
      if (all(is.null(vals))) vals <- get("levels", environment(colorPal))
      if (all(is.null(vals))) vals <- unique(emeta.df[, colorCName])
    }
    col_vec <- colorPal(vals)
    names(col_vec) <- vals
  }
  
  # Include only rows (peaks) where that are observed in at least one column of e_data
  samp_cnames <- as.character(dataObj$f_data[, getFDataColName(dataObj)])
  if(length(samp_cnames) == 1){
    ind <- dataObj$e_data[,samp_cnames] >0
  }else{
    ind <- rowSums(dataObj$e_data[, samp_cnames]) > 0
  }
  
  obs.peaks <- as.character(dataObj$e_data[ind, getEDataColName(dataObj)])
  emeta.df <- emeta.df[which(emeta.df[,getEDataColName(dataObj)] %in% obs.peaks), ]
  
  ind.na <- is.na(emeta.df[,OC.col]) | is.na(emeta.df[,HC.col])
  emeta.df <- emeta.df[!ind.na, ]
  
  # Show Mass and Molecular Formula
  hovertext <- paste("Molecular Formula:", emeta.df[, getMFColName(dataObj)],"<br>Mass:", emeta.df[,getMassColName(dataObj)])
  if (!is.numeric(emeta.df[, colorCName])) {
    p <- plot_ly(emeta.df, x=emeta.df[,OC.col], y=emeta.df[,HC.col]) %>%
      add_markers(color=emeta.df[,colorCName], colors=col_vec, text=hovertext, hoverinfo="text") %>%
      layout(xaxis=list(title=xlabel, range=nice_axis_limits(emeta.df[, OC.col], zero.min=TRUE)), 
             yaxis=list(title=ylabel, range=nice_axis_limits(emeta.df[, HC.col], zero.min=TRUE)))
  } else { #NUMERIC COLOR COLUMN:
    p <- plot_ly(emeta.df, x=emeta.df[,OC.col], y=emeta.df[,HC.col]) %>%
      add_markers(color=emeta.df[,colorCName], colors=col_vec, text=hovertext, hoverinfo="text",
                  marker = list(colorbar = list(title = legendTitle))) %>%
      layout(xaxis=list(title=xlabel, range=nice_axis_limits(emeta.df[, OC.col], zero.min=TRUE)), 
             yaxis=list(title=ylabel, range=nice_axis_limits(emeta.df[, HC.col], zero.min=TRUE)))
  }

  if (showVKBounds) {
    if (vk_color_different_than_pts) {
      # if vk boxes are drawn but points are not colored according to vk category, color boxes black
      p <- p %>%
        add_segments(x=~x0, y=~y0, xend=~x1, yend=~y1, data=vankrev_categories, 
                     text=vankrev_categories$category, hoverinfo="text", line=list(color="#000000"), showlegend = FALSE) 
    } else {
      p <- p %>%
        add_segments(x=~x0, y=~y0, xend=~x1, yend=~y1, data=vankrev_categories, color=~category,
                     text=vankrev_categories$category, hoverinfo="text", showlegend = FALSE) 
      
    }
  }
  if (!is.na(title)) {
    p <- p %>% layout(title=title)
  }
  p
}
#vanKrevelenPlot(dataObj, title=colnames(dataObj$e_data)[2])

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


getFactorColorPalette <- function(level_names) {
  if (length(level_names) > 12) 
    stop("too many levels to infer a color scheme, please provide a colorPal parameter")
  else if (length(level_names) > 9)
    pal_name <- "Set3"
  else 
    pal_name <- "Set1"
  colorPal<- scales::col_factor(pal_name, levels=level_names)   
  return(colorPal)
}
