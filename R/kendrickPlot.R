#' Kendrick plot of icrData object
#' 
#' Constructs a Kendrick plot (Kendrick defect vs Kendrick mass) for an
#' object of type icrData.
#' 
#' @param dataObj icrData object
#' @param title plot title
#' @param colorPal color palette function, one of \code{\link{col_numeric}}, \code{\link{col_factor}} or similar \code{scales} palette function
#' @param colorCName column name of \code{dataObj$e_meta} to use for coloring the points. If NA and vkBoundarySet is provided, the points will be colored according to the Van Krevelen category.
#' @param vkBoundarySet character vector specifying which boundary set to use when determining class. Valid options are currently "bs1" and "bs2" and defaults to "bs1". See details of \code{\link{assign_class}} for differences in sets.
#' @param xlabel x axis label, default is "Kendrick Mass"
#' @param ylabel y axis label, default is "Kendrick Defect"
#' @param legendTitle title for the legend, only used when coloring points according to a numeric scale
#' @return a plotly object
#' @seealso \code{\link{plot_ly}}
#' @export
kendrickPlot <- function(dataObj, title=NA, colorPal=NA, colorCName=NA, vkBoundarySet = "bs1", 
                         xlabel="Kendrick Mass", ylabel="Kendrick Defect", legendTitle=colorCName) {
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
  
  km_col <- getKendrickMassColName(dataObj)
  if (is.null(km_col) | !is.element(km_col, colnames(dataObj$e_meta))) {
    stop("Kendrick mass column attribute is not set or is not present in dataObj$e_meta")
  }
  kd_col <- getKendrickDefectColName(dataObj)
  if (is.null(kd_col) | !is.element(kd_col, colnames(dataObj$e_meta))) {
    stop("Kendrick defect column attribute is not set or is not present in dataObj$e_meta")
  }
  
  if (is.na(colorCName) & is.na(vkBoundarySet)) {
    stop("at least one of colorCName or vkBoundarySet must be specified")
  }
  
  emeta.df <- dataObj$e_meta
  emeta.df[,getEDataColName(dataObj)] = as.character(emeta.df[,getEDataColName(dataObj)])
  
  # Van Krevelen categories
  if (!is.na(vkBoundarySet) & is.na(colorCName)) {
    vk_cat <- factor(getVanKrevelenCategories(dataObj, vkBoundarySet), levels=rownames(getVanKrevelenCategoryBounds(vkBoundarySet)))
    emeta.df$vk_categories_really_long_name1234 <- vk_cat
    colorCName <- "vk_categories_really_long_name1234"
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
  
  ind.na <- is.na(emeta.df[,km_col]) | is.na(emeta.df[,kd_col])
  emeta.df <- emeta.df[!ind.na, ]
  
  ed_id = getEDataColName(dataObj)
  # Mouseover text (TODO add function parameter for this)
  #hovertext <- sprintf("%s=%s<br>Kendrick Mass=%1.3f<br>Kendrick Defect=%1.3f", ed_id, emeta.df[,ed_id], emeta.df[,km_col], emeta.df[,kd_col])
  hovertext <- paste("Molecular Formula: ", emeta.df[, getMFColName(dataObj)],"<br>", getEDataColName(dataObj),": ", emeta.df[,getEDataColName(dataObj)], sep = "")
  
  if (!is.numeric(emeta.df[, colorCName])) {
    p <- plot_ly(emeta.df, x=emeta.df[,km_col], y=emeta.df[,kd_col]) %>%
#      add_trace(name=legendTitle, mode="none", type="scatter", opacity=0) %>% # empty trace in order to add legend title
      add_markers(color=emeta.df[, colorCName], colors=col_vec, text=hovertext, hoverinfo="text") %>%
      layout(xaxis=list(title=xlabel, range=nice_axis_limits(emeta.df[, km_col])), 
             yaxis=list(title=ylabel, range=nice_axis_limits(emeta.df[, kd_col])))
  } else { #NUMERIC COLOR COLUMN:
    p <- plot_ly(emeta.df, x=emeta.df[,km_col], y=emeta.df[,kd_col]) %>%
      add_markers(color=emeta.df[, colorCName], colors=col_vec, text=hovertext, hoverinfo="text",
                  marker = list(colorbar = list(title = legendTitle))) %>%
      layout(xaxis=list(title=xlabel, range=nice_axis_limits(emeta.df[, km_col])), 
             yaxis=list(title=ylabel, range=nice_axis_limits(emeta.df[, kd_col])))
  }
  
  
  if (!is.na(title)) {
    p <- p %>% layout(title=title)
  }
  p
  
}

#kendrickPlot(dataObj, title=colnames(dataObj$e_data)[2])
