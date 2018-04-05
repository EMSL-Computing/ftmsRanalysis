#' Kendrick plot of icrData object
#' 
#' Constructs a Kendrick plot (Kendrick defect vs Kendrick mass) for an
#' object of type icrData.
#' 
#' @param dataObj icrData object
#' @param title plot title
#' @param colorPal color palette function, one of \code{\link{col_numeric}}, \code{\link{col_factor}} or similar \code{scales} palette function
#' @param colorCName column name of \code{dataObj$e_data} to use for coloring the points
#' @param xlabel x axis label, default is "Kendrick Mass"
#' @param ylabel y axis label, default is "Kendrick Defect"
#' @param legendTitle title for the legend, only used when coloring points according to a numeric scale
#' @return a plotly object
#' @seealso \code{\link{plot_ly}}
#' @export
groupKendrickPlot <- function(dataObj, title=NA, colorPal=NA, colorCName=NA,
                         xlabel="Kendrick Mass", ylabel="Kendrick Defect", legendTitle=colorCName) {
  require(dplyr)
  require(plotly)
  require(scales)
  
  # Test inputs
  if (!inherits(dataObj, "groupSummary")) {
    stop("dataObj must be of type groupSummary")
  }
  if (!inherits(dataObj, "icrData")) {
    stop("dataObj must be of type icrData")
  }
  if (is.null(dataObj$e_meta)) {
    stop("dataObj must have e_meta element")
  }
  if ( inherits(dataObj, "groupComparison")) {
    stop("dataObj cannot be a groupComparison object for this function")
  }
  
  km_col <- getKendrickMassColName(dataObj)
  if (is.null(km_col) | !is.element(km_col, colnames(dataObj$e_meta))) {
    stop("Kendrick mass column attribute is not set or is not present in dataObj$e_meta")
  }
  kd_col <- getKendrickDefectColName(dataObj)
  if (is.null(kd_col) | !is.element(kd_col, colnames(dataObj$e_meta))) {
    stop("Kendrick defect column attribute is not set or is not present in dataObj$e_meta")
  }
  
  if (is.na(colorCName) | is.null(colorCName)) {
    stop("colorCName must be specified")
  }
  if (! (colorCName %in% colnames(dataObj$e_data))) {
    stop("colorCName must be a column of the e_data element")
  }
  
  edata.df <- dataObj$e_data
  edata.df[,getEDataColName(dataObj)] = as.character(edata.df[,getEDataColName(dataObj)])
  edata.df <- left_join(edata.df, 
                        dataObj$e_meta[, c(getEDataColName(dataObj), getKendrickMassColName(dataObj), 
                                           getKendrickDefectColName(dataObj), getMFColName(dataObj))])
  
  # # Van Krevelen categories
  # if (!is.na(vkBoundarySet) & is.na(colorCName)) {
  #   vk_cat <- factor(getVanKrevelenCategories(dataObj, vkBoundarySet), levels=rownames(getVanKrevelenCategoryBounds(vkBoundarySet)))
  #   edata.df$vk_categories_really_long_name1234 <- vk_cat
  #   colorCName <- "vk_categories_really_long_name1234"
  # }
  
  # color palette
  if (is.na(colorPal)) {
    if (is.numeric(edata.df[, colorCName])) {
      val_range <- range(edata.df[, colorCName], na.rm=TRUE)
      colorPal <- scales::col_numeric(palette="YlOrRd", domain=val_range)
      vals <- seq(val_range[1], val_range[2], length=100)
      col_vec <- colorPal(vals)
      names(col_vec) <- vals
    } else if (is.factor(edata.df[, colorCName])) {
      cc <- levels(edata.df[, colorCName])
      colorPal<- getFactorColorPalette(cc)  
      col_vec <- colorPal(cc)
      names(col_vec) <- cc
    } else if (is.character(edata.df[, colorCName])) {
      cc <- sort(unique(edata.df[, colorCName]))
      edata.df[, colorCName] <- factor(edata.df[, colorCName], levels = cc)
      colorPal<- getFactorColorPalette(cc)  
      col_vec <- colorPal(cc)
      names(col_vec) <- cc
    } else {
      stop(sprintf("invalid data type for column '%s': %s (must be numeric, factor or character)", colorCName, data.class(edata.df[, colorCName])))
    }
    
  } else { # check that supplied color palette is applicable to the colorCName
    if (is.numeric(edata.df[, colorCName])) {
      if (!(attr(colorPal, "colorType") %in% c("numeric", "bin", "quantile"))) {
        stop("the provided color palette is not applicable to the chosen color column")
      }
      pal_domain <- sort(get("domain", environment(colorPal)))
      vals <- seq(pal_domain[1], pal_domain[2], length=100)
    } else if (class(edata.df[, colorCName]) %in% c("factor", "character")) {
      if (attr(colorPal, "colorType") != "factor") {
        stop("the provided color palette is not applicable to the chosen color column")
      } 
      vals <- get("domain", environment(colorPal))
      if (all(is.null(vals))) vals <- get("levels", environment(colorPal))
      if (all(is.null(vals))) vals <- unique(edata.df[, colorCName])
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
  edata.df <- edata.df[which(edata.df[,getEDataColName(dataObj)] %in% obs.peaks), ]
  
  ind.na <- is.na(edata.df[,km_col]) | is.na(edata.df[,kd_col])
  edata.df <- edata.df[!ind.na, ]
  
  ed_id = getEDataColName(dataObj)
  # Mouseover text (TODO add function parameter for this)
  hovertext <- sprintf("%s<br>Kendrick Mass=%1.3f<br>Kendrick Defect=%1.3f", edata.df[,ed_id], edata.df[,km_col], edata.df[,kd_col])
  
  if (!is.numeric(edata.df[, colorCName])) {
    p <- plot_ly(edata.df, x=edata.df[,km_col], y=edata.df[,kd_col]) %>%
#      add_trace(name=legendTitle, mode="none", type="scatter", opacity=0) %>% # empty trace in order to add legend title
      add_markers(color=edata.df[, colorCName], colors=col_vec, text=hovertext, hoverinfo="text") %>%
      layout(xaxis=list(title=xlabel, range=nice_axis_limits(edata.df[, km_col])), 
             yaxis=list(title=ylabel, range=nice_axis_limits(edata.df[, kd_col])))
  } else { #NUMERIC COLOR COLUMN:
    p <- plot_ly(edata.df, x=edata.df[,km_col], y=edata.df[,kd_col]) %>%
      add_markers(color=edata.df[, colorCName], colors=col_vec, text=hovertext, hoverinfo="text",
                  marker = list(colorbar = list(title = legendTitle))) %>%
      layout(xaxis=list(title=xlabel, range=nice_axis_limits(edata.df[, km_col])), 
             yaxis=list(title=ylabel, range=nice_axis_limits(edata.df[, kd_col])))
  }
  
  
  if (!is.na(title)) {
    p <- p %>% layout(title=title)
  }
  p
  
}

#kendrickPlot(dataObj, title=colnames(dataObj$e_data)[2])
