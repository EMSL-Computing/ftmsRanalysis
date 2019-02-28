#' Title
#'
#' @param icrData an object of class 'peakData' or 'compoundIcrData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}.
#' @param xCName column name for x-axis, must be a column of \code{icrData$e_data} or \code{icrData$e_meta}
#' @param yCName column name for y-axis, must be a column of \code{icrData$e_data} or \code{icrData$e_meta}
#' @param colorCName column name for point colors, must be a column of \code{icrData$e_data} or \code{icrData$e_meta}
#' @param colorPal color palette function, one of \code{\link{col_numeric}}, \code{\link{col_factor}} or similar \code{scales} palette function
#' @param xlabel x axis label, default is \code{xCName}
#' @param ylabel y axis label, default is \code{yCName}
#' @param legendTitle title for the legend, only used when coloring points according to a numeric scale
#' @param title plot title
#' @param xrange x-axis bounds
#' @param yrange y-axis bounds
#' @param logColorCol TRUE/FALSE, should the color column be log-transformed? Default is FALSE.
#' @param hoverTextCName column name for hover (mouseover) text, must be a column of \code{icrData$e_data} or \code{icrData$e_meta}
#'
#' @return plotly object
#' @export
#'
#' @examples
#' scatterPlot(peakIcrProcessed, "NOSC", "DBE", colorCName="HtoC_ratio", legendTitle="H:C Ratio", title="DBE vs NOSC for peakIcrProcessed")
scatterPlot <- function(icrData, xCName, yCName, colorCName=NA, colorPal=NA, xlabel=xCName, ylabel=yCName, 
                        legendTitle=colorCName, title=NA, xrange=NA, yrange=NA, logColorCol=FALSE, hoverTextCName=NA) {

  if (missing(icrData)) stop("icrData must be provided")
  if (missing(xCName)) stop("xCName must be provided")
  if (missing(yCName)) stop("yCName must be provided")
  
  if (xCName %in% colnames(icrData$e_data)) {
    plot_data <- icrData$e_data[, c(getEDataColName(icrData), xCName)]
  } else if (xCName %in% colnames(icrData$e_meta)) {
    plot_data <- icrData$e_meta[, c(getEDataColName(icrData), xCName)]
  } else {
    stop("xCName must be a column name in e_data or e_meta")
  }
  if (!is.numeric(plot_data[, xCName])) stop("xCName must be a numeric column")
  
  if (yCName %in% colnames(icrData$e_data)) {
    plot_data <- dplyr::full_join(plot_data, icrData$e_data[, c(getEDataColName(icrData), yCName)], by=getEDataColName(icrData))
  } else if (yCName %in% colnames(icrData$e_meta)) {
    plot_data <- dplyr::full_join(plot_data, icrData$e_meta[, c(getEDataColName(icrData), yCName)], by=getEDataColName(icrData))
  } else {
    stop("yCName must be a column name in e_data or e_meta")
  }
  if (!is.numeric(plot_data[, yCName])) stop("yCName must be a numeric column")
  
  if (!is.na(hoverTextCName) & !(hoverTextCName %in% colnames(plot_data))) {
    if (hoverTextCName %in% colnames(icrData$e_data)) {
      plot_data <- dplyr::full_join(plot_data, icrData$e_data[, c(getEDataColName(icrData), hoverTextCName)], by=getEDataColName(icrData))
    } else if (hoverTextCName %in% colnames(icrData$e_meta)) {
      plot_data <- dplyr::full_join(plot_data, icrData$e_meta[, c(getEDataColName(icrData), hoverTextCName)], by=getEDataColName(icrData))
    } else {
      stop("hoverTextCName must be a column name in e_data or e_meta")
    }
  }
  
  if (!is.na(colorCName)) {
    if (colorCName %in% colnames(plot_data)) {
      # do nothing
    } else if (colorCName %in% colnames(icrData$e_data)) {
      plot_data <- dplyr::full_join(plot_data, icrData$e_data[, c(getEDataColName(icrData), colorCName)], by=getEDataColName(icrData))
    } else if (colorCName %in% colnames(icrData$e_meta)) {
      plot_data <- dplyr::full_join(plot_data, icrData$e_meta[, c(getEDataColName(icrData), colorCName)], by=getEDataColName(icrData))
    } else {
      stop("colorCName must be a column name in e_data or e_meta")
    }

    # color palette
    if (identical(colorPal, NA)) {
      if (is.integer(plot_data[, colorCName])) { # probably count data
        val_range <- range(plot_data[, colorCName], na.rm=TRUE)
        vals <- seq(val_range[1], val_range[2], by=1)
        pal = RColorBrewer::brewer.pal(n = 9, "YlOrRd")[3:9]
        colorPal <- scales::col_factor(palette=pal, domain=vals)
        col_vec <- colorPal(vals)
        names(col_vec) <- vals
        plot_data[, colorCName] <- factor(plot_data[, colorCName], levels=sort(unique(plot_data[, colorCName])))
      } else if (is.numeric(plot_data[, colorCName])) {
        val_range <- range(plot_data[, colorCName], na.rm=TRUE)
        pal = RColorBrewer::brewer.pal(n = 9, "YlOrRd")[3:9]
        if (logColorCol) { #log transform data
          if (min(val_range) == 0) {
            val_range2 <- log(1+val_range)
            plot_data[, colorCName] <- log(1+plot_data[, colorCName])
          } else {
            val_range2 <- log(val_range)
            plot_data[, colorCName] <- log(plot_data[, colorCName])
          }
          colorPal <- scales::col_numeric(palette=pal, domain=val_range2)
          vals <- seq(val_range2[1], val_range2[2], length=100)
          col_vec <- colorPal(vals)
          names(col_vec) <- seq(val_range2[1], val_range2[2], length=100)
        } else {
          colorPal <- scales::col_numeric(palette=pal, domain=val_range)
          vals <- seq(val_range[1], val_range[2], length=100)
          col_vec <- colorPal(vals)
          names(col_vec) <- vals
        }
      } else if (is.factor(plot_data[, colorCName])) {
        cc <- levels(plot_data[, colorCName])
        if (any(is.na(plot_data[, colorCName]))) {
          cc <- c(cc, NA)
          new_vals <- as.character(plot_data[, colorCName])
          new_vals[is.na(new_vals)] <- "NA"
          plot_data[, colorCName] <- factor(new_vals, levels=cc)
        }
        colorPal<- getFactorColorPalette(cc)  
        col_vec <- colorPal(cc)
        names(col_vec) <- cc
      } else if (is.character(plot_data[, colorCName])) {
        cc <- sort(unique(plot_data[, colorCName]), na.last = TRUE)
        cc[is.na(cc)] <- "NA"
        plot_data[is.na(plot_data[, colorCName]), colorCName] <-"NA"
        plot_data[, colorCName] <- factor(plot_data[, colorCName], levels = cc)
        colorPal<- getFactorColorPalette(cc)  
        col_vec <- colorPal(cc)
        names(col_vec) <- cc
      } else {
        stop(sprintf("invalid data type for column '%s': %s (must be numeric, factor or character)", colorCName, data.class(plot_data[, colorCName])))
      }
    } else { # check that supplied color palette is applicable to the colorCName
      if (is.numeric(plot_data[, colorCName])) {
        if (!(attr(colorPal, "colorType") %in% c("numeric", "bin", "quantile"))) {
          stop("the provided color palette is not applicable to the chosen color column")
        }
        pal_domain <- sort(get("domain", environment(colorPal)))
        vals <- seq(pal_domain[1], pal_domain[2], length=100)
      } else if (class(plot_data[, colorCName]) %in% c("factor", "character")) {
        if (attr(colorPal, "colorType") != "factor") {
          stop("the provided color palette is not applicable to the chosen color column")
        } 
        vals <- get("domain", environment(colorPal))
        if (all(is.null(vals))) vals <- get("levels", environment(colorPal))
        if (all(is.null(vals))) vals <- unique(plot_data[, colorCName])
      }
      col_vec <- colorPal(vals)
      names(col_vec) <- vals
    }
  } else if (!identical(colorPal, NA) & is.character(colorPal)) {
    const_color <- colorPal
  } else {
    const_color <- NA
  }
  
  # Include only rows (peaks) where that are observed in at least one column of e_data
  samp_cnames <- as.character(icrData$f_data[, getFDataColName(icrData)])
  ind <- fticRanalysis:::n_present(icrData$e_data[,samp_cnames], getDataScale(icrData))[,1] > 0
  
  obs.peaks <- as.character(icrData$e_data[ind, getEDataColName(icrData)])
  plot_data <- plot_data[which(plot_data[,getEDataColName(icrData)] %in% obs.peaks), ]
  
  # remove rows where x or y value is NA
  ind.na <- is.na(plot_data[,xCName]) | is.na(plot_data[,yCName])
  plot_data <- plot_data[!ind.na, ]
  
  if (identical(xrange, NA)) {
    xrange <- fticRanalysis:::nice_axis_limits(plot_data[, xCName])
  }
  if (identical(yrange, NA)) {
    yrange <- fticRanalysis:::nice_axis_limits(plot_data[, yCName])
  }

  p <- plotly::plot_ly(plot_data, x=plot_data[,xCName], y=plot_data[,yCName]) 
  marker_parms <- list(a=p, key=plot_data[, getEDataColName(icrData)])
  names(marker_parms)[1] <- ""
  
  if (!is.na(colorCName)) {
#    if (!is.numeric(plot_data[, colorCName]) || is.integer(plot_data[, colorCName])) { # FACTOR OR INTEGER COLOR:
      marker_parms <- c(marker_parms, list(color=plot_data[,colorCName], colors=col_vec))
#    } else { #NUMERIC COLOR COLUMN:
#      marker_parms <- c(marker_parms, list(color=plot_data[,colorCName], colors=col_vec, marker = list(colorbar = list(title = legendTitle))))
#    }
  } else { #NO COLOR COLUMN:
    if (!identical(const_color, NA)) {
      marker_parms <- c(marker_parms, list(marker=list(color=const_color)))
    }
  }
  if (!identical(hoverTextCName, NA)) {
    marker_parms <- c(marker_parms, list(text=plot_data[, hoverTextCName], hoverinfo="text"))
  }

  p <- do.call(plotly::add_markers, marker_parms)
  
  # colorbar title:
  if (!is.na(colorCName) & !is.na(legendTitle) & nchar(legendTitle) > 0) {
    if (is.numeric(plot_data[, colorCName]) && !is.integer(plot_data[, colorCName])) { # NUMERIC NOT INTEGER
      p <- p %>%
        plotly::colorbar(title=legendTitle, which=1)
    }
  }
  
  # axis styling
  ax <- list(
    zeroline = FALSE, # don't plot axes at zero
    showline = TRUE,
    mirror = "ticks" # makes box go all the way around not just bottom and left
#    gridcolor = "#eee", # change grid color, "#eee" is default
#    linewidth = 2 # makes box around plot area slightly wider
  )
  
  p <- p %>%
    plotly::layout(xaxis=c(ax, list(title=xlabel, range=xrange)), yaxis=c(ax, list(title=ylabel, range=yrange)))
  
  if (!is.na(title)) {
    p <- p %>% plotly::layout(title=title)
  }
  return(p)
  
}

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
  pal_colors <- RColorBrewer::brewer.pal(length(level_names), pal_name)
  colorPal<- scales::col_factor(pal_colors, levels=level_names)   
  return(colorPal)
}
