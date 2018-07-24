# icrData <- peakIcrData
# 
# icrData <- edata_transform(peakIcrData, "log2")
# 
# xlabel <- "Sample"
# ylabel <- NA
# title <- "My Title"

#' Plot method for peakIcrData objects
#' 
#' Depending on the scale of the object provided, this function with either construct
#' a barplot of the number of peaks observed for each sample (presence/absence data)
#' or boxplot of values of observed peaks per sample (all others). Samples will be 
#' colored by group, if grouping information is present in \code{icrData}.
#'
#' @param icrData peakIcrData object
#' @param title optional title for the plot
#' @param xlabel optional label for X axis, if not provided "Sample" will be used
#' @param ylabel optional label for Y axis, if not provided a label will be constructed based on data scale (e.g. "Abundance" or "Number Observed")
#'
#' @return a \code{plotly} graph object
#' @export
#'
#' @examples
#' plot(edata_transform(peakIcrData, "log2"))
#' plot(edata_transform(peakIcrProcessed, "pres"))
plot.peakIcrData <- function(icrData, title=NA, xlabel=NA, ylabel=NA) {
  require(plotly)
  
  # Tests
  if (!inherits(icrData, "peakIcrData")) stop("icrData must be of type peakIcrData")
  if (inherits(icrData, "groupSummary") | inherits(icrData, "comparisonSummary")) {
    stop("Cannot use this plotting function for 'groupSummary' and 'comparisonSummary' data")
  }
  data_scale <- getDataScale(icrData)
  if (data_scale == "" | identical(data_scale, NULL) | identical(data_scale, NA)) {
    stop(sprintf("Unknown data scale '%s': this data is not appropriate for plotting with this function", data_scale))
  }
  # end tests
  
  if (identical(xlabel, NA)) xlabel <- "Sample"
  if (identical(ylabel, NA)) {
    if (data_scale == "abundance") ylabel <- "Abundance"
    else if (data_scale == "pres") ylabel <- "Number Observed"
    else ylabel <- sprintf("%s(Abundance)", data_scale)
  }
  
  df <- icrData$e_data %>% tidyr::gather("Sample", "Value", -dplyr::matches(getEDataColName(icrData)))
  
  if (data_scale == "abundance" | data_scale == "pres") {
    df <- df %>% dplyr::filter(Value != 0 & !is.na(Value))
  } else {
    df <- df %>% dplyr::filter(!is.na(Value))
  }
  
  # If group information is present, color by groups
  p <- plot_ly()
  grouped <- FALSE
  if (!is.null(fticRanalysis:::getGroupDF(icrData))) {
    grouped <- TRUE
    groupDF <- fticRanalysis:::getGroupDF(icrData)
    df <- df %>% dplyr::left_join(groupDF, by=c(Sample=getFDataColName(icrData)))
  }

  if (data_scale == "pres") { # do a barplot of num observed
    if (grouped) {
      counts <- df %>% group_by(Group, Sample) %>% summarize(Count=n()) %>% ungroup()
      p <- p %>% add_bars(x=~Sample, y=~Count, color=~Group, data=counts)
    } else {
      counts <- df %>% group_by(Sample) %>% summarize(Count=n()) %>% ungroup()
      p <- p %>% add_bars(x=~Sample, y=~Count, data=counts)
    }
  } else { # boxplot
    if (grouped) {
      p <- p %>%
        add_boxplot(x=~Sample, y=~Value, color=~Group, data=df)
    } else {
      p <- p %>%
        add_boxplot(x=~Sample, y=~Value, data=df)
    }
    
  }

  # axis styling
  ax <- list(
    zeroline = FALSE, # don't plot axes at zero
    showline = TRUE,
    mirror = "ticks" # makes box go all the way around not just bottom and left
  )
  
  p <- p %>%
    layout(xaxis=c(ax, list(title=xlabel), tickangle=-90, automargin=TRUE), yaxis=c(ax, list(title=ylabel)))
  
  if (!is.na(title)) {
    p <- p %>% layout(title=title)
  }
  
  p
}

