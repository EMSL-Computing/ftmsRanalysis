#' Plot method for peakData objects
#' 
#' Depending on the scale of the object provided, this function with either construct
#' a barplot of the number of peaks observed for each sample (presence/absence data)
#' or boxplot of values of observed peaks per sample (all others). Samples will be 
#' colored by group, if grouping information is present in \code{ftmsObj}.
#'
#' @param ftmsObj peakData object
#' @param title optional title for the plot
#' @param xlabel optional label for X axis, if not provided "Sample" will be used
#' @param ylabel optional label for Y axis, if not provided a label will be constructed based on data scale (e.g. "Abundance" or "Number Observed")
#' @param colorBy "groups" to color by groups, "molform" for whether or not the peak has a molecular formula (presence/absence only), or NA to make all bars the same color
#'
#' @return a \code{plotly} graph object
#' @export
#'
#' @examples
#' plot(edata_transform(examplePeakData, "log2"))
#' plot(edata_transform(exampleProcessedPeakData, "pres"))
plot.peakData <- function(ftmsObj, title=NA, xlabel=NA, ylabel=NA, colorBy="groups") {
  
  # Tests
  if (!inherits(ftmsObj, "peakData")) stop("ftmsObj must be of type 'peakData'")
  if (inherits(ftmsObj, "groupSummary") | inherits(ftmsObj, "comparisonSummary")) {
    stop("Cannot use this plotting function for 'groupSummary' and 'comparisonSummary' data")
  }
  data_scale <- getDataScale(ftmsObj)
  if (data_scale == "" | identical(data_scale, NULL) | identical(data_scale, NA)) {
    stop(sprintf("Unknown data scale '%s': this data is not appropriate for plotting with this function", data_scale))
  }
  if (!is.na(colorBy) & !(colorBy %in% c("groups", "molform"))) {
    stop("Invalid value for colorBy: must be one of 'groups', 'molform' or NA")
  }
  if (identical(colorBy, "molform") & data_scale != "pres") {
    stop("colorBy = 'molform' can only be used if data is presence/absence")
  }
  # end tests
  
  if (identical(xlabel, NA)) xlabel <- "Sample"
  if (identical(ylabel, NA)) {
    if (data_scale == "abundance") ylabel <- "Abundance"
    else if (data_scale == "pres") ylabel <- "Number Observed"
    else ylabel <- sprintf("%s(Abundance)", data_scale)
  }
  
  df <- ftmsObj$e_data %>% tidyr::gather("Sample", "Value", -dplyr::matches(getEDataColName(ftmsObj)))
  
  if (data_scale == "abundance" | data_scale == "pres") {
    df <- df %>% dplyr::filter(Value != 0 & !is.na(Value))
  } else {
    df <- df %>% dplyr::filter(!is.na(Value))
  }
  
  # If group information is present and colorBy == "groups", color bars by groups
  p <- plotly::plot_ly()
  grouped <- FALSE
  if (identical(colorBy, "groups")) {
    if (!is.null(ftmsRanalysis:::getGroupDF(ftmsObj))) {
      grouped <- TRUE
      groupDF <- ftmsRanalysis:::getGroupDF(ftmsObj)
      df <- df %>% dplyr::left_join(groupDF, by=c(Sample=getFDataColName(ftmsObj)))
    }
  }
  
  if (data_scale == "pres") { # do a barplot of num observed
    # browser()
    if (grouped) {
      counts <- df %>% dplyr::group_by(Group, Sample) %>% dplyr::summarize(Count=n()) %>% dplyr::ungroup()
      p <- p %>% plotly::add_bars(x=~Sample, y=~Count, color=~Group, data=counts, hoverinfo="y")
      
    } else if (identical(colorBy, "molform")) { # stacked barplots showing counts of molecular form vs not for each sample
      df2 <- df %>% 
        dplyr::left_join(dplyr::select(ftmsObj$e_meta, getEDataColName(ftmsObj), getMFColName(ftmsObj))) %>% 
        dplyr::mutate(HasMolForm=factor(!is.na(!!rlang::sym(getMFColName(ftmsObj))), levels = c(TRUE, FALSE)))
      
      counts <- df2 %>% 
        dplyr::group_by(Sample, HasMolForm, .drop = FALSE) %>% 
        dplyr::summarize(Count=n()) %>% 
        dplyr::ungroup() %>%
        tidyr::spread(HasMolForm, Count) %>%
        dplyr::rename(MolForm=`TRUE`, NoMolForm=`FALSE`)
      
      p <- p %>% 
        plotly::add_bars(data=counts, x=~Sample, y=~NoMolForm, name="No Molecular Form", marker=list(color="#d62728")) %>%
        plotly::add_bars(data=counts, x=~Sample, y=~MolForm, type="bar", name="Has Molecular Form", marker=list(color="#1f77b4")) %>%
        plotly::layout(yaxis = list(title = 'Count'), barmode = 'stack')
      
    } else {
      counts <- df %>% dplyr::group_by(Sample) %>% dplyr::summarize(Count=n()) %>% dplyr::ungroup()
      p <- p %>% plotly::add_bars(x=~Sample, y=~Count, data=counts, hoverinfo="y")
    }
  } else { # boxplot
    if (grouped) {
      p <- p %>%
        plotly::add_boxplot(x=~Sample, y=~Value, color=~Group, data=df)
    } else {
      p <- p %>%
        plotly::add_boxplot(x=~Sample, y=~Value, data=df)
    }
    
  }
  
  # axis styling
  ax <- list(
    zeroline = FALSE, # don't plot axes at zero
    showline = TRUE,
    mirror = "ticks" # makes box go all the way around not just bottom and left
  )
  
  p <- p %>%
    plotly::layout(xaxis=c(ax, list(title=xlabel), tickangle=-90, automargin=TRUE), yaxis=c(ax, list(title=ylabel)))
  
  if (!is.na(title)) {
    p <- p %>% plotly::layout(title=title)
  }
  
  p
}