#' Density plot of quantitative characteristic of peaks for a peakData or compoundData object
#' 
#' Constructs a density plot for a calculated variable in \code{e_meta} portion of \code{icrData} object
#' 
#' @param icrData icrData object of class peakData or compoundData
#' @param variable column name of column in \code{e_meta} which should be plotted. Must be one of the column names in \code{icrData$e_meta} that contains numeric values.
#' @param samples vector of sample names to plot. Default of \code{NA} indicates all samples found in \code{icrData} should be plotted. Specify \code{samples=FALSE} to plot no samples, only groups.
#' @param groups vector of group names to plot. Value of \code{NA} indicates all groups found in \code{icrData} should be plotted. Default value of \code{groups=FALSE} to plot no groups, only samples.
#' @param title plot title, default is NA (no title)
#' @param yaxis what should the y-axis represent, "density" or "count"? 
#' @param plot_hist TRUE/FALSE should a histogram be added to the plot? A histogram may only be added if a single sample or groups is plotted.
#' @param plot_curve TRUE/FALSE should a smoothed curve be added to the plot?
#' @param curve_colors named vector of colors for curves, where names correspond to samples or groups chosen. Named colors or hex codes are accepted.
#' @param hist_color (single) color for histogram bars. Named colors or hex codes are accepted.
#' @param xlabel x axis label, default is NULL, which will result in the same name as "variable"
#' @param ylabel y axis label, default is "Density"
#' 
#' @return a plotly object
#' 
#' @details If a histogram is produced, the histogram bar data as a data frame is attached to the output as the attribute \code{hist_data}.
#' This may be useful for adding additional data to the plot, e.g. for linking selections in multiple plots.
#' 
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Allison Thompson, Lisa Bramer, Amanda White
#' @importFrom dplyr %>%
#' @export
densityPlot <- function(icrData, variable, samples=NA, groups=FALSE, title=NA, 
                            yaxis="density", plot_hist=FALSE, plot_curve=TRUE, 
                            curve_colors=NA, hist_color="gray", 
                            xlabel=NA, ylabel=paste0(toupper(substring(yaxis, 1,1)), substring(yaxis,2), sep="")) {
  
  # Test inputs #
  if (!inherits(icrData, "peakData") & !inherits(icrData, "compoundData")) {
    stop("icrData must be of type 'peakData' or 'compoundData'")
  }
  
  if (inherits(icrData, "comparisonSummary")) {
    stop("icrData must not be a comparisonSummary object")
  }
  
  if (is.null(variable) | length(which(colnames(icrData$e_meta) == variable)) != 1) {
    stop("variable must be a single column name found in icrData$e_meta")
  }
  
  if (is.null(yaxis) | !(yaxis %in% c("count", "density"))) {
    stop("yaxis must be either 'density' or 'count'")
  }
  
  if(is.na(xlabel)){
    xlabel <- variable
  }
  
  # merge e_data and e_meta #  
  df <- merge(icrData$e_data, icrData$e_meta, by=getMassColName(icrData))
  
  # construct a list with the samples for each plot trace
  trace_subsets <- list()
  if (identical(groups, NA)) {
    if (is.null(getGroupDF(icrData)$Group))
      groups <- FALSE
    else
      groups <- unique(getGroupDF(icrData)$Group)
  }
  if (!identical(groups, FALSE)) {
    tmp <- getGroupDF(icrData) %>% 
      dplyr::group_by(Group) %>% 
      dplyr::filter(Group %in% groups) %>%
      dplyr::rename(Sample=!!getFDataColName(icrData)) %>%
      dplyr::summarize(samples=list(as.character(unique(Sample))))
    trace_subsets <- tmp$samples
    names(trace_subsets) <- tmp$Group
  }
  if (identical(samples, NA)) {
    samples <- intersect(colnames(icrData$e_data), icrData$f_data[, getFDataColName(icrData)])
  }
  if (!identical(samples, FALSE)) {
    tmp <- as.list(samples)
    names(tmp) <- samples
    trace_subsets <- c(trace_subsets, tmp)
  }
  if (length(trace_subsets) == 0) {
    stop("No valid samples or groups specified")
  } else if (length(trace_subsets) > 1 & plot_hist) {
    warning("Cannot plot histograms when more than one sample or group is chosen")
    plot_hist <- FALSE
  }
  
  # histogram breaks
  breaks <- seq(min(df[, variable], na.rm=TRUE), max(df[, variable], na.rm=TRUE), length=26)
  
  hist_data <- lapply(names(trace_subsets), function(trace_name) {
    res <- fticRanalysis:::get_hist_data(df[, trace_subsets[[trace_name]]], df[, variable], getDataScale(icrData), breaks)
    res$Category <- trace_name
    return(res)
  })
  hist_data <- do.call(rbind, hist_data)
  
  curve_data <- lapply(names(trace_subsets), function(trace_name) {
    res <- fticRanalysis:::get_curve_data(df[, trace_subsets[[trace_name]]], df[, variable], getDataScale(icrData), yaxis=yaxis, nbins=512)
    res$count <- res$density*nrow(df)*(breaks[2]-breaks[1])
    res$Category <- trace_name
    return(res)
  })
  curve_data <- do.call(rbind, curve_data)
  
  if (yaxis == "density") {
    curve_data <- dplyr::rename(curve_data, y=density)
    hist_data <- dplyr::rename(hist_data, y=density)
  } else if (yaxis == "count") {
    curve_data <- dplyr::rename(curve_data, y=count)
    hist_data <- dplyr::rename(hist_data, y=count)
  }  
  yrange <- c(NA, NA)
  p <- plotly::plot_ly()
  if (plot_hist) {
    p <- p %>% plotly::add_bars(x=~x, y=~y, width=~barwidth, key=~key, data=hist_data, 
                        marker=list(color=hist_color), showlegend=FALSE) 
  }
  
  if (plot_curve) {
    if (identical(curve_colors, NA)) {
      curve_colors <- fticRanalysis:::get_curve_colors(names(trace_subsets))
    }
    if (length(trace_subsets) > 1) {
      p <- p %>% plotly::add_lines(x=~x, y=~y, color=~Category, data=curve_data, #alpha=0.5, 
                           showlegend=TRUE, colors=curve_colors)
    } else {
      p <- p %>% plotly::add_lines(x=~x, y=~y, data=curve_data, #alpha=0.5, 
                           showlegend=FALSE, line=list(color=curve_colors[1]))
    }
  }
  
  # axis styling
  ax <- list(
    zeroline = FALSE, # don't plot axes at zero
    showline = TRUE,
    mirror = "ticks" # makes box go all the way around not just bottom and left
  )
  
  p <- p %>% plotly::layout(barmode="overlay", xaxis=c(ax, list(title=xlabel)), 
                    yaxis=c(ax, list(title=ylabel)))#, range=nice_axis_limits(hist_data$y)))
  
  if(!is.na(title)){
    p <- p %>%
      plotly::layout(title=title)
  }
  
  if (plot_hist) {
    if (yaxis=="count") hist_data <- dplyr::select(hist_data, -density)
    if (yaxis=="density") hist_data <- dplyr::select(hist_data, -count)
    attr(p, "hist_data") <- hist_data
  }
  
  p
}

# Internal only helper functions

get_data_vector <- function(edata_cols, variable_vec, data_scale) {
  # get num present for each peak
  #browser()
  counts <- fticRanalysis:::n_present(edata_cols, data_scale)
  counts$index <- 1:nrow(counts)
  indices <- unlist(apply(counts, 1, function(x) rep(x["index"], times=x["n_present"])))
  
  # replicate points according to how many samples in which they are observed
  data_vec <- variable_vec[indices]
  data_vec <- data_vec[!is.na(data_vec)]
  return(data_vec)
}

get_hist_data <- function(edata_cols, variable_vec, data_scale, breaks) {
  data_vec <- get_data_vector(edata_cols, variable_vec, data_scale)
  hist_data <- hist(data_vec, breaks = breaks, plot = FALSE)
  keys <- lapply(1:length(hist_data$mids), function(i) as.numeric(hist_data$breaks[i:(i+1)]))
  hist_data <- tibble:::tibble(x=hist_data$mids, count=hist_data$counts, density=hist_data$density, 
                               barwidth=diff(hist_data$breaks), key=keys)
  return(hist_data)
}


get_curve_data <- function(edata_cols, variable_vec, data_scale, yaxis="density", nbins=512) {
  data_vec <- get_data_vector(edata_cols, variable_vec, data_scale)
  
  dens_curve <- stats::density(x=data_vec, weights=rep(1/length(data_vec), length=length(data_vec)), 
                               from = min(data_vec, na.rm=TRUE), to = max(data_vec, na.rm=TRUE), 
                               bw = "nrd0", adjust = 1, kernel = "gaussian", n = nbins)
  dens_curve <- as.data.frame(dens_curve[c("x", "y")])
  colnames(dens_curve) <- c("x", "density")
  return(dens_curve)
}

get_curve_colors <- function(trace_names) {
  if (length(trace_names) == 1) {
    colorPal <- c("blue")
    names(colorPal) <- trace_names
    return(colorPal)
  } else if (length(trace_names) > 12) {
    stop("too many curves to infer a color scheme")
  } else if (length(trace_names) > 9) {
    pal_name <- "Set3"
    pal_colors <- RColorBrewer::brewer.pal(12, "Set3")[1:length(trace_names)]
  } else {
    pal_colors <- RColorBrewer::brewer.pal(9, "Set1")[1:length(trace_names)]
  }
  names(pal_colors) <- trace_names
  return(pal_colors)
}
