#' Default plot method for emetaFilt
#'
#' @param x object of class \code{\link{emeta_filter}}
#' @param min_val a numeric value specifying the minimum value (inclusive) that a peak should have for the specified 'e_meta' column
#' @param max_val a numeric value specifying the maximum value (inclusive) that a peak should have for the specified 'e_meta' column
#' @param cats a vector of character values specifying the level(s) of the specified 'e_meta' column which should be retained
#' @param na.rm logical value specifying if peaks with NA values for the specified 'e_meta' column should be removed. Default value is TRUE
#' @param title plot title
#' @param xlabel x-axis label, default is the column of emeta used to construct \code{x}
#' @param ylabel y-axis label
#' @param ... other arguments
#' @return \code{plotly} plot object
#' 
#' @rdname plot.emetaFilt
#' @name plot.emetaFilt
#' @export
#'
#' @examples
#' \dontrun{
#' # Emeta filter based on the "OtoC_ratio" column (numeric variable)
#' filter_object1 = emeta_filter(exampleProcessedPeakData, cname = "OtoC_ratio")
#' 
#' # Plots the distribution of OtoC_ratio
#' plot(filter_object1)
#' 
#' # Demonstrates retaining values between 0.5 and 1
#' plot(filter_object1, min_val = 0.5, max_val = 1)
#' 
#' # Emeta filter based on the "MolForm" column (categorical variable)
#' filter_object2 = emeta_filter(exampleProcessedPeakData, cname = "MolForm")
#' 
#' # Only non-NAs retained
#' plot(filter_object2)
#' 
#' # Peaks matching "C10" retained
#' plot(filter_object2, cats=grep("C10", filter_object2$emeta_value, value = TRUE))
#' }
plot.emetaFilt <- function(x, min_val = NA, max_val = NA, cats = NA, na.rm = TRUE, 
                           title=NA, xlabel=attr(x, "cname"), ylabel="Count", ...) {
  if (attr(x, "type") == "quantitative") {
    ftmsRanalysis:::.filterNumericRangePlot(x, "emeta_value", min_val=min_val, max_val=max_val, title=title, 
                                            xlabel=xlabel, ylabel=ylabel)
  } else if (attr(x, "type") == "categorical") {
    .plotCategoricalEmetaFilt(x, cats = cats, na.rm = na.rm, title=title, 
                              xlabel=xlabel, ylabel=ylabel)
  } else {
    stop(sprintf("Unknown emetaFilt type: '%s'", attr(x, "type")))
  }
  
}

# internal only function
.plotCategoricalEmetaFilt <- function(filter_obj, cats = NA, na.rm = TRUE, title=NA, xlabel="", ylabel="Count") {
  data_vector <- dplyr::pull(filter_obj, "emeta_value")
  peaks_retained <- 0
  if (!identical(cats, NA)) peaks_retained <- peaks_retained + sum(data_vector %in% cats)
  else peaks_retained <- peaks_retained + sum(!is.na(data_vector))
  
  if (!na.rm) peaks_retained <- peaks_retained + sum(is.na(data_vector))
  counts <- c(`Peaks Retained`=peaks_retained, `Peaks Removed`=length(data_vector) - peaks_retained)
  
  plot_data <- data.frame(x=names(counts), count=as.vector(counts), stringsAsFactors = FALSE)
  plot_data$x <- factor(plot_data$x, levels=c("Peaks Retained", "Peaks Removed"))
  color_vec <- c(`Peaks Retained`="blue", `Peaks Removed`="gray")
  
  p <- plotly::plot_ly() %>%
    plotly::add_bars(x=~x, y=~count, color=~x, colors=color_vec, data=plot_data, showlegend=FALSE, hoverinfo="y") 
  
  # axis styling
  ax <- list(
    zeroline = FALSE, # don't plot axes at zero
    showline = TRUE,
    mirror = "ticks" # makes box go all the way around not just bottom and left
  )
  
  # annotations
  annotations <- lapply(seq_len(nrow(plot_data)), function(i) {
    list(x=plot_data$x[i], y=plot_data$count[i], text=as.character(plot_data$count[i]), yanchor="bottom", showarrow=FALSE)
  })
  
  p <- p %>%
    plotly::layout(barmode="overlay", annotations=annotations, 
                   xaxis=c(ax, list(title=xlabel), automargin=TRUE), yaxis=c(ax, list(title=ylabel)))
  
  if (!identical(title, NA)) {
    p <- p %>% plotly::layout(title=title)
  }
  
  p
  
}



#' Plot formula filter 
#'
#' @param x object of type 'formulaFilt', the output of \code{\link{formula_filter}} 
#' @param remove which items to remove ('NoFormula', or 'Formula'). This only affects plot coloring, no filtering is performed during plotting step.
#' @param title title for plot
#' @param xlabel x-axis label for plot
#' @param ylabel y-axis label for plot
#' @param ... other arguments
#'
#' @return plotly object
#' 
#' @rdname plot.formulaFilt
#' @name plot.formulaFilt
#' 
#' @export
#' 
#' @author Amanda White
#'
#' @examples
#' \dontrun{
#' filter_obj <- formula_filter(examplePeakData)
#' plot(filter_obj, remove='NoFormula')
#' }
plot.formulaFilt <- function(x, remove = NA, title=NA, xlabel="", ylabel="Count", ...) {
  
  counts <- table(x$Formula_Assigned)
  
  plot_data <- data.frame(x=names(counts), count=as.vector(counts), stringsAsFactors = FALSE)
  plot_data$x[plot_data$x == "FALSE"] <- "NoFormula"
  plot_data$x[plot_data$x == "TRUE"] <- "Formula"
  plot_data$x <- factor(plot_data$x, levels=c("Formula", "NoFormula"))
  color_vec <- c(Formula="gray", NoFormula="gray")
  if (!identical(remove, NA)) color_vec[names(color_vec) != remove] <- "blue"
  
  p <- plotly::plot_ly() %>%
    plotly::add_bars(x=~x, y=~count, color=~x, colors=color_vec, data=plot_data, showlegend=FALSE, hoverinfo="y") 
  
  # axis styling
  ax <- list(
    zeroline = FALSE, # don't plot axes at zero
    showline = TRUE,
    mirror = "ticks" # makes box go all the way around not just bottom and left
  )
  
  # annotations
  annotations <- lapply(seq_len(nrow(plot_data)), function(i) {
    list(x=plot_data$x[i], y=plot_data$count[i], text=as.character(plot_data$count[i]), yanchor="bottom", showarrow=FALSE)
  })
  
  p <- p %>%
    plotly::layout(barmode="overlay", annotations=annotations, 
                   xaxis=c(ax, list(title=xlabel), automargin=TRUE), yaxis=c(ax, list(title=ylabel)))
  
  if (!identical(title, NA)) {
    p <- p %>% plotly::layout(title=title)
  }
  
  p
  
}



#' Plot mass filter object
#'
#' @param x massFilt object, created by \code{\link{mass_filter}} function
#' @param min_mass (optional) minimum mass, for highlighting graph
#' @param max_mass (optional) maximum mass, for highlighting graph
#' @param title title
#' @param xlabel x axis label
#' @param ylabel y axis label
#' @param ... other arguments
#'
#' @return plotly object
#' 
#' @rdname plot.massFilt
#' @name plot.massFilt
#' 
#' @export
#'
#' @author Amanda White
#'
#' @examples
#' \dontrun{
#' filter_obj <- mass_filter(examplePeakData)
#' plot(filter_obj, min_mass = 200, max_mass = 800)
#' }
plot.massFilt <- function(x, min_mass=NA, max_mass=NA, title=NA, xlabel="Mass", ylabel="Count", ...) {
  ftmsRanalysis:::.filterNumericRangePlot(x, "Mass", min_mass, max_mass, title, xlabel, ylabel)
}


# Internal only function to plot a filter object (mass or emeta) based on a range of values
.filterNumericRangePlot <- function(filter_obj, field_name, min_val=NA, max_val=NA, title=NA, xlabel=field_name, ylabel="Count") {
  data_vector <- dplyr::pull(filter_obj, field_name)
  data_range <- range(data_vector, na.rm=TRUE)
  
  if (identical(min_val, NA) & !identical(max_val, NA)) min_val <- data_range[1]
  if (!identical(min_val, NA) & identical(max_val, NA)) max_val <- data_range[2]
  
  if (!identical(min_val, NA) & min_val < data_range[1]) min_val <- data_range[1]
  if (!identical(max_val, NA) & max_val > data_range[2]) max_val <- data_range[2]
  
  colored <- FALSE
  if (identical(min_val, NA) & identical(max_val, NA)) {
    breaks=pretty(data_range, n=25)
  } else { # come up with breaks the hard way!
    colored <- TRUE
    
    range_prop <- (max_val-min_val)/diff(data_range)
    breaks1 <- pretty(c(min_val, max_val), n=round(range_prop*25))
    binsize <- diff(breaks1)[1]
    colored_bins <- data.frame(x=breaks1[-1]-diff(breaks1)/2, category="Keep")
    
    n1 <- ceiling((min_val-data_range[1])/binsize)
    n2 <- ceiling((data_range[2]-max_val)/binsize)
    breaks <- c(seq(to=min_val-binsize, length=n1, by=binsize), breaks1, seq(from=max_val+binsize, length=n2, by=binsize))
    
    if (max(breaks) < data_range[2] | min(breaks) > data_range[1]) 
      stop("You did something wrong calculating those breaks!")
  }
  
  hist_data <- hist(data_vector, breaks = breaks, plot = FALSE)
  
  plot_data <- data.frame(x=hist_data$mids, count=hist_data$counts, binwidth=diff(breaks))
  
  p <- plotly::plot_ly() 
  if (!colored) {
    p <- p %>%
      plotly::add_bars(x=~x, y=~count, data=plot_data, width=~binwidth, showlegend=FALSE, hoverinfo="y", marker=list(color="gray"))  
  } else {
    color_vec <- c(Keep="blue", Remove="gray")
    plot_data <- plot_data %>% dplyr::left_join(colored_bins, by="x") %>%
      dplyr::mutate(category=ifelse(is.na(category), "Remove", "Keep"))
    p <- p %>% 
      plotly::add_bars(x=~x, y=~count, color=~category, data=plot_data, width=~binwidth, colors=color_vec, 
                       showlegend=FALSE, hoverinfo="y") 
    
    keep_bins <- dplyr::filter(plot_data, category=="Keep")
    keep_total <- sum(keep_bins$count)
    highest_count <- max(keep_bins$count)
    annotations <-
      list(x=(min_val+max_val)/2, y=highest_count, text=sprintf("%d Peaks Retained", keep_total), yanchor="bottom", 
           showarrow=FALSE)#, textangle=-90)
    p <- p %>% plotly::layout(annotations=annotations)
    
  }
  
  # axis styling
  ax <- list(
    zeroline = FALSE, # don't plot axes at zero
    showline = TRUE,
    mirror = "ticks" # makes box go all the way around not just bottom and left
  )
  
  p <- p %>%
    plotly::layout(xaxis=c(ax, list(title=xlabel), automargin=TRUE), yaxis=c(ax, list(title=ylabel)))
  
  if (!identical(title, NA)) {
    p <- p %>% plotly::layout(title=title)
  }
  
  p
}



#' Plot molecule filter data
#'
#' @param x moleculeFilt object, created by \code{\link{molecule_filter}} function
#' @param min_num minimum number of observations for filtering (only affects graph coloring/labeling, no data filtering is performed in this step)
#' @param title title for plot
#' @param xlabel x-axis label for plot
#' @param ylabel y-axis label for plot
#' @param ... other arguments
#'
#' @return plotly object
#' 
#' @rdname plot.moleculeFilt
#' @name plot.moleculeFilt
#' 
#' @export
#' 
#' @author Amanda White
#'
#' @examples
#' \dontrun{
#' filter_obj <- molecule_filter(examplePeakData)
#' plot(filter_obj, min_num=2)
#' }
plot.moleculeFilt <- function(x, min_num=NA, title=NA, xlabel="Minimum Number of Samples for which a Peak is Observed", ylabel="Number of Peaks", ...) {
  
  counts <- table(x$Num_Observations)
  
  # how many molecules have x or more observations:
  values <- rev(cumsum(rev(counts)))
  
  plot_data <- data.frame(x=as.numeric(names(values)), count=values)
  
  p <- plotly::plot_ly() %>%
    plotly::add_bars(x=~x, y=~count, data=plot_data, width=1, showlegend=FALSE, hoverinfo="y",
                     marker=list(color="gray"))  
  
  # axis styling
  ax <- list(
    zeroline = FALSE, # don't plot axes at zero
    showline = TRUE,
    mirror = "ticks" # makes box go all the way around not just bottom and left
  )
  
  p <- p %>%
    plotly::layout(barmode="overlay", xaxis=c(ax, list(title=xlabel), automargin=TRUE), yaxis=c(ax, list(title=ylabel)))
  
  if (!identical(min_num, NA)) {
    tmp_dat <- dplyr::filter(plot_data, x==min_num)
    p <- p %>% plotly::add_bars(x=~x, y=~count, width=1, data=tmp_dat, 
                                marker=list(color="blue"), hoverinfo="y")
    
    # annotations
    annotations <-
      list(x=tmp_dat$x[1], y=tmp_dat$count[1], text=as.character(tmp_dat$count[1]), yanchor="bottom", 
           showarrow=FALSE)#, textangle=-90)
    p <- p %>% plotly::layout(annotations=annotations)
    
  }
  
  if (!identical(title, NA)) {
    p <- p %>% plotly::layout(title=title)
  }
  
  p
  
}

