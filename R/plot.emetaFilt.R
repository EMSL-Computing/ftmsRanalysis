#' Default plot method for emetaFilt
#'
#' @param filter_obj object of class \code{\link{emetaFilt}}
#' @param min_val a numeric value specifying the minimum value (inclusive) that a peak should have for the specified 'e_meta' column
#' @param max_val a numeric value specifying the maximum value (inclusive) that a peak should have for the specified 'e_meta' column
#' @param cats a vector of character values specifying the level(s) of the specified 'e_meta' column which should be retained
#' @param na.rm logical value specifying if peaks with NA values for the specified 'e_meta' column should be removed. Default value is TRUE
#' @param title plot title
#' @param xlabel x-axis label, default is the column of emeta used to construct \code{filter_obj}
#' @param ylabel y-axis label
#' @return \code{plotly} plot object
#' @export
#'
#' @examples
#' # Emeta filter based on the "OtoC_ratio" column (numeric variable)
#' filter_object1 = emeta_filter(peakIcrProcessed, cname = "OtoC_ratio")
#' 
#' # Plots the distribution of OtoC_ratio
#' plot(filter_object1)
#' 
#' # Demonstrates retaining values between 0.5 and 1
#' plot(filter_object1, min_val = 0.5, max_val = 1)
#' 
#' # Emeta filter based on the "MolForm" column (categorical variable)
#' filter_object2 = emeta_filter(peakIcrProcessed, cname = "MolForm")
#' 
#' # Only non-NAs retained
#' plot(filter_object2)
#' 
#' # Peaks matching "C10" retained
#' plot(filter_object2, cats=grep("C10", filter_object2$emeta_value, value = TRUE))
plot.emetaFilt <- function(filter_obj, min_val = NA, max_val = NA, cats = NA, na.rm = TRUE, 
                           title=NA, xlabel=attr(filter_obj, "cname"), ylabel="Count") {
  if (attr(filter_obj, "type") == "quantitative") {
    fticRanalysis:::.filterNumericRangePlot(filter_obj, "emeta_value", min_val=min_val, max_val=max_val, title=title, 
                                            xlabel=xlabel, ylabel=ylabel)
  } else if (attr(filter_obj, "type") == "categorical") {
    .plotCategoricalEmetaFilt(filter_obj, cats = cats, na.rm = na.rm, title=title, 
                              xlabel=xlabel, ylabel=ylabel)
  } else {
    stop(sprintf("Unknown emetaFilt type: '%s'", attr(filter_obj, "type")))
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
