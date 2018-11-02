#'
#' @export
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
  if (!na.rm) peaks_retained <- peaks_retained + sum(is.na(data_vector))
  counts <- c(`Peaks Retained`=peaks_retained, `Peaks Removed`=length(data_vector) - peaks_retained)
  
  plot_data <- data.frame(x=names(counts), count=as.vector(counts), stringsAsFactors = FALSE)
  plot_data$x <- factor(plot_data$x, levels=c("Peaks Retained", "Peaks Removed"))
  color_vec <- c(`Peaks Retained`="red", `Peaks Removed`="gray")
  
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
