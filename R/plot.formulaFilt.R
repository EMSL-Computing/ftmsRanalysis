#' Plot formula filter 
#'
#' @param filter_obj object of type 'formulaFilt', the output of \code{\link{formula_filter}} 
#' @param remove which items to remove ('NoFormula', or 'Formula'). This only affects plot coloring, no filtering is performed during plotting step.
#' @param title title for plot
#' @param xlabel x-axis label for plot
#' @param ylabel y-axis label for plot
#'
#' @return plotly object
#' @export
#' 
#' @author Amanda White
#'
#' @examples
#' filter_obj <- formula_filter(peakIcrData)
#' plot(filter_obj, remove='NoFormula')
plot.formulaFilt <- function(filter_obj, remove = NA, title=NA, xlabel="", ylabel="Count") {
  
  counts <- table(filter_obj$Formula_Assigned)
  
  plot_data <- data.frame(x=names(counts), count=as.vector(counts), stringsAsFactors = FALSE)
  plot_data$x[plot_data$x == "FALSE"] <- "NoFormula"
  plot_data$x[plot_data$x == "TRUE"] <- "Formula"
  plot_data$x <- factor(plot_data$x, levels=c("Formula", "NoFormula"))
  color_vec <- c(Formula="gray", NoFormula="gray")
  if (!identical(remove, NA)) color_vec[names(color_vec) != remove] <- "red"
  
  p <- plot_ly() %>%
    add_bars(x=~x, y=~count, color=~x, colors=color_vec, data=plot_data, showlegend=FALSE, hoverinfo="y") 
  
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
    layout(barmode="overlay", annotations=annotations, 
           xaxis=c(ax, list(title=xlabel), automargin=TRUE), yaxis=c(ax, list(title=ylabel)))

  if (!identical(title, NA)) {
    p <- p %>% layout(title=title)
  }
  
  p
  
}
