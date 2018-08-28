#' Plot molecule filter data
#'
#' @param filter_obj moleculeFilt object, created by \code{\link{molecule_filter}} function
#' @param min_num minimum number of observations for filtering (only affects graph coloring/labeling, no data filtering is performed in this step)
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
#' filter_obj <- molecule_filter(peakIcrData)
#' plot(filter_obj, min_num=2)
plot.moleculeFilt <- function(filter_obj, min_num=NA, title=NA, xlabel="Observed", ylabel="Count") {
  
  counts <- table(filter_obj$Num_Observations)
  
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
                          marker=list(color="red"), hoverinfo="y")
    
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
