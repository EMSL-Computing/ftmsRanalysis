#' Plot mass filter object
#'
#' @param filter_obj massFilt object, created by \code{\link{mass_filter}} function
#' @param min_mass (optional) minimum mass, for highlighting graph
#' @param max_mass (optional) maximum mass, for highlighting graph
#' @param title title
#' @param xlabel x axis label
#' @param ylabel y axis label
#'
#' @return plotly object
#' @export
#'
#' @author Amanda White
#'
#' @examples
#' filter_obj <- mass_filter(peakIcrData)
#' plot(filter_obj, min_mass = 200, max_mass = 800)
plot.massFilt <- function(filter_obj, min_mass=NA, max_mass=NA, title=NA, xlabel="Mass", ylabel="Count") {
  
  data_range <- range(filter_obj$Mass, na.rm=TRUE)
  
  if (identical(min_mass, NA) & !identical(max_mass, NA)) max_mass <- data_range[2]
  if (!identical(min_mass, NA) & identical(max_mass, NA)) min_mass <- data_range[1]
  
  if (!identical(min_mass, NA) & min_mass < data_range[1]) min_mass <- data_range[1]
  if (!identical(max_mass, NA) & max_mass > data_range[2]) max_mass <- data_range[2]
  
  colored <- FALSE
  if (identical(min_mass, NA) & identical(max_mass, NA)) {
    breaks=pretty(data_range, n=25)
  } else { # come up with breaks the hard way!
    colored <- TRUE
    
    range_prop <- (max_mass-min_mass)/diff(data_range)
    breaks1 <- pretty(c(min_mass, max_mass), n=round(range_prop*25))
    binsize <- diff(breaks1)[1]
    colored_bins <- data.frame(x=breaks1[-1]-diff(breaks1)/2, category="Keep")

    n1 <- ceiling((min_mass-data_range[1])/binsize)
    n2 <- ceiling((data_range[2]-max_mass)/binsize)
    breaks <- c(seq(to=min_mass-binsize, length=n1, by=binsize), breaks1, seq(from=max_mass+binsize, length=n2, by=binsize))
    
    if (max(breaks) < data_range[2] | min(breaks) > data_range[1]) 
      stop("You did something wrong calculating those breaks!")
  }
  
  hist_data <- hist(filter_obj$Mass, breaks = breaks, plot = FALSE)
  
  plot_data <- data.frame(x=hist_data$mids, count=hist_data$counts, binwidth=diff(breaks))
  
  p <- plotly::plot_ly() 
  if (!colored) {
    p <- p %>%
      plotly::add_bars(x=~x, y=~count, data=plot_data, width=~binwidth, showlegend=FALSE, hoverinfo="y", marker=list(color="gray"))  
  } else {
    color_vec <- c(Keep="red", Remove="gray")
    plot_data <- plot_data %>% dplyr::left_join(colored_bins, by="x") %>%
      mutate(category=ifelse(is.na(category), "Remove", "Keep"))
    p <- p %>% 
      plotly::add_bars(x=~x, y=~count, color=~category, data=plot_data, width=~binwidth, colors=color_vec, 
               showlegend=FALSE, hoverinfo="y") 
    
    keep_bins <- dplyr::filter(plot_data, category=="Keep")
    keep_total <- sum(keep_bins$count)
    highest_count <- max(keep_bins$count)
    annotations <-
      list(x=(min_mass+max_mass)/2, y=highest_count, text=sprintf("%d Peaks Retained", keep_total), yanchor="bottom", 
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
