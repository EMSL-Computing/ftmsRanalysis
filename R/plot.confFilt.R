#' Plot confidence filter object
#'
#' @param confFiltObj object of class 'confFilt' created by \code{\link{conf_filter}} function
#' @param title (optional) plot title
#' @param xlabel x axis label, defaults to "Minimum Confidence Threshold"
#' @param ylabel y axis label, defaults to "Number of Peaks"
#' @param ... additional arguments
#'
#' @return `plotly` object
#' 
#' @author Natalie Winans
#' 
#' @export
#'
#' @examples
plot.confFilt <- function(confFiltObj, 
                          title = NULL, 
                          xlabel = "Minimum Confidence Threshold",
                          ylabel = "Number of Peaks",
                          ...) {
  
  # Check that confFiltObj is a confFilt object (output from function conf_filter)
  if(!inherits(confFiltObj, "confFilt")) stop("confFiltObj must be of class 'confFilt'")
  
  # Check that that parameter inputs are valid
  if(!is.null(title)) {
    if(!inherits(title, "character") | !(length(title) == 1)) stop("title must be single character string")
  }
  if(!is.null(xlabel)) {
    if(!inherits(xlabel, "character") | !(length(xlabel) == 1)) stop("xlabel must be single character string")
  }
  if(!is.null(ylabel)) {
    if(!inherits(ylabel, "character") | !(length(ylabel) == 1)) stop("ylabel must be single character string")
  }
  
  conf_score <- attr(confFiltObj, "conf_cname")
  
  # count rows with NA values for confidence score
  num_na <- sum(is.na(dplyr::pull(confFiltObj, conf_score)))
  if (!(num_na == 0)){
    print(paste0(num_na, " rows with missing values were omitted from the dataset"))
  }
  
  min_conf <- seq(from = 0, to = 1, by = 0.1)
  Kept <- c()
  for (i in 1:length(min_conf)) {
    Kept[i] <- sum(dplyr::pull(confFiltObj, conf_score) > min_conf[i], na.rm = TRUE)
  }
  
  plot_data <- data.frame(min_conf, Kept) %>% 
    dplyr::mutate(Removed = Kept[1] - Kept) %>% 
    tidyr::pivot_longer(cols = c(Kept, Removed), names_to = "Num_Peaks", values_to = "Count")
  
  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(min_conf), 
                                 y = Count, 
                                 fill = factor(Num_Peaks, levels = c("Removed", "Kept")),
                                 label = Count,
                                 text = paste0("Minimum Confidence: ", min_conf, 
                                 "\nPeaks ", Num_Peaks, ": ", Count))) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title,
                  x = xlabel,
                  y = ylabel) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL,
                                                 reverse = TRUE)) +
    ggplot2::geom_text(data = subset(plot_data, Num_Peaks == "Kept"),
                       nudge_y = 3) +
    ggplot2::scale_fill_manual(values = c("skyblue1", "steelblue4" ))
  
  plotly::ggplotly(p, tooltip = "text")
  
}