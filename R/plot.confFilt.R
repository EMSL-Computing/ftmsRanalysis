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
plot.confFilt <- function(confFiltObj, 
                          title = "Peaks Retained Under Confidence Filter", 
                          xlabel = "Minimum Confidence Threshold",
                          ylabel = "Monoisotopic Peaks Retained",
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

  min_conf <- seq(from = 0, to = 1, by = 0.1)
  Kept_mono <- c()
  Kept_iso <- c()
  for (i in 1:length(min_conf)) {
    Kept_mono[i] <- sum(dplyr::pull(confFiltObj$monoiso_data, conf_score) > min_conf[i], na.rm = TRUE)
    Kept_iso[i] <- sum(dplyr::pull(confFiltObj$iso_data, conf_score) > min_conf[i], na.rm = TRUE)
  }
  
  mono_plot_data <- data.frame(min_conf, Kept_mono) %>% 
    dplyr::mutate(Removed = Kept_mono[1] - Kept_mono) %>% 
    dplyr::rename(Kept = Kept_mono) %>%
    tidyr::pivot_longer(cols = c(Kept, Removed), names_to = "Num_Peaks", values_to = "Count") %>% 
    dplyr::mutate(Peak_type = "Monoisotopic")
  
  iso_plot_data <- data.frame(min_conf, Kept_iso) %>% 
    dplyr::mutate(Removed = Kept_iso[1] - Kept_iso) %>% 
    dplyr::rename(Kept = Kept_iso) %>%
    tidyr::pivot_longer(cols = c(Kept, Removed), names_to = "Num_Peaks", values_to = "Count") %>% 
    dplyr::mutate(Peak_type = "Isotopic")
  
  plot_data <- rbind(mono_plot_data, iso_plot_data) %>% 
    dplyr::mutate(Peak_type = factor(Peak_type, levels = c('Monoisotopic', 'Isotopic')))
  
  # for now just plot monoisotopic peaks
  p <- mono_plot_data %>% #plot_data %>%
    dplyr::filter(Num_Peaks == "Kept") %>% 
    ggplot2::ggplot(ggplot2::aes(x = factor(min_conf),
                                 y = Count,
                                 #fill = Peak_type,#factor(Num_Peaks, levels = c("Removed", "Kept")),
                                 label = Count,
                                 text = paste0("Minimum Confidence: ", min_conf,
                                 "\nPeaks Retained: ", Count))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", fill = "#00AFBB") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title,
                  x = xlabel,
                  y = ylabel) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) 
    # ggplot2::guides(fill = ggplot2::guide_legend(title = NULL,
    #                                              reverse = TRUE)) +
    # ggplot2::scale_fill_discrete(name = NULL) +
    # ggplot2::geom_text(ggplot2::aes(label = Count),
    #                    position = position_dodge(0.9),
    #                    vjust = -1)
    # ggplot2::scale_fill_manual(values = c("skyblue1", "steelblue4" ))

  # plotly::ggplotly(p, tooltip = "text")
  p
  
}