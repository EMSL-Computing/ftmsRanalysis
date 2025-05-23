#' Plot Method for CoreMSData Objects
#' 
#' Initial plot for CoreMSData objets showing number of unique masses per sample/file
#'
#' @param x CoreMSData object
#' @param title optional, if not specified "Unique Masses per Sample" will be used
#' @param xlabel optional, if not specified "Sample" will be used
#' @param ylabel optional, if not specified "Unique Masses" will be used
#' @param rotate_x_labs logical, optionally angles sample names on x-axis for readability if there are many samples and/or long sample names
#' @param ... included for compliance with generic method
#' 
#' @return `ggplot` object
#' @export
#'
plot.CoreMSData <- function(x, 
                            title = "Unique Masses per Sample", 
                            xlabel = "Sample", 
                            ylabel = "Unique Masses",
                            rotate_x_labs = FALSE,
                            ...) {
  
  # Check that x is a "CoreMSData" object (output from function as.CoreMSData())
  if(!inherits(x, "CoreMSData")) stop("x must be of class 'CoreMSData'")
  
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
  if(!(rotate_x_labs == TRUE | rotate_x_labs == FALSE)) stop("rotate_x_labs must be logical argument")
  
  sample_id <- attr(x, "cnames")$file_cname
  mass_id <- attr(x, "cnames")$calc_mass_cname 

  unique_masses_per_sample <- x$monoiso_data %>% 
    dplyr::group_by(!!rlang::sym(sample_id)) %>% 
    dplyr::distinct(!!rlang::sym(mass_id)) %>% 
    dplyr::tally() %>% dplyr::rename(Sample = sample_id, Monoisotopic = n)
  
  Isotopic <- x$iso_data %>% dplyr::group_by(!!rlang::sym(sample_id)) %>% 
    dplyr::distinct(!!rlang::sym(mass_id)) %>% 
    dplyr::tally() %>%
    dplyr::rename(Sample = sample_id, Isotopic = n)
    
  unique_masses_per_sample <- dplyr::full_join(unique_masses_per_sample, Isotopic) %>% 
    tidyr::pivot_longer(cols = c(!Sample), names_to = "Peak_type", values_to = "Count") %>% 
    dplyr::mutate(Peak_type = factor(Peak_type, levels = c("Monoisotopic", "Isotopic")))
  # set factor levels manually so bars will be in descending order by count
  
  plot <- unique_masses_per_sample %>%
    ggplot2::ggplot(ggplot2::aes(x = Sample,
                        y = Count,
                        fill = Peak_type)) +
    ggplot2::geom_bar(stat = "identity",
                      position = "dodge") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title,
                  x = xlabel,
                  y = ylabel) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Peak Type"))
  
  if (rotate_x_labs == TRUE) {
    plot <- plot +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  }

  plotly::ggplotly(plot)
  
}