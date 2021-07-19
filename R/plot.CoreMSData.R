#' Plot Method for CoreMSData Objects
#' 
#' Initial plot for CoreMSData objets showing number of unique masses per sample/file
#'
#' @param cmsObj CoreMSData object
#' @param title optional, if not specified "Unique Masses per Sample" will be used
#' @param xlabel optional, if not specified "Sample" will be used
#' @param ylabel optional, if not specified "Unique Masses" will be used
#' @param long_sample_names logical, optionally angles sample names on x-axis if names are long
#'
#' @return a `ggplot` object
#' @export
#'
#' @examples
plot.CoreMSData <- function(cmsObj, 
                            title = "Unique Masses per Sample", 
                            xlabel = "Sample", 
                            ylabel = "Unique Masses",
                            long_sample_names = FALSE) {
  
  if(!inherits(cmsObj, "CoreMSData")) stop("cmsObj must be of class 'CoreMSData'")
  
  sample_id <- attr(cmsObj, "cnames")$file_cname
  mass_id <- attr(cmsObj, "cnames")$mass_cname 

  unique_masses_per_sample <- cmsObj %>%
    dplyr::group_by(dplyr::pull(cmsObj, sample_id)) %>%
    dplyr::distinct(dplyr::pull(cmsObj, mass_id)) %>%
    dplyr::tally()
  
  print(unique_masses_per_sample)

  plot <- unique_masses_per_sample %>%
    ggplot2::ggplot(aes(x = `dplyr::pull(cmsObj, sample_id)`,
                        y = n,
                        label = n)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title,
                  x = xlabel,
                  y = ylabel) +
    ggplot2::geom_text(nudge_y = 1.5)
  
  if (long_sample_names == TRUE) {
    plot <- plot +
      ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  }

  return(plot)
}