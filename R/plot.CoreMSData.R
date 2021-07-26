#' Plot Method for CoreMSData Objects
#' 
#' Initial plot for CoreMSData objets showing number of unique masses per sample/file
#'
#' @param cmsObj CoreMSData object
#' @param title optional, if not specified "Unique Masses per Sample" will be used
#' @param xlabel optional, if not specified "Sample" will be used
#' @param ylabel optional, if not specified "Unique Masses" will be used
#' @param diag_x_labs logical, optionally angles sample names on x-axis for readability if there are many samples and/or long sample names
#'
#' @return `ggplot` object
#' @export
#'
#' @examples
plot.CoreMSData <- function(cmsObj, 
                            title = "Unique Masses per Sample", 
                            xlabel = "Sample", 
                            ylabel = "Unique Masses",
                            diag_x_labs = FALSE) {
  
  # Check that cmsObj is a "CoreMSData" object (output from function as.CoreMSData())
  if(!inherits(cmsObj, "CoreMSData")) stop("cmsObj must be of class 'CoreMSData'")
  
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
  if(!(diag_x_labs == TRUE | diag_x_labs == FALSE)) stop("diag_x_labs must be logical argument")
  
  sample_id <- attr(cmsObj, "cnames")$file_cname
  mass_id <- attr(cmsObj, "cnames")$mass_cname 

  unique_masses_per_sample <- cmsObj %>%
    dplyr::group_by(dplyr::pull(cmsObj, sample_id)) %>%
    dplyr::distinct(dplyr::pull(cmsObj, mass_id)) %>%
    dplyr::tally()

  plot <- unique_masses_per_sample %>%
    ggplot2::ggplot(ggplot2::aes(x = `dplyr::pull(cmsObj, sample_id)`,
                        y = n,
                        label = n)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "steelblue4") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title,
                  x = xlabel,
                  y = ylabel) +
    ggplot2::guides(fill = FALSE) +
    ggplot2::geom_text(nudge_y = 1.5)
  
  if (diag_x_labs == TRUE) {
    plot <- plot +
      ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  }

  plot
}