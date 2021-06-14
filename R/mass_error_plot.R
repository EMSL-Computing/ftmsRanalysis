#' Generates mass-error plot for CoreMS output data
#'
#' @param cmsObj a data.frame of MS data with columns containing calculated m/z, mass error (ppm), and file names, the output of function \code{read_CoreMS_data}
#' @param title a character string specifying the plot title
#' @param xlabel a character string specifying the x-axis label
#' @param ylabel a character string specifying the y-axis label
#' @param xrange a numerical vector of length 2 specifying the x-axis range
#' @param yrange a numerical vector of length 2 specifying the y-axis range
#' @param color_pal
#' @param log_color_scale logical, if TRUE, color scale will be log-transformed. Defaults to FALSE.
#'
#' @details
#'
#' @return plotly object
#'
#' @export

mass_error_plot <- function(cmsObj,
                            title = NULL,
                            xlabel = "Mass to charge ratio (m/z)",
                            ylabel = "Mass error (ppm)",
                            xrange = NULL,
                            yrange = NULL,
                            color_pal = NULL,
                            log_color_scale = FALSE) {
  
  
  # Check that cmsObj is a "CoreMSData" object (output from function as.CoreMSData())
  if(!inherits(cmsObj, "CoreMSData")) stop("cmsObj must be of the class 'CoreMSData'")
  
  # Check that that optional parameter inputs are valid
  if(!is.null(title)) {
    if(!inherits(title, "character") | !(length(title) == 1)) stop("title must be single character string")
  }
  if(!is.null(xlabel)) {
    if(!inherits(xlabel, "character") | !(length(xlabel) == 1)) stop("xlabel must be single character string")
  }
  if(!is.null(ylabel)) {
    if(!inherits(ylabel, "character") | !(length(ylabel) == 1)) stop("ylabel must be single character string")
  }
  if(!is.null(xrange)) {
    if(!inherits(xrange, "numeric") | !(length(xrange) == 2)) stop("xrange must be numeric vector with length 2")
  }
  if(!is.null(yrange)) {
    if(!inherits(yrange, "numeric") | !(length(yrange) == 2)) stop("yrange must be numeric vector with length 2")
  }
  
  
  mass_id  <- attr(cmsObj, "cnames")$mass_cname
  error_id <- attr(cmsObj, "cnames")$error_cname
  file_id <- attr(cmsObj, "cnames")$file_cname
  formula_id <- attr(cmsObj, "cnames")$mf_cname
  
  num_files <- length(unique(dplyr::pull(cmsObj, file_id)))
  
  plot <- if (num_files == 1) {
    
    p <- cmsObj %>%
      ggplot2::ggplot(ggplot2::aes(x = dplyr::pull(cmsObj, mass_id),
                                   y = dplyr::pull(cmsObj, error_id),
                                   text = paste("Mass-to-charge (m/z): ",
                                                dplyr::pull(cmsObj, mass_id),
                                                "\nMass Error (ppm): ",
                                                dplyr::pull(cmsObj, error_id),
                                                "\nFormula: ",
                                                dplyr::pull(cmsObj, formula_id)))) +
      ggplot2::geom_point(shape = 21,
                          color = "navyblue",
                          fill = "chartreuse",
                          size = 2,
                          stroke = 0.5) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, x = xlabel, y = ylabel)
    
    if (!is.null(xrange)) {
      p <- p + ggplot2::xlim(xrange)
    }
    
    if (!is.null(yrange)) {
      p <- p + ggplot2::ylim(yrange)
    }
    
    plotly::ggplotly(p, tooltip = "text")
    
  } else if (num_files > 1) {
    p <- cmsObj %>%
      ggplot2::ggplot(ggplot2::aes(x = dplyr::pull(cmsObj, mass_id),
                                   y = dplyr::pull(cmsObj, error_id))) +
      ggplot2::geom_hex() +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, x = xlabel, y = ylabel)
    
    if (!is.null(xrange)) {
      p <- p + ggplot2::xlim(xrange)
    }
    
    if (!is.null(yrange)) {
      p <- p + ggplot2::ylim(yrange)
    }
    
    if (log_color_scale) {
      p <- p + viridis::scale_fill_viridis(trans = "log")
    } else {
      p <- p + viridis::scale_fill_viridis()
    }
    
    plotly::ggplotly(p)
  }
  plot
}
