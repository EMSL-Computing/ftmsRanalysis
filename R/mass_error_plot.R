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

  mass_id  <- attr(cmsObj, "cnames")$mass_cname
  error_id <- attr(cmsObj, "cnames")$error_cname
  file_id <- attr(cmsObj, "cnames")$file_cname

  num_files <- length(unique(dplyr::pull(cmsObj, file_id)))

  plot <- if (num_files == 1) {
    plotly::plot_ly(x = ~dplyr::pull(cmsObj, mass_id),
                    y = ~dplyr::pull(cmsObj, error_id),
                    type = "scatter",
                    mode = "markers",
                    marker = list(color = "chartreuse",
                                  size = 5,
                                  line = list(color = "steelblue4",
                                              width = 1))) %>%
      plotly::layout(title = title,
                     xaxis = list(title = xlabel),
                     yaxis = list(title = ylabel))
  } else if (num_files > 1) {
    p <- cmsObj %>%
      ggplot2::ggplot(aes(x = dplyr::pull(cmsObj, mass_id),
                          y = dplyr::pull(cmsObj, error_id))) +
      ggplot2::geom_hex() +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, x = xlabel, y = ylabel)

    if (!is.null(xrange)) {
      p <- p + xlim(xrange)
    }

    if (!is.null(yrange)) {
      p <- p + ylim(yrange)
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
