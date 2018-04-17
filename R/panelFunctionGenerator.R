#' Convenience function to wrap fticRanalysis plotting functions for Trelliscope
#'
#' @param plot_fn_name plot function name, e.g. "vanKrevelenPlot" or "kendrickPlot"
#' @param ... other parameters to pass to the plotting function other than the data object
#' @export
panelFunctionGenerator <- function(plot_fn_name, ...) {
  if (missing(plot_fn_name)) stop("plot_fn_name must be provided")
  if (!is.character(plot_fn_name)) stop("plot_fn_name must be character")
  if (length(plot_fn_name)>1) stop("plot_fn_name must be a single value")
  
#  browser()
  parms <- list(...)
#  parms2 <- pryr::dots(...)
  
  plot_fn <- getFromNamespace(plot_fn_name, "fticRanalysis")
  if (is.null(plot_fn)) stop(sprintf("Unknown function '%s', cannot load this function from fticRanalysis", plot_fn_name))
  
  fn <- function(v) {
    tmp <- list(a=v)
    names(tmp)<- NULL
    do.call(plot_fn, c(tmp, parms))
  }
  return(fn)
}