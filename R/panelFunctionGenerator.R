#' Convenience function to wrap ftmsRanalysis plotting functions for Trelliscope
#'
#' @param plot_fn_name plot function name, e.g. "vanKrevelenPlot" or "kendrickPlot"
#' @param ... other parameters to pass to the plotting function other than the data object
#' 
#' @details Sometimes the additional parameters to pass to the plotting function will depend
#' on the key associated with the data in the ddo (e.g. if a column name is prepended with 
#' the group name). In that case, see example below for how
#' to construct an \code{expr} statement to obtain the necessary information.
#' 
#' @author Amanda White
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' library(ftmsRanalysis)
#' library(trelliscope)
#' 
#' vdbDir <- vdbConn(file.path(tempdir(), "trell_test"), autoYes = TRUE) #temporary directory
#' 
#' data('exampleProcessedPeakData')
#' groupDdo <- divideByGroup(exampleProcessedPeakData)
#' groupSummaryDdo <- summarizeGroups(groupDdo, summary_functions = c("prop_present", "n_present"))
#' 
#' # See rlang::expr here:
#' panelFnG1 <- panelFunctionGenerator("vanKrevelenPlot", colorCName=rlang::expr(paste0(getSplitVar(v, "Group"), "_prop_present")), 
#'                                     legendTitle="Proportion<br>Present")
#' 
#' makeDisplay(groupSummaryDdo, 
#'             panelFn=panelFnG1,
#'             name = "Trelliscope test G_1 with VK plot per group",
#'             group = "Group")
#' view()
#' }
panelFunctionGenerator <- function(plot_fn_name, ...) {
  if (missing(plot_fn_name)) stop("plot_fn_name must be provided")
  if (!is.character(plot_fn_name)) stop("plot_fn_name must be character")
  if (length(plot_fn_name)>1) stop("plot_fn_name must be a single value")
  
#  browser()
  parms <- list(...)
#  parms2 <- pryr::dots(...)
  
  plot_fn <- getFromNamespace(plot_fn_name, "ftmsRanalysis")
  if (is.null(plot_fn)) stop(sprintf("Unknown function '%s', cannot load this function from ftmsRanalysis", plot_fn_name))
  
  fn <- function(v) {
    tmp <- list(a=v)
    names(tmp)<- NULL
    do.call(plot_fn, c(tmp, parms))
  }
  return(fn)
}