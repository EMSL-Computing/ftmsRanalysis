#' Plot principal coordinates of icrData samples
#'
#' Constructs a scatter plot of any combination of principal coordinates calculated by the \code{\link{getPrincipalCoordinates}} function. If an associated \code{icrData} object is provided with groups defined, the resulting plot will be colored by group membership.
#' 
#' @param pcoaMat numeric matrix, the output of \code{\link{getPrincipalCoordinates}}
#' @param x numeric value, which column of \code{pcoaMat} should serve as the x-axis
#' @param y numeric value, which column of \code{pcoaMat} should serve as the y-axis 
#' @param icrData optional, if provided points will be colored according to the groups defined in \code{icrData}
#' @param title optional plot title
#' @param xlabel x-axis label
#' @param ylabel y-axis label
#' @param includeR2OnAxes TRUE/FALSE, should the x- and y-axis labels be appended with R^2 values?
#'
#' @return a \code{\link{plotly}} plot object
#' @export
#'
#' @examples
#' pcoaMat <- getPrincipalCoordinates(peakIcrProcessed)
#' plotPrincipalCoordinates(pcoaMat, x=2, y=3, icrData=peakIcrProcessed)
plotPrincipalCoordinates <- function(pcoaMat, x=1, y=2, icrData=NA, title=NA, 
                                     xlabel=sprintf("Principal Coordinate %d", x), 
                                     ylabel=sprintf("Principal Coordinate %d", y),
                                     includeR2OnAxes=TRUE) {
  
  # test inputs
  if (!is.numeric(pcoaMat) | !inherits(pcoaMat, "matrix")) stop("pcoaMat must be a numeric matrix")
  if (!is.numeric(x) | x > ncol(pcoaMat)) stop("x must be a number and must be less than the number of columns in pcoaMat")
  if (!is.numeric(y) | y > ncol(pcoaMat)) stop("y must be a number and must be less than the number of columns in pcoaMat")
  if (!identical(icrData, NA) & !inherits(icrData, "icrData")) stop("if icrData parameter is provided it must be an object of type icrData")
  
  r2 <- attr(pcoaMat, "R^2")
  cnames <- colnames(pcoaMat)
  
  pcoaMat <- data.frame(pcoaMat)
  pcoaMat[, "Sample"] <- rownames(pcoaMat)
  
  parms <- list()
  if (!identical(icrData, NA)) {
    if (!is.null(getGroupDF(icrData))) {
      by=c(Sample=getFDataColName(icrData))
      suppressWarnings(pcoaMat <- dplyr::left_join(pcoaMat, getGroupDF(icrData), by=by))
      parms <- list(color=~Group)
    }
  }
  parms <- c(list(data=pcoaMat, x=as.formula(sprintf("~%s", cnames[x])), 
                y=as.formula(sprintf("~%s", cnames[y])), 
                type="scatter", 
                mode="markers", hoverinfo="text", 
                text=~Sample), 
             parms)
  
  p <- do.call(plotly::plot_ly, parms)
 
  if (includeR2OnAxes) {
    xlabel <- sprintf("%s (R<sup>2</sup>=%.3f)", xlabel, r2[x])
    ylabel <- sprintf("%s (R<sup>2</sup>=%.3f)", ylabel, r2[y])
  }
  ax <- list(
    zeroline = FALSE, # don't plot axes at zero
    showline = TRUE,
    mirror = "ticks" # makes box go all the way around not just bottom and left
  )
    
  p <- p %>% plotly::layout(xaxis=c(ax, list(title=xlabel)), yaxis=c(ax, list(title=ylabel)))
  
  if (!is.na(title)) {
    p <- p %>% plotly::layout(title=title)
  }
  
  return(p)
}
