# This is needed because the plotly::plotly_build function which translates the
# data object returned by plot_ly to a data object used in the HTML reorders
# the axis category labels in alphabetical order. This fixes that issue. This
# function should be attached to object returned by plot_ly as 'preRenderHook',
# e.g. 
#    p <- plot_ly(..., type="heatmap")
#    p$preRenderHook <- plotly_build_wrapper
plotly_build_wrapper <- function(p) {
    p2 <- plotly::plotly_build(p)
    p2$x$layout$yaxis$categoryarray <- as.vector(p2$x$data[[1]]$y)
    p2$x$layout$xaxis$categoryarray <- as.vector(p2$x$data[[1]]$x)
    return(p2)
}


#' A wrapper of the plot_ly function to create a heatmap
#' 
#' Produces a heatmap using the plotly package. 
#' 
#' @param zMatrix numeric matrix of values for the heatmap
#' @param xLabels character vector of x-axis labels (corresponding to 
#'                columns of zMatrix)
#' @param yLabels character vector of x-axis labels (corresponding to 
#'                rows of zMatrix)
#' @param pal color palette, can be the name of an RColorBrewer palette 
#'            (see \code{\link{brewer.pal.info}} for valid names)
#'            or a palette function such as \code{\link{col_numeric}}
#' @param hoverText a matrix the same dimensions as \code{zMatrix} 
#'                  that provides text for mouseovers 
#' @param colorbarTitle a title to appear over the color scale bar
#' @param zLimits a vector of length two giving the limits of the z (color)
#'                axis, which is used to determine colors
#' @param margins list of margin parameters 
#'                (see \code{\link{layout}} function)
#' @export
#' @seealso \code{\link{plot_ly}}
plotlyHeatmap <- function(zMatrix, xLabels=colnames(zMatrix), 
        yLabels=rownames(zMatrix), pal="YlOrRd", hoverText=NA, 
        colorbarTitle="Color Scale", zLimits=range(zMatrix, na.rm=TRUE), 
        margins=list(l=200)) {

    #color palette
    if (is.character(pal)) {
        #color.pal <- scales::col_numeric(pal, zLimits, na.color="#FFFFFF")
        #plotly transforms values to (0,1) before calling color palette function
        color.pal <- scales::col_numeric(pal, c(0,1), na.color="#FFFFFF")
    } else if (is.function(pal)) {
        color.pal <- pal
    } else {
        stop("'pal' must be a name of an RColorBrewer palette or a palette function (e.g. scales::col_numeric)")
    }
    
    z.vals <- seq(from=zLimits[1], to=zLimits[2], length=100)
#    color.vec <- color.pal(z.vals)
#    colz <- setNames(data.frame(z.vals, color.vec), NULL)
    #colz <- #colorRamp(c("red", "green"))
#connectgaps = TRUE?
    p <- plotly::plot_ly(x=matrix(xLabels, nrow=1), 
        y=yLabels, z=as.matrix(zMatrix), 
        type="heatmap", colors=color.pal, zauto=FALSE, zmin=zLimits[1], zmax=zLimits[2],
        colorbar=list(title=colorbarTitle), text=as.matrix(hoverText), hoverinfo=ifelse(all(is.na(hoverText)), "all", "text")) %>%
        plotly::layout(margin=margins, yaxis=list(type="category")) 
    p$preRenderHook <- plotly_build_wrapper
    
    return(p)
}
