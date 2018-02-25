#' Plot Van Krevelen boundary set
#' 
#' Generate the boundaries making up any boundary set available in \code{fticRanalysis}
#' 
#' @param boundary_df a data.frame giving Hydrogen:Carbon and Oxygen:Carbon upper and lower limits, usually a result of \code{\link{getVanKrevelenCategories}} or must match this format.
#' 
#' @author Lisa Bramer
#' 
#' @export

plot_vkBoundarySet <- function(boundary_df){
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(plotly))
  suppressPackageStartupMessages(require(scales))
  
  # check that boundary_df is a data.frame #
  if(!inherits(boundary_df, "data.frame")) stop("boundary_df is not of class data.frame")
  
  # check that column names match the output of getVanKrevelenCategories #
  if(sum(c("HC.low", "HC.high", "OC.low", "OC.high") %in% names(boundary_df)) != 4) stop("boundary_df does not have appropriate column names to utilize this function")
  
  vankrev_categories <- data.frame(y0 = c(boundary_df$HC.low, boundary_df$HC.high, boundary_df$HC.low, boundary_df$HC.low), 
                                   y1 = c(boundary_df$HC.low, boundary_df$HC.high, boundary_df$HC.high, boundary_df$HC.high), 
                                   x0 = c(boundary_df$OC.low, boundary_df$OC.low, boundary_df$OC.low, boundary_df$OC.high), 
                                   x1 = c(boundary_df$OC.high, boundary_df$OC.high, boundary_df$OC.low, boundary_df$OC.high), 
                                   category = rep(rownames(boundary_df), 4))
  
  col.pal<- scales::col_factor("Set1", levels=rownames(boundary_df))   
  col.vec <- col.pal(rownames(boundary_df))
  names(col.vec) <- rownames(boundary_df)
  
  plot_ly() %>%
    add_segments(x=~x0, y=~y0, xend=~x1, yend=~y1, data=vankrev_categories, color=~category, colors=col.vec, showlegend = TRUE) %>% 
    layout(xaxis=list(title="O:C", range=c(0,1.55)), yaxis=list(title="H:C", range=c(0,2.6)))
}