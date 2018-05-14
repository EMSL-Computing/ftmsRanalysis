#' Density plot of quantitative characteristic of peaks for a peakIcrData or compoundIcrData object
#' 
#' Constructs a density plot for a calculated variable in \code{e_meta} portion of icrData object
#' 
#' @param icrData icrData object of class peakIcrData or compoundIcrData
#' @param variable column name of column in \code{e_meta} which should be plotted. Must be one of the column names in icrData$e_meta that contains numeric values.
#' @param title plot title, default is NULL
#' @param yaxis what should the y-axis represent, "density" or "count"? If "count" no smoothed density curve will be added.
#' @param smooth_curve TRUE/FALSE should a smoothed curve be added to the plot?
#' @param fill_col fill colour to use in plot under smoothed curve, must be a string with color code in hexadecimal format (see \code{\link{rgb}}) or an element of \code{\link{colors}()}
#' @param xlabel x axis label, default is NULL, which will result in the same name as "variable"
#' @param ylabel y axis label, default is "Density"
#' 
#' @return a plotly object
#' 
#' @details If more than one sample is in \code{icrData}, the density of \code{variable} for all peaks seen in at least 
#' one sample will be displayed. The histogram bar data as a data frame is attached to the output as the attribute \code{hist_data}.
#' This may be useful for adding additional data to the plot, e.g. for linking selections in multiple plots.
#' 
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Allison Thompson, Lisa Bramer, Amanda White
#' 
#' @export
densityPlot <- function(icrData, variable, title=NA, yaxis="density", smooth_curve=TRUE, 
                        fill_col="blue", xlabel=NA, ylabel=paste0(toupper(substring(yaxis, 1,1)), substring(yaxis,2), sep="")) {

  # Test inputs #
  if (!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) {
    stop("icrData must be of type peakIcrData or compoundIcrData")
  }

  if (is.null(variable) | length(which(colnames(icrData$e_meta) == variable)) != 1) {
    stop("variable must be a single column name found in icrData$e_meta")
  }
  
  if (is.null(yaxis) | !(yaxis %in% c("count", "density"))) {
    stop("yaxis must be either 'density' or 'count'")
  }
  
  if (!is.character(fill_col)) {
    stop("fill_col must be a string")
  }
  if (!startsWith(fill_col, "#")) {
    if (! (fill_col %in% colors())) {
      stop("fill_col must be an element of colors() or a hexadecimal color code")
    }
    # convert color name to hex string with alpha 0.25
    fill_col <- do.call(rgb, c(as.list(col2rgb(fill_col)[,1]/255), alpha=0.25))
  }
  
  # End Tests #
  
  if(is.na(xlabel)){
    xlabel <- variable
  }
  
  # merge e_data and e_meta #  
  df <- merge(icrData$e_data, icrData$e_meta, by=getMassColName(icrData))
  
  # if data is not on a log-scale #
  if(getDataScale(icrData) %in% c('pres', 'abundance')){
    # subset down to where sample value != 0 #
    if(length(as.character(icrData$f_data[,getFDataColName(icrData)])) == 1){
      df <- df[which(df[,as.character(icrData$f_data[,attr(icrData, "cnames")$fdata_cname])] > 0),]
    }else if(length(as.character(icrData$f_data[,getFDataColName(icrData)])) > 1){
      df <- df[which(rowSums(df[,as.character(icrData$f_data[,attr(icrData, "cnames")$fdata_cname])] > 0, na.rm=TRUE) > 0),]
    }else{
      stop("No samples in object")
    }
  }else{
    # subset down to where sample value != NA #
    if(length(as.character(icrData$f_data[,getFDataColName(icrData)])) == 1){
      df <- df[which(!is.na(df[,as.character(icrData$f_data[,attr(icrData, "cnames")$fdata_cname])])),]
    }else if(length(as.character(icrData$f_data[,getFDataColName(icrData)])) > 1){
      df <- df[which(rowSums(!is.na(df[,as.character(icrData$f_data[,attr(icrData, "cnames")$fdata_cname])])) > 0),]
    }else{
      stop("No samples in object")
    }
  }
  # subset down to those able to be plotted #
  if(any(is.na(df[,variable]))){
    df <- df[which(!is.na(df[,variable])),]
  }
  

  map <- ggplot2::aes_string(x = variable)

  p2 = ggplot2::ggplot(data = df, map) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), alpha = 0.5, bins = 25) +
    ggplot2::geom_density(fill = fill_col, alpha = 0.25) +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel)
  # 
  # if(!is.na(title)){
  #   p = p +
  #   ggplot2::ggtitle(title)
  # }

  # ggplotly(p)

  hist_data <- hist(df[, variable], breaks = 25, plot = FALSE)
  keys <- lapply(1:length(hist_data$mids), function(i) as.numeric(hist_data$breaks[i:(i+1)]))
  hist_data <- tibble:::tibble(x=hist_data$mids, count=hist_data$counts, density=hist_data$density, 
                               barwidth=diff(hist_data$breaks), key=keys)

  if (yaxis == "density") {
    p <- plot_ly() %>%
      add_bars(x=~x, y=~density, width=~barwidth, key=~key, data=hist_data, marker=list(color="gray")) 
  } else if (yaxis == "count") {
    p <- plot_ly() %>%
      add_bars(x=~x, y=~count, width=~barwidth, key=~key, data=hist_data, marker=list(color="gray")) 
  }
  
  if (smooth_curve) {
    n <- 512
    dens_curve <- stats::density(x=df[, variable], weights=rep(1/nrow(df), length=nrow(df)), 
                                 from = min(df[, variable], na.rm=TRUE), to = max(df[, variable], na.rm=TRUE), 
                                 bw = "nrd0", adjust = 1, kernel = "gaussian", n = n)
    dens_curve <- as.data.frame(dens_curve[c("x", "y")])
    if (yaxis == "count") { # scale density up to match counts
      dens_curve$y <- dens_curve$y*nrow(df)*hist_data$barwidth[1]
    }
    
    p <- p %>%
      add_lines(x=~x, y=~y, data=dens_curve, fillcolor=fill_col, fill = 'tonexty', opacity=0.5, 
                alpha=0.5, showlegend=FALSE, line=list(color=fill_col))
  }
  
  p <- p %>% layout(barmode="overlay", xaxis=list(title=xlabel), yaxis=list(title=ylabel))
  
  if(!is.na(title)){
    p <- p %>%
      layout(title=title)
  }
  
  attr(p, "hist_data") <- hist_data
  
  p
}