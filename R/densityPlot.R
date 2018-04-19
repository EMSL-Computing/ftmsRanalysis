#' Density plot of quantitative characteristic of peaks for a peakIcrData or compoundIcrData object
#' 
#' Constructs a density plot for a calculated variable in \code{e_meta} portion of icrData object
#' 
#' @param icrData icrData object of class peakIcrData or compoundIcrData
#' @param variable column name of column in \code{e_meta} which should be plotted. Must be one of the column names in icrData$e_meta that contains numeric values.
#' @param title plot title, default is NULL
#' @param fill_col fill colour to use in plot, must be in a format acceptable to ggplot2
#' @param xlabel x axis label, default is NULL, which will result in the same name as "variable"
#' @param ylabel y axis label, default is "Density"
#' 
#' @return a plotly object
#' 
#' @details If more than one sample is in \code{icrData}, the density of \code{variable} for all peaks seen in at least one sample will be displayed.
#' 
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Allison Thompson, Lisa Bramer
#' 
#' @export
densityPlot <- function(icrData, variable, title=NA, fill_col="blue", 
                         xlabel=NA, ylabel="Density") {

  # Test inputs #
  if (!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) {
    stop("icrData must be of type peakIcrData or compoundIcrData")
  }

  if (is.null(variable) | length(which(colnames(icrData$e_meta) == variable)) != 1) {
    stop("variable must be a single column name found in icrData$e_meta")
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

  p = ggplot2::ggplot(data = df, map) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), alpha = 0.5, bins = 25) +
    ggplot2::geom_density(fill = fill_col, alpha = 0.25) +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) 
  
  if(!is.na(title)){
    p = p +
    ggplot2::ggtitle(title)
  }
  
  ggplotly(p)
}