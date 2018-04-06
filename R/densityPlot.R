#' Density plot of quantitative characteristic of peaks for a peakIcrData or compoundIcrData object
#' 
#' Constructs a density plot for a calculated variable in an
#' object of type icrData.
#' 
#' @param dataObj icrData object
#' @param variable which compound calcs calculation to plot. Must be one of the column names in dataObj$e_meta that contains numeric values.
#' @param title plot title, default is NULL
#' @param fill_col fill colour to use in plot
#' @param xlabel x axis label, default is NULL, which will result in the same name as "variable"
#' @param ylabel y axis label, default is "Density"
#' 
#' @return a plotly object
#' 
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Allison Thompson, Lisa Bramer
#' 
#' @export
densityPlot <- function(dataObj, variable, title=NA, fill_col="blue", 
                         xlabel=NA, ylabel="Density") {

  # Test inputs #
  if (!inherits(dataObj, "peakIcrData") & !inherits(dataObj, "compoundIcrData")) {
    stop("dataObj must be of type peakIcrData or compoundIcrData")
  }

  if (is.null(variable) | length(which(colnames(dataObj$e_meta) == variable)) != 1) {
    stop("variable must be a single column name found in dataObj$e_meta")
  }
  
  # End Tests #
  
  if(is.na(xlabel)){
    xlabel <- variable
  }
  
  # merge e_data and e_meta #  
  df <- merge(dataObj$e_data, dataObj$e_meta, by=getMassColName(dataObj))
  
  # subset down to where sample value != 0 #
  if(length(as.character(dataObj$f_data[,getFDataColName(dataObj)])) == 1){
    df <- df[which(df[,as.character(dataObj$f_data[,attr(dataObj, "cnames")$fdata_cname])] > 0),]
  }else if(length(as.character(dataObj$f_data[,getFDataColName(dataObj)])) > 1){
    df <- df[which(rowSums(df[,as.character(dataObj$f_data[,attr(dataObj, "cnames")$fdata_cname])] > 0, na.rm=TRUE) > 0),]
  }else{
    stop("No samples in object")
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