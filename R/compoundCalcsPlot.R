#' Compound calcs plot of icrData object
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
#' @author Allison Thompson
#' 
#' @export
compoundCalcsPlot <- function(dataObj, variable, title=NULL, fill_col="blue", 
                         xlabel=NULL, ylabel="Density") {

  # Test inputs #
  if (!inherits(dataObj, "icrData")) {
    stop("dataObj must be of type icrData")
  }
  if (is.null(dataObj$e_meta)) {
    stop("dataObj must have e_meta element")
  }
  
  if (is.null(variable) | length(which(colnames(dataObj$e_meta) == variable)) != 1) {
    stop("variable must be a single column name found in dataObj$e_meta")
  }
  
  # End Tests #
  
  if(is.null(xlabel)){
    xlabel <- variable
  }
  
  # merge e_data and e_meta #  
  df <- merge(dataObj$e_data, dataObj$e_meta, by=getMassColName(dataObj))
  # subset down to where sample value != 0 #
  df <- df[which(df[,dataObj$f_data[,attr(dataObj, "cnames")$fdata_cname]] > 0),]
  # subset down to those able to be plotted #
  df <- df[which(!is.na(df[,variable])),]
  

  map <- ggplot2::aes_string(x = variable)

  p = ggplot2::ggplot(data = df, map) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), alpha = 0.5, bins = 25) +
    ggplot2::geom_density(fill = fill_col, alpha = 0.25) +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) +
    ggplot2::ggtitle(title)
  
  ggplotly(p)
}