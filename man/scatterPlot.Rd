% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatterPlot.R
\name{scatterPlot}
\alias{scatterPlot}
\title{Scatter Plot}
\usage{
scatterPlot(
  ftmsObj,
  xCName,
  yCName,
  colorCName = NA,
  colorPal = NA,
  xlabel = xCName,
  ylabel = yCName,
  legendTitle = colorCName,
  title = NA,
  xrange = NA,
  yrange = NA,
  logColorCol = FALSE,
  hoverTextCName = NA,
  zero.min = FALSE
)
}
\arguments{
\item{ftmsObj}{an object of class 'peakData' or 'compoundData', typically a
result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}.}

\item{xCName}{column name for x-axis, must be a column of
\code{ftmsObj$e_data} or \code{ftmsObj$e_meta}}

\item{yCName}{column name for y-axis, must be a column of
\code{ftmsObj$e_data} or \code{ftmsObj$e_meta}}

\item{colorCName}{column name for point colors, must be a column of
\code{ftmsObj$e_data} or \code{ftmsObj$e_meta}}

\item{colorPal}{color palette function, one of \code{\link{col_numeric}},
\code{\link{col_factor}} or similar \code{scales} palette function}

\item{xlabel}{x axis label, default is \code{xCName}}

\item{ylabel}{y axis label, default is \code{yCName}}

\item{legendTitle}{title for the legend, only used when coloring points
according to a numeric scale}

\item{title}{plot title}

\item{xrange}{x-axis bounds}

\item{yrange}{y-axis bounds}

\item{logColorCol}{TRUE/FALSE, should the color column be log-transformed? Default is FALSE.}

\item{hoverTextCName}{column name for hover (mouseover) text, must be a column of \code{ftmsObj$e_data} or \code{ftmsObj$e_meta}}

\item{zero.min}{TRUE/FALSE, if an axis range is not provided, should the automatic range minimum be set to 0?}
}
\value{
plotly object
}
\description{
Scatter Plot
}
\examples{
\dontrun{
scatterPlot(exampleProcessedPeakData,
            "NOSC", 
            "DBE", 
            colorCName="HtoC_ratio", 
            legendTitle="H:C Ratio", 
            title="DBE vs NOSC for exampleProcessedPeakData")
}
}
