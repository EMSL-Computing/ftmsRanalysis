% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_plot_methods.R
\name{plot.moleculeFilt}
\alias{plot.moleculeFilt}
\title{Plot molecule filter data}
\usage{
\method{plot}{moleculeFilt}(
  x,
  min_num = NA,
  title = NA,
  xlabel = "Minimum Number of Samples for which a Peak is Observed",
  ylabel = "Number of Peaks",
  ...
)
}
\arguments{
\item{x}{moleculeFilt object, created by \code{\link{molecule_filter}} function}

\item{min_num}{minimum number of observations for filtering (only affects graph coloring/labeling, no data filtering is performed in this step)}

\item{title}{title for plot}

\item{xlabel}{x-axis label for plot}

\item{ylabel}{y-axis label for plot}

\item{...}{other arguments}
}
\value{
plotly object
}
\description{
Plot molecule filter data
}
\examples{
\dontrun{
filter_obj <- molecule_filter(examplePeakData)
plot(filter_obj, min_num=2)
}
}
\author{
Amanda White
}
