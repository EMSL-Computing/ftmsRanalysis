% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.confFilt.R
\name{plot.confFilt}
\alias{plot.confFilt}
\title{Plot confidence filter object}
\usage{
\method{plot}{confFilt}(
  x,
  title = "Peaks Retained Under Confidence Filter",
  xlabel = "Minimum Confidence Threshold",
  ylabel = "Monoisotopic Peaks Retained",
  ...
)
}
\arguments{
\item{x}{object of class 'confFilt' created by \code{\link{conf_filter}} function}

\item{title}{(optional) plot title}

\item{xlabel}{x axis label, defaults to "Minimum Confidence Threshold"}

\item{ylabel}{y axis label, defaults to "Number of Peaks"}

\item{...}{additional arguments}
}
\value{
`plotly` object
}
\description{
Plot confidence filter object
}
\author{
Natalie Winans
}
