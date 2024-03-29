% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.CoreMSData.R
\name{plot.CoreMSData}
\alias{plot.CoreMSData}
\title{Plot Method for CoreMSData Objects}
\usage{
\method{plot}{CoreMSData}(
  x,
  title = "Unique Masses per Sample",
  xlabel = "Sample",
  ylabel = "Unique Masses",
  rotate_x_labs = FALSE,
  ...
)
}
\arguments{
\item{x}{CoreMSData object}

\item{title}{optional, if not specified "Unique Masses per Sample" will be used}

\item{xlabel}{optional, if not specified "Sample" will be used}

\item{ylabel}{optional, if not specified "Unique Masses" will be used}

\item{rotate_x_labs}{logical, optionally angles sample names on x-axis for readability if there are many samples and/or long sample names}

\item{...}{included for compliance with generic method}
}
\value{
`ggplot` object
}
\description{
Initial plot for CoreMSData objets showing number of unique masses per sample/file
}
