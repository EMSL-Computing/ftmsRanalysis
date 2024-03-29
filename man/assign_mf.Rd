% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_mf.R
\name{assign_mf}
\alias{assign_mf}
\title{Assign Molecular Formulae Based on Element Counts}
\usage{
assign_mf(ftmsObj, metacyc = FALSE)
}
\arguments{
\item{ftmsObj}{an object of class 'ftmsData', typically a result of \code{\link{as.peakData}}. e_meta must be present.}

\item{metacyc}{TRUE/FALSE, use MetaCyc style formulae? (FALSE by default)}
}
\value{
an object of class 'ftmsData' with a column in \code{e_meta} giving the molecular formula.
}
\description{
Construct molecular formulae based on element counts
}
\details{
Assigns molecular formulae for observed peaks, when possible. Formulae are assigned in a manner so they can be matched to databases (e.g. MetaCyc). If a Carbon 13 column is given, formulae are not assigned to peaks where C13 is present.
}
\author{
Lisa Bramer
}
