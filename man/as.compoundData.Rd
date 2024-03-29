% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.ftmsData.R
\name{as.compoundData}
\alias{as.compoundData}
\title{Convert Data to compoundData Class}
\usage{
as.compoundData(
  e_data,
  f_data,
  e_meta,
  edata_cname,
  fdata_cname,
  mass_cname,
  compound_cname,
  ...
)
}
\arguments{
\item{e_data}{a \eqn{p \times n + 1} data.frame of expression data, where \eqn{p} is the number of observed compounds and \eqn{n} is the number of samples. Each row corresponds to data for each peak. One column specifying a unique identifier for each peak/mass (row) must be present.}

\item{f_data}{a data.frame with \eqn{n} rows. Each row corresponds to a sample with one column giving the unique sample identifiers found in e_data column names and other columns providing qualitative and/or quantitative traits of each sample.}

\item{e_meta}{a data.frame with \eqn{p} rows. Each row corresponds to a compound with one column giving a unique peak/identifier (must be named the same as the column in \code{e_data}) and other columns giving meta information. At a minimum a column giving the mass of each peak and a column giving molecular formulae or columns giving elemental counts must be present.}

\item{edata_cname}{character string specifying the name of the column containing a unique identifier for each peak/mass in \code{e_data} and \code{e_meta}.}

\item{fdata_cname}{character string specifying the name of the column containing the sample identifiers in \code{f_data}.}

\item{mass_cname}{character string specifying the name of the column containing the peak/mass identifiers in \code{e_meta}. Note: this is often the same as \code{edata_cname} for cases where mass is used as a unique identifier.}

\item{compound_cname}{character string specifying the name of the column containing the compound identifier in \code{e_meta}. This is a compound identifier related to a database (e.g. MetaCyc)}

\item{...}{further arguments (see \code{\link{as.peakData}})}
}
\description{
Converts a list object or several data.frames of FT-MS data to an object of the class 'compoundData'. Objects of the class 'compoundData' are lists with three obligatory components \code{e_data}, \code{f_data}, and \code{e_meta}.
}
\details{
\code{as.compoundData} constructs a compoundData object which is an ftmsData object where the rows of \code{e_data} correspond to compounds.
}
