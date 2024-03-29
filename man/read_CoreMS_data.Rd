% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_CoreMS_data.R
\name{read_CoreMS_data}
\alias{read_CoreMS_data}
\title{Read in CoreMS output}
\usage{
read_CoreMS_data(list_of_files, sample_names = NULL)
}
\arguments{
\item{list_of_files}{a vector of character strings specifying the data files
(in .csv format) to be read in and concatenated}

\item{sample_names}{(optional) a character vector denoting preferred sample names}
}
\value{
CoreMSrbind object
}
\description{
Reads in CoreMS output file(s) in CSV format as a single `data.frame` with class `CoreMSrbind`
}
\details{
\code{read_CoreMS_data} reads in selected CSV files, appends
  'filename' column, verifies identical column names, and combines all files
  into a single `data.frame`/`CoreMSrbind` object
}
\author{
Natalie Winans
}
