% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colname_methods.R
\name{getElementColName}
\alias{getElementColName}
\title{Get the name of the column for the specified element}
\usage{
getElementColName(ftmsObj, element)
}
\arguments{
\item{ftmsObj}{an object of type ftmsData}

\item{element}{a string specifying the element or isotope name under its standard abbreviation}
}
\value{
name of element column
}
\description{
Returns the name of the column in e_meta for the specified element that 
contains element count data for calculations.
}
