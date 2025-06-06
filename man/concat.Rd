% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concat.R
\name{concat}
\alias{concat}
\title{Combine multiple ftmsData objects}
\usage{
concat(...)
}
\arguments{
\item{...}{two or more ftmsData objects that are the output of \code{\link{summarizeGroups}} or \code{\link{summarizeGroupComparisons}}}
}
\description{
Combine multiple ftmsData objects into a single ftmsData object
for further analysis or visualizations. The inputs to this function are
the ftmsData objects created by \code{\link{divideByGroup}} or 
\code{\link{divideByGroupComparisons}}. The output is a new ftmsData
with all the data of the supplied inputs, in a form that can be 
summarized with \code{\link{summarizeGroups}} or \code{\link{summarizeGroupComparisons}}
respectively.
}
\examples{
data("exampleProcessedPeakData")
exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Location")
grp1 <- divideByGroup(exampleProcessedPeakData)

exampleProcessedPeakData <- group_designation(exampleProcessedPeakData, main_effects = "Crop.Flora")
grp2 <- divideByGroup(exampleProcessedPeakData)

allGrps <-concat(grp1, grp2)
grpSummaries <- summarizeGroups(allGrps, c("n_present", "prop_present"))
}
\author{
Amanda White
}
