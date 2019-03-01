#' FTICR Expression Data
#'
#' A dataset containing expression data from a 12T FTICR instrument. 
#'
#' @format A data.frame with 2,442 rows (peaks/masses) and 21 columns (mass identifier and samples):
#' @rdname ftms12T_edata
#' @name ftms12T_edata
NULL

#' FTICR Sample Feature Data
#'
#' A dataset containing the sample metadata.
#'
#' @format A data.frame with 20 rows (samples) and 4 columns (sample identifier, location, block, and crop/flora):
#' \describe{
#'   \item{SampleID}{Sample identifier (matches column headers in edata)}
#'   \item{Location}{Location from which a sample was taken - 2 levels (M, W)}
#'   \item{Block}{Field block from which a sample was taken - 5 levels (1, 2, 3, 4, 5)}
#'   \item{Crop.Flora}{Crop/flora type of given sample - 2 levels (C, S)}
#' }
#' @rdname ftms12T_fdata
#' @name ftms12T_fdata
NULL

#' FTICR Peak Meta Information
#'
#' A dataset containing the meta information associated with each peak.
#'
#' @format A data.frame with 2,442 rows (peaks/masses) and 10 columns:
#' \describe{
#'   \item{Mass}{Mass identifier (matches Mass column in edata) - negative ion mass as samples were analyzed by electrospray ionization in negative mode}
#'   \item{C}{Number of Carbons making up identified compound}
#'   \item{H}{Number of Hydrogens making up identified compound}
#'   \item{O}{Number of Oxygens making up identified compound}
#'   \item{N}{Number of Nitrogens making up identified compound}
#'   \item{C13}{Indicates if compound contains Carbon13 if value is '1'}
#'   \item{S}{Number of Sulfurs making up identified compound}
#'   \item{P}{Number of Phosphorus making up identified compound}
#'   \item{Error}{Difference between the experimental mass and the accurate mass }
#'   \item{NeutralMass}{Experimental mass + mass of hydrogen - mass of electron}
#' }
#' @rdname ftms12T_emeta
#' @name ftms12T_emeta
NULL

#' FTICR Data Object of Class peakData
#'
#' An S3 object of class peakData
#'
#' @format A peakData object (see \code{\link{as.peakData}} for details)
#' \describe{
#'   \item{e_data}{a \eqn{p \times n + 1} data.frame of expression data, where \eqn{p} is the number of peaks observed and \eqn{n} is the number of samples. Each row corresponds to data for each peak/mass}
#'   \item{f_data}{a data.frame with \eqn{n} rows. Each row corresponds to a sample with one column giving the unique sample identifiers found in e_data column names and other columns providing qualitative and/or quantitative traits of each sample.}
#'   \item{e_meta}{a \eqn{p \times 10} data.frame of meta information for each peak/mass.}
#' }
#' 
#' @details Created by running the following code 
#' \preformatted{
#' as.peakData(e_data = ftms12T_edata, f_data = ftms12T_fdata, 
#'                e_meta = ftms12T_emeta, edata_cname = "Mass", mass_cname = "Mass", 
#'                fdata_cname = "SampleID",isotopic_cname = "C13", 
#'                isotopic_notation = "1",c_cname = "C", h_cname = "H", 
#'                o_cname = "O", n_cname = "N", s_cname = "S", p_cname = "P", 
#'                instrument_type = "12T")}
#' @rdname examplePeakData
#' @name examplePeakData
NULL

#' Processed FTICR Data Object of Class peakData
#'
#' An S3 object of class peakData
#'
#' @format A peakData object (see \code{\link{as.peakData}} for details)
#' \describe{
#'   \item{e_data}{a \eqn{p \times n + 1} data.frame of expression data, where \eqn{p} is the number of peaks observed and \eqn{n} is the number of samples. Each row corresponds to data for each peak/mass}
#'   \item{f_data}{a data.frame with \eqn{n} s. Each row corresponds to a sample with one column giving the unique sample identifiers found in e_data column names and other columns providing qualitative and/or quantitative traits of each sample.}
#'   \item{e_meta}{a \eqn{p \times 10} data.frame of meta information for each peak/mass.}
#' }
#' 
#' @details Created by running the following commands:
#' \preformatted{
#' exampleProcessedPeakData <- group_designation(examplePeakData, main_effects=c("Location", "Crop.Flora"))
#' exampleProcessedPeakData <- compound_calcs(exampleProcessedPeakData)
#' exampleProcessedPeakData <- applyFilt(mass_filter(exampleProcessedPeakData), exampleProcessedPeakData, min_mass=200, max_mass=900)
#' exampleProcessedPeakData <- applyFilt(molecule_filter(exampleProcessedPeakData), exampleProcessedPeakData, min=2)
#' }
#' @rdname exampleProcessedPeakData
#' @name exampleProcessedPeakData
NULL
