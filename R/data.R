#' FTICR Expression Data
#'
#' A dataset containing expression data from a 12T FTICR instrument. 
#'
#' @format A data.frame with 2,442 rows (peaks/masses) and 21 columns (mass identifier and samples):
#' @rdname fticr12T_edata
#' @name fticr12T_edata
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
#' @rdname fticr12T_fdata
#' @name fticr12T_fdata
NULL

#' FTICR Peak Meta Information
#'
#' A dataset containing the meta information associated with each peak.
#'
#' @format A data.frame with 2,442 rows (peaks/masses) and 10 columns:
#' \describe{
#'   \item{Mass}{Mass identifier (matches Mass column in edata)}
#'   \item{C}{Number of Carbons making up identified compound}
#'   \item{H}{Number of Hydrogens making up identified compound}
#'   \item{O}{Number of Oxygens making up identified compound}
#'   \item{N}{Number of Nitrogens making up identified compound}
#'   \item{C13}{Indicates if compound contains Carbon13 if value is '1'}
#'   \item{S}{Number of Sulfurs making up identified compound}
#'   \item{P}{Number of Phosphorus making up identified compound}
#'   \item{Error}{ }
#'   \item{NeutralMass}{}
#' }
#' @rdname fticr12T_emeta
#' @name fticr12T_emeta
NULL

#' FTICR Data Object of Class peakIcrData
#'
#' An S3 object of class peakIcrData
#'
#' @format A peakIcrData object (see \code{\link{as.peakIcrData}} for details)
#' \describe{
#'   \item{e_data}{a \eqn{p \times n + 1} data.frame of expression data, where \eqn{p} is the number of peaks observed and \eqn{n} is the number of samples. Each row corresponds to data for each peak/mass}
#'   \item{f_data}{a data.frame with \eqn{n} rows. Each row corresponds to a sample with one column giving the unique sample identifiers found in e_data column names and other columns providing qualitative and/or quantitative traits of each sample.}
#'   \item{e_meta}{a \eqn{p \times 10} data.frame of meta information for each peak/mass.}
#' }
#' 
#' @details Created by running the following code 'as.peakIcrData(e_data = fticr12T_edata, f_data = fticr12T_fdata, e_meta = fticr12T_emeta, edata_cname = "Mass", mass_cname = "Mass", fdata_cname = "SampleID",isotopic_cname = "C13", isotopic_notation = "1",c_cname = "C", h_cname = "H", o_cname = "O", n_cname = "N", s_cname = "S", p_cname = "P",instrument_type = "12T")'
#' @rdname peakIcrData
#' @name peakIcrData
NULL

#' Processed FTICR Data Object of Class peakIcrData
#'
#' An S3 object of class peakIcrData
#'
#' @format A peakIcrData object (see \code{\link{as.peakIcrData}} for details)
#' \describe{
#'   \item{e_data}{a \eqn{p \times n + 1} data.frame of expression data, where \eqn{p} is the number of peaks observed and \eqn{n} is the number of samples. Each row corresponds to data for each peak/mass}
#'   \item{f_data}{a data.frame with \eqn{n} rows. Each row corresponds to a sample with one column giving the unique sample identifiers found in e_data column names and other columns providing qualitative and/or quantitative traits of each sample.}
#'   \item{e_meta}{a \eqn{p \times 10} data.frame of meta information for each peak/mass.}
#' }
#' 
#' @details Created by running the following code 'as.peakIcrData(e_data = fticr12T_edata, f_data = fticr12T_fdata, e_meta = fticr12T_emeta, edata_cname = "Mass", mass_cname = "Mass", fdata_cname = "SampleID",isotopic_cname = "C13", isotopic_notation = "1",c_cname = "C", h_cname = "H", o_cname = "O", n_cname = "N", s_cname = "S", p_cname = "P", instrument_type = "12T")'. Then \code{\link{group_designation}} was run and filters to restrict peaks to those with a mass between 200 and 900 which had been seen in at least two samples were implemented.
#' @rdname peakIcrData
#' @name peakIcrData
NULL
