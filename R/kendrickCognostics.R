#' Default cognostics for Kendrick plots in Trelliscope
#' 
#' The \code{kendrickCognostics} function provides a set of default cognostics
#' to be used with Kendrick plots in Trelliscope. The \code{kendrickCognostics}
#' function accepts the boundary set used for Van Krevelen class calculations
#' and (for \code{comparisonSummary} objects only) the name of the column to use
#' for identifying which peaks are observed in which group. It 
#' returns a function that may be applied to each \code{ftmsData} object, as is
#' appropriate for use with the \code{\link[trelliscope:makeDisplay]{makeDisplay}} function. See 
#' Examples section for use.
#'
#' @param vkBoundarySet Van Krevelen boundary set to use for calculating class proportions
#' @param uniquenessColName if \code{ftmsObj} is a group comparison summary object, what is the 
#' name of the column that specifies uniqueness to a group? If only one uniqueness function has
#' been applied this is unnecessary. (See \code{\link{summarizeGroupComparisons}}.)
#'
#' @return a function that may be applied to objects of type \code{peakData}, \code{groupSummary},
#' and \code{comparisonSummary}
#' @export
#'
#' @examples
#' \dontrun{
#' library(ftmsRanalysis)
#' library(trelliscope)
#' 
#' vdbDir <- vdbConn(file.path(tempdir(), "trell_test"), autoYes = TRUE)
#' data('exampleProcessedPeakData')
#' 
#' ## Kendrick plot for each sample
#' sampleDdo <- divideBySample(exampleProcessedPeakData)
#' panelFn1 <- panelFunctionGenerator("kendrickPlot", vkBoundarySet="bs2", title="Test")
#' 
#' # Note: make sure the same vkBoundarySet value is provided to the panel and cognostics functions
#' makeDisplay(sampleDdo, 
#'          panelFn=panelFn1,
#'          cogFn=kendrickCognostics(vkBoundarySet="bs2"),
#'          name = "Kendrick_plots_per_sample")
#'
#' ## Kendrick plots for group summaries
#' groupDdo <- divideByGroup(exampleProcessedPeakData)
#' groupSummaryDdo <- summarizeGroups(groupDdo, summary_functions = c("prop_present", "n_present"))
#' 
#' panelFn2 <- panelFunctionGenerator("kendrickPlot", colorCName=expr(paste0(getSplitVar(v, "Group"), "_n_present")), 
#'                                    legendTitle="Number<br>Present")
#' 
#' makeDisplay(grpCompSummaryDdo, 
#'          panelFn=panelFn2,
#'          cogFn=kendrickCognostics(),
#'          name = "Kendrick_plots_for_group_summaries")
#'          
#' view()
#' }
kendrickCognostics <- function(vkBoundarySet="bs1", uniquenessColName=NA) {
  fn <- function(ftmsObj) {
    
    cogs <- vanKrevelenCognostics(vkBoundarySet, uniquenessColName)(ftmsObj)
    
    divisionType <- ftmsRanalysis:::getDivisionType(ftmsObj)
    if (divisionType == "sample") {
      # add mean observed mass and defect
      
      sample_colnames <- as.character(ftmsObj$f_data[, getFDataColName(ftmsObj)])
      sample_colnames <- sample_colnames[sample_colnames %in% colnames(ftmsObj$e_data)]
      presInd <- ftmsRanalysis:::n_present(ftmsObj$e_data[, sample_colnames], 
                                           ftmsRanalysis:::getDataScale(ftmsObj)) > 0
      
      massColname <- ftmsRanalysis:::getKendrickMassColName(ftmsObj)
      defectColname <- ftmsRanalysis:::getKendrickDefectColName(ftmsObj)
      
      cogs <- c(cogs, list(
        mean_kendrick_mass = trelliscope::cog(val=mean(ftmsObj$e_meta[presInd, massColname], na.rm=TRUE),
                                              desc="Mean observed Kendrick mass"),
        mean_kendrick_defect = trelliscope::cog(val=mean(ftmsObj$e_meta[presInd, defectColname], na.rm=TRUE),
                                              desc="Mean observed Kendrick defect")
      ))
      
    }
    return(cogs)
  }
  return(fn)
}

    