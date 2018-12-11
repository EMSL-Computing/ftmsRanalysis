#' Default cognostics for Kendrick plots in Trelliscope
#' 
#' The kendrickCognostics function provides a set of default cognostics
#' to be used with Kendrick plots in Trelliscope.
#'
#' @param icrData 
#' @param vkBoundarySet Van Krevelen boundary set to use for calculating class proportions
#' @param uniquenessColName if \code{icrData} is a group comparison summary object, what is the 
#' name of the column that specifies uniqueness to a group? If only one uniqueness function has
#' been applied this is unnecessary. (See \code{\link{summarizeComparisons}}.)
#'
#' @return list of cognostics
#' @export
#'
# @examples
kendrickCognostics <- function(icrData, vkBoundarySet="bs1", uniquenessColName=NA) {
  
  cogs <- vanKrevelenCognostics(icrData, vkBoundarySet, uniquenessColName)
  
  divisionType <- fticRanalysis:::getDivisionType(icrData)
  if (divisionType == "sample") {
    # add mean observed mass and defect
    
    sample_colnames <- as.character(icrData$f_data[, getFDataColName(icrData)])
    sample_colnames <- sample_colnames[sample_colnames %in% colnames(icrData$e_data)]
    presInd <- fticRanalysis:::n_present(icrData$e_data[, sample_colnames], 
                                         fticRanalysis:::getDataScale(icrData)) > 0
    
    massColname <- fticRanalysis:::getKendrickMassColName(icrData)
    defectColname <- fticRanalysis:::getKendrickDefectColName(icrData)
    
    cogs <- c(cogs, list(
      mean_kendrick_mass = trelliscope::cog(val=mean(icrData$e_meta[presInd, massColname], na.rm=TRUE),
                                            desc="Mean observed Kendrick mass"),
      mean_kendrick_defect = trelliscope::cog(val=mean(icrData$e_meta[presInd, defectColname], na.rm=TRUE),
                                            desc="Mean observed Kendrick defect")
    ))
    
  }
  return(cogs)
}

    