
#' Default cognostics for Van Krevelen plots in Trelliscope
#' 
#' The \code{vanKrevelenCognostics} function provides a set of default cognostics
#' to be used with Van Krevelen plots in Trelliscope. The \code{vanKrevelenCognostics}
#' function accepts the boundary set used for Van Krevelen class calculations
#' and (for \code{comparisonSummary} objects only) the name of the column to use
#' for identifying which peaks are observed in which group. It 
#' returns a function that may be applied to each \code{icrData} object, as is
#' appropriate for use with the \code{\link{makeDisplay}} function. See 
#' Examples section for use.
#'
#' @param vkBoundarySet Van Krevelen boundary set to use for calculating class proportions
#' @param uniquenessColName if \code{icrData} is a group comparison summary object, what is the 
#' name of the column that specifies uniqueness to a group? If only one uniqueness function has
#' been applied this is unnecessary. (See \code{\link{summarizeComparisons}}.)
#'
#' @return a function that may be applied to objects of type \code{peakIcrData}, \code{groupSummary},
#' and \code{comparisonSummary}
#' @export
#'
#' @examples
#' \dontrun{
#' library(fticRanalysis)
#' library(trelliscope)
#' 
#' vdbDir <- vdbConn(file.path(tempdir(), "trell_test"), autoYes = TRUE)
#' data('peakIcrProcessed')
#' 
#' ## Van Krevelen plot for each sample
#' sampleDdo <- divideBySample(peakIcrProcessed)
#' panelFn1 <- panelFunctionGenerator("vanKrevelenPlot", vkBoundarySet="bs2", title="Test")
#' 
#' # Note: make sure the same vkBoundarySet value is provided to the panel and cognostics functions
#' makeDisplay(sampleDdo, 
#'          panelFn=panelFn1,
#'          cogFn=vanKrevelenCognostics(vkBoundarySet="bs2"),
#'          name = "Van_Krevelen_plots_per_sample")
#'
#' ## Van Krevelen plots for group comparison summaries
#' grpCompDdo <- divideByGroupComparisons(peakIcrProcessed, "all")
#' grpCompSummaryDdo <- summarizeGroupComparisons(grpCompDdo, summary_functions="uniqueness_gtest", 
#'                                             summary_function_params=list(uniqueness_gtest=list(pres_fn="nsamps", pres_thresh=2, pvalue_thresh=0.05)))
#' 
#' panelFn2 <- panelFunctionGenerator("vanKrevelenPlot", colorCName="uniqueness_gtest")
#' 
#' # Note: uniquenessColName parameter tells vanKrevelenCognostics which column to use to determine 
#' # group uniqueness for each peak. If only one summary function is used in summarizeGroupComparisons
#' # then it will be inferred, otherwise it's necessary to specify. 
#' makeDisplay(grpCompSummaryDdo, 
#'          panelFn=panelFn2,
#'          cogFn=vanKrevelenCognostics(uniquenessColName="uniqueness_gtest"),
#'          name = "Van_Krevelen_plots_for_group_comparison_summaries")
#'          
#' view()
#' }
vanKrevelenCognostics <- function(vkBoundarySet="bs1", uniquenessColName=NA) {
  fn <- function(icrData) {
    divisionType <- fticRanalysis:::getDivisionType(icrData)
    if (divisionType == "sample" | divisionType == "group") {
      sample_colnames <- as.character(icrData$f_data[, getFDataColName(icrData)])
      sample_colnames <- sample_colnames[sample_colnames %in% colnames(icrData$e_data)]
      presInd <- fticRanalysis:::n_present(icrData$e_data[, sample_colnames], 
                                           fticRanalysis:::getDataScale(icrData)) > 0
      
      cogs <- fticRanalysis:::commonVanKrevelenCognostics(icrData, presInd, vkBoundarySet=vkBoundarySet)
      
      if (divisionType == "sample") {
        cogs <- c(cogs, fticRanalysis:::sampleCognostics(icrData))
      } else {
        cogs <- c(cogs, fticRanalysis:::groupCognostics(icrData))
      }
      return(cogs)
      
    } else if (divisionType == "groupSummary") {
      cname <- grep(pattern = ".*_n_present", x = colnames(icrData$e_data), value=TRUE)
      if (length(cname) == 0) {
        cname <- grep(pattern = ".*_prop_present", x = colnames(icrData$e_data), value=TRUE)
      }
      if (length(cname) == 0) stop("Cannot find appropriate group summary column of e_data, looking for 'n_present' or 'prop_present'")
      
      presInd <- icrData$e_data[, cname] > 0 
      cogs <- fticRanalysis:::commonVanKrevelenCognostics(icrData, presInd, vkBoundarySet=vkBoundarySet)
      
      cogs <- c(cogs, fticRanalysis:::groupCognostics(icrData))
      return(cogs)
    
    } else if (divisionType == "comparisonSummary") {
      cogs <- fticRanalysis:::comparisonSummaryVanKrevelenCognostics(icrData, vkBoundarySet=vkBoundarySet, uniquenessColName=uniquenessColName)
      return(cogs)
    } else {
      stop(sprintf("vanKrevelenCognostics doesn't work with objects of this type (%s)", divisionType))
    }
  }
  return(fn)
  
}

# Internal function: VK cogs common to both sample and group
commonVanKrevelenCognostics <- function(icrData, presenceIndicator, vkBoundarySet="bs1") {
  vkColname <- getVKColName(icrData, vkBoundarySet)
  if (is.null(vkColname)) {
    icrData <- assign_class(icrData, boundary_set = vkBoundarySet)
    vkColname <- getVKColName(icrData, vkBoundarySet)
  }
  
  vkClasses = icrData$e_meta[, vkColname]
  vkClasses <- strsplit(vkClasses, ";")

  denom <- sum(presenceIndicator)
  allClasses <- rownames(getVanKrevelenCategoryBounds(vkBoundarySet)$VKbounds)
  classProportions <- lapply(allClasses, function(cc) {
    cog(val=sum(unlist(vkClasses[which(presenceIndicator)]) == cc, na.rm=TRUE)/denom, desc=sprintf("Proportion of observed peaks of class %s", cc))
  })
  names(classProportions) <- paste("prop_", gsub(" ", "_", allClasses), sep="")
  
  cogs <- list(
    num_peaks=trelliscope::cog(val = denom, desc="Number of peaks observed")
  )
  cogs <- c(cogs, classProportions)
  return(cogs)
}

# Cogs for samples: f_data columns
sampleCognostics <- function(icrData) {
  # add f_data columns
  fdata_cols <- setdiff(colnames(icrData$f_data), getFDataColName(icrData))
  more_cogs <- lapply(fdata_cols, function(cc) {
    cog(val=icrData$f_data[1, cc], desc=cc)
  })
  names(more_cogs) <- fdata_cols
  return(more_cogs)
}

# Cogs for groups: group defining columns of f_data
groupCognostics <- function(icrData) {
  groupDF <- fticRanalysis:::getGroupDF(icrData)
  if (is.null(groupDF)) stop("Invalid icrData object, no group definition found")
  cols <- c(attr(groupDF, "main_effects"), attr(groupDF, "covariates"))
  more_cogs <- lapply(cols, function(cc) {
    cog(val=groupDF[1, cc], desc=cc)
  })
  names(more_cogs) <- cols
  return(more_cogs)
}

# Cognostics for comparison summary icrData objects
comparisonSummaryVanKrevelenCognostics <- function(icrData, vkBoundarySet="bs1", uniquenessColName=NA) {
  if (!inherits(icrData, "comparisonSummary")) stop("icrData must be of class 'comparisonSummary'")
  groupDF <- getGroupDF(icrData)
  if (is.null(groupDF)) stop("Invalid icrData object, no group definition found")
  
  groups <- as.character(unique(groupDF$Group))
  sampColName <- getFDataColName(icrData)
  groupList <- lapply(groups, function(g) as.character(groupDF[groupDF[,"Group"] == g, sampColName]))
  names(groupList) <- groups  
  
  if (identical(uniquenessColName, NA)) {
    uniquenessColName <- setdiff(colnames(icrData$e_data), getEDataColName(icrData))
    if (length(uniquenessColName) != 1) stop("Cannot determine with column to use for uniqueness, please specify 'uniquenessColName' parameter")
  }
  presInd <- lapply(groups, function(g) {
    !is.na(icrData$e_data[, uniquenessColName]) & as.character(icrData$e_data[, uniquenessColName]) == sprintf("Unique to %s", g)
  })
  names(presInd) <- groups
  
  vkColname <- getVKColName(icrData, vkBoundarySet)
  if (is.null(vkColname)) {
    icrData <- assign_class(icrData, boundary_set = vkBoundarySet)
    vkColname <- getVKColName(icrData, vkBoundarySet)
  }
  
  vkClasses <- icrData$e_meta[, vkColname]
  vkClasses <- strsplit(vkClasses, ";")
  #vkClasses <- vkClasses[!is.na(vkClasses)]
  denom <- sum(!is.na(icrData$e_data[, uniquenessColName])) #count how many peaks observed in any group
  allClasses <- rownames(getVanKrevelenCategoryBounds(vkBoundarySet)$VKbounds)
  classProportions <- lapply(1:2, function(g) {
    x <- lapply(allClasses, function(cc) {
      trelliscope::cog(val=sum(unlist(vkClasses[which(presInd[[g]])]) == cc, na.rm=TRUE)/denom, 
          desc=sprintf("Proportion of observed peaks of class %s unique to group %s", cc, g))
    })
    names(x) <- paste("prop_unique_", gsub(" ", "_", allClasses), "_group_", g, sep="")
    return(x)
  })
  classProportions <- do.call(c, classProportions)
  
  cogs <- list(
    group1=trelliscope::cog(val=groups[1], desc="Group 1"),
    group2=trelliscope::cog(val=groups[2], desc="Group 2"),
    num_peaks_common=trelliscope::cog(val = sum(icrData$e_data[, uniquenessColName] == "Observed in Both", na.rm=TRUE), desc="Number of peaks observed in both groups"),
    num_peaks_g1=trelliscope::cog(val=sum(presInd[[1]]), desc="Number of peaks unique to group 1"),
    num_peaks_g2=trelliscope::cog(val=sum(presInd[[2]]), desc="Number of peaks unique to group 2")
  )
#  names(cogs)[2:3] <- gsub(" ", "_", paste("num_peaks_", groups, sep=""))
  cogs <- c(cogs, classProportions)
  return(cogs)
}

# Internal only function to determine if an icrData object is divided by "sample", "group", "groupSummary", "groupComparison"
getDivisionType <- function(icrData) {
  svars <- getSplitVars(icrData) 
  if (getFDataColName(icrData) %in% names(svars)) {
    return("sample")
  } else if ("Group" %in% names(svars)) {
    if (inherits(icrData, "groupSummary")) {
      return("groupSummary")
    } else {
      return("group")
    }
  } else if (inherits(icrData, "groupComparison")) {
    return("groupComparison")
  } else if (inherits(icrData, "comparisonSummary")) {
    return("comparisonSummary")
  } else {
    stop("Unknown division type")
  }
}

getVKColName <- function(icrData, vkBoundarySet) {
  vkColname = switch(vkBoundarySet,
                     bs1=getBS1ColName(icrData),
                     bs2=getBS2ColName(icrData),
                     bs3=getBS3ColName(icrData)
  )
  return(vkColname)
}
