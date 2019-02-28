#' Default cognostics for density plots in Trelliscope
#' 
#' The \code{densityCognostics} function provides a set of default cognostics
#' to be used with density plots in Trelliscope. The \code{densityCognostics}
#' function accepts the name of the variable used for the density plot and
#' returns a function that may be applied to each \code{icrData} object, as is
#' appropriate for use with the \code{\link{makeDisplay}} function. See 
#' Examples section for use.
#'
#' @param variable column name of column in \code{e_meta} which should be plotted. Must be one of the column names in \code{icrData$e_meta} that contains numeric values.
#' 
#' @return a function that may be applied to objects of type \code{peakData} and \code{groupSummary}
#' @export
#'
#' @examples
#' \dontrun{
#' library(fticRanalysis)
#' library(trelliscope)
#' 
#' vdbDir <- vdbConn(file.path(tempdir(), "trell_test"), autoYes = TRUE)
#' data('exampleProcessedPeakData')
#' 
#' ## Plot density of NOSC variable for each sample
#' sampleDdo <- divideBySample(exampleProcessedPeakData)
#' panelFn1 <- panelFunctionGenerator("densityPlot", variable="NOSC")
#' makeDisplay(sampleDdo, 
#'             panelFn=panelFn1,
#'             cogFn=densityCognostics("NOSC"),
#'             name = "NOSC_density_by_sample",
#'             group = "Sample")
#'
#' ## Plot density of NOSC for each group
#' groupDdo <- divideByGroup(exampleProcessedPeakData)
#' panelFn2 <- panelFunctionGenerator("densityPlot", variable="NOSC", groups=NA)
#' 
#' makeDisplay(groupDdo, 
#'             panelFn=panelFn2,
#'             cogFn=densityCognostics("NOSC"),
#'             name = "NOSC_density_by_group",
#'             group = "Group")
#' 
#' view()
#' }
densityCognostics <- function(variable) {
  fn <- function(icrData) {
    divisionType <- fticRanalysis:::getDivisionType(icrData)
    if (divisionType == "sample" | divisionType == "group") {
      sample_colnames <- as.character(icrData$f_data[, getFDataColName(icrData)])
      sample_colnames <- sample_colnames[sample_colnames %in% colnames(icrData$e_data)]
      presInd <- fticRanalysis:::n_present(icrData$e_data[, sample_colnames], 
                                           fticRanalysis:::getDataScale(icrData)) > 0
      
      cogs <- fticRanalysis:::commonDensityCognostics(icrData, variable, presInd)
      
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
      cogs <- fticRanalysis:::commonDensityCognostics(icrData, variable, presInd)
      
      cogs <- c(cogs, fticRanalysis:::groupCognostics(icrData))
      return(cogs)
      
    } else if (divisionType == "groupComparison") {
      cogs <- fticRanalysis:::comparisonDensityCognostics(icrData, variable)
      return(cogs)
    } else {
      stop(sprintf("densityCognostics doesn't work with objects of this type (%s)", divisionType))
    }
  }
  return(fn)
  
}

# Internal function: VK cogs common to both sample and group
commonDensityCognostics <- function(icrData, variable, presenceIndicator) {

  .data <- dplyr::pull(icrData$e_meta, variable)[presenceIndicator]
  cogs <- list(
    num_peaks=trelliscope::cog(val = sum(presenceIndicator, na.rm=TRUE), desc="Number of peaks observed"),
    mean=trelliscope::cog(val=mean(.data, na.rm=TRUE), desc=sprintf("Mean of %s", variable)),
    median=trelliscope::cog(val=median(.data, na.rm=TRUE), desc=sprintf("Median of %s", variable)),
    stdev=trelliscope::cog(val=sqrt(var(.data, na.rm=TRUE)), desc=sprintf("Standard deviation of %s", variable)),
    skewness=trelliscope::cog(val=skewness(.data), desc=sprintf("Skewness of %s", variable))
  )
  return(cogs)
}

# Cognostics for group comparison and comparison summary icrData objects
comparisonDensityCognostics <- function(icrData, variable, uniquenessColName=NA) {
  groupDF <- getGroupDF(icrData)
  if (is.null(groupDF)) stop("Invalid icrData object, no group definition found")
  groups <- as.character(unique(groupDF$Group))

  if (inherits(icrData, "groupComparison")) {
    sampColName <- getFDataColName(icrData)
    groupList <- lapply(groups, function(g) as.character(groupDF[groupDF[,"Group"] == g, sampColName]))
    names(groupList) <- groups  
    
    presInd1 <- fticRanalysis:::n_present(icrData$e_data[, groupList[[1]]], 
                                         fticRanalysis:::getDataScale(icrData)) > 0
    presInd2 <- fticRanalysis:::n_present(icrData$e_data[, groupList[[2]]], 
                                          fticRanalysis:::getDataScale(icrData)) > 0
    
  # } else if (inherits(icrData, "comparisonSummary")) {
  #   if (identical(uniquenessColName, NA)) {
  #     uniquenessColName <- setdiff(colnames(icrData$e_data), getEDataColName(icrData))
  #     if (length(uniquenessColName) != 1) stop("Cannot determine with column to use for uniqueness, please specify 'uniquenessColName' parameter")
  #   }
  #   indNa <- is.na(icrData$e_data[, uniquenessColName])
  #   uniqueCol <- as.character(icrData$e_data[, uniquenessColName])
  #   presInd1 <- !indNa & (uniqueCol == sprintf("Unique to %s", groups[1]) | uniqueCol == "Observed in Both")
  #   presInd2 <- !indNa & (uniqueCol == sprintf("Unique to %s", groups[2]) | uniqueCol == "Observed in Both")
  #   
  } else {
    stop("icrData must be of class 'groupComparison'")
  }  
  
  .data.g1 <- dplyr::pull(icrData$e_meta, variable)[presInd1]
  .data.g2 <- dplyr::pull(icrData$e_meta, variable)[presInd2]

  ks.val <- suppressWarnings(ks.test(.data.g1, .data.g2, alternative = "two.sided"))
  cogs <- list(
    group1=trelliscope::cog(val=groups[1], desc="Group 1"),
    group2=trelliscope::cog(val=groups[2], desc="Group 2"),
    ks.statistic=trelliscope::cog(val =ks.val$statistic, desc="Kolmogorov-Smirnov statistic for two-sided test"),
    ks.pvalue=trelliscope::cog(val =ks.val$p.value, desc="P-value of Kolmogorov-Smirnov statistic for two-sided test"),
    num_peaks_g1=trelliscope::cog(val=sum(presInd1), desc="Number of peaks observed in group 1"),
    num_peaks_g2=trelliscope::cog(val=sum(presInd2), desc="Number of peaks observed in group 2"),
    mean_diff_group1_minus_group2=trelliscope::cog(val=mean(.data.g1, na.rm=TRUE)-mean(.data.g2, na.rm=TRUE), 
                                             desc=sprintf("Difference of mean values of %s (group 1 - group 2)", variable)),
    median_diff_group1_minus_group2=trelliscope::cog(val=median(.data.g1, na.rm=TRUE)-median(.data.g2, na.rm=TRUE), 
                                             desc=sprintf("Difference of median values of %s (group 1 - group 2)", variable)),
    stdev_diff_group1_minus_group2=trelliscope::cog(val=sqrt(var(.data.g1, na.rm=TRUE))-sqrt(var(.data.g2, na.rm=TRUE)), 
                                             desc=sprintf("Difference of square root values of %s (group 1 - group 2)", variable)),
    skewness_diff_group1_minus_group2=trelliscope::cog(val=skewness(.data.g1)-skewness(.data.g2), 
                                              desc=sprintf("Difference of skewness values of %s (group 1 - group 2)", variable))
  )

  return(cogs)
}

# Internal function to calculate skewness
skewness <- function(vals) {
  n <- sum(!is.na(vals))
  if (is.na(n) | n == 0) return(NA)
  m_vals <- mean(vals, na.rm=TRUE)
  numer <- sum((vals-m_vals)^3, na.rm=TRUE)/n
  denom <- sqrt(var(vals, na.rm=TRUE))^3
  return(numer/denom)
}
