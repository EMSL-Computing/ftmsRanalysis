#' Default cognostics for density plots in Trelliscope
#' 
#' The densityCognostics function provides a set of default cognostics
#' to be used with density plots in Trelliscope.
#'
#' @param icrData 
#' @param variable column name of column in \code{e_meta} which should be plotted. Must be one of the column names in \code{icrData$e_meta} that contains numeric values.
#' 
#' @return list of cognostics
#' @export
#'
# @examples
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
      
    } else if (divisionType == "group_summary") {
      cname <- grep(pattern = ".*_n_present", x = colnames(icrData$e_data), value=TRUE)
      if (length(cname) == 0) {
        cname <- grep(pattern = ".*_prop_present", x = colnames(icrData$e_data), value=TRUE)
      }
      if (length(cname) == 0) stop("Cannot find appropriate group summary column of e_data, looking for 'n_present' or 'prop_present'")
      
      presInd <- icrData$e_data[, cname] > 0 
      cogs <- fticRanalysis:::commonDensityCognostics(icrData, variable, presInd)
      
      cogs <- c(cogs, fticRanalysis:::groupCognostics(icrData))
      return(cogs)
      
    } else if (divisionType == "group_comparison") {
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
    variance=trelliscope::cog(val=var(.data, na.rm=TRUE), desc=sprintf("Variance of %s", variable)),
    skewness=trelliscope::cog(val=skewness(.data), desc=sprintf("Skewness of %s", variable))
  )
  return(cogs)
}

# Cognostics for group comparison icrData objects
comparisonDensityCognostics <- function(icrData, variable) {
  if (!inherits(icrData, "groupComparison")) stop("icrData must be of class 'groupComparison'")
  groupDF <- getGroupDF(icrData)
  if (is.null(groupDF)) stop("Invalid icrData object, no group definition found")
  
  groups <- as.character(unique(groupDF$Group))
  sampColName <- getFDataColName(icrData)
  groupList <- lapply(groups, function(g) as.character(groupDF[groupDF[,"Group"] == g, sampColName]))
  names(groupList) <- groups  
  
  presInd1 <- fticRanalysis:::n_present(icrData$e_data[, groupList[[1]]], 
                                       fticRanalysis:::getDataScale(icrData)) > 0
  presInd2 <- fticRanalysis:::n_present(icrData$e_data[, groupList[[2]]], 
                                        fticRanalysis:::getDataScale(icrData)) > 0
  
  .data.g1 <- dplyr::pull(icrData$e_meta, variable)[presInd1]
  .data.g2 <- dplyr::pull(icrData$e_meta, variable)[presInd2]

  ks.val <- suppressWarnings(ks.test(.data.g1, .data.g2, alternative = "two.sided"))
  cogs <- list(
    group1=trelliscope::cog(val=groups[1], desc="Group 1"),
    group2=trelliscope::cog(val=groups[2], desc="Group 2"),
    ks.statistic=trelliscope::cog(val =ks.val$statistic, desc="Kolmogorov-Smirnov statistic for two-sided test"),
    ks.pvalue=trelliscope::cog(val =ks.val$p.value, desc="P-value of Kolmogorov-Smirnov statistic for two-sided test"),
    num_peaks_g1=trelliscope::cog(val=sum(presInd1), desc="Number of peaks observed in group 1"),
    mean_group1=trelliscope::cog(val=mean(.data.g1, na.rm=TRUE), desc=sprintf("Mean of %s for peaks observed in group 1", variable)),
    median_group1=trelliscope::cog(val=median(.data.g1, na.rm=TRUE), desc=sprintf("Median of %s for peaks observed in group 1", variable)),
    variance_group1=trelliscope::cog(val=var(.data.g1, na.rm=TRUE), desc=sprintf("Variance of %s for peaks observed in group 1", variable)),
    skewness_group1=trelliscope::cog(val=skewness(.data.g1), desc=sprintf("Skewness of %s for peaks observed in group 1", variable)),
    num_peaks_g2=trelliscope::cog(val=sum(presInd2), desc="Number of peaks observed in group 2"),
    mean_group2=trelliscope::cog(val=mean(.data.g2, na.rm=TRUE), desc=sprintf("Mean of %s for peaks observed in group 2", variable)),
    median_group2=trelliscope::cog(val=median(.data.g2, na.rm=TRUE), desc=sprintf("Median of %s for peaks observed in group 2", variable)),
    variance_group2=trelliscope::cog(val=var(.data.g2, na.rm=TRUE), desc=sprintf("Variance of %s for peaks observed in group 2", variable)),
    skewness_group2=trelliscope::cog(val=skewness(.data.g2), desc=sprintf("Skewness of %s for peaks observed in group 2", variable))
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
