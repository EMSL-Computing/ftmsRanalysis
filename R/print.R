# Print methods for peakData, compoundData, reactionData and moduleData

#' @export
print.peakData <- function(peakObj) {
  if (!inherits(peakObj, "peakData")) stop("Not a peakData object")
  if (is.null(peakObj) | all(is.na(peakObj))) {
    print(peakObj)
    invisible(peakObj)
  }
  res <- c("peakData object", 
           sprintf("# Peaks: %d", nrow(peakObj$e_data)),
           sprintf("# Samples: %d", nrow(peakObj$f_data)))
  
  res <- c(res, .common_print_data(peakObj))
    
  cat(res, sep="\n")
  invisible(peakObj)
}

# internal only method for constructing print info for common parts of ftmsData objects
.common_print_data <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) stop("Not a ftmsData object")
  
  res <- NULL
  if (!is.null(ftmsObj$e_meta)) {
    res <- c(res, sprintf("Meta data columns: [%s]", paste(colnames(ftmsObj$e_meta), collapse = ", ")))
  }
  
  if (!is.null(getGroupDF(ftmsObj))) {
    group.df <- getGroupDF(ftmsObj)
    group.str <- sprintf("Group info: main effects=[%s]", paste(attr(group.df, "main_effects"), collapse=", "))
    if (!is.null(attr(group.df, "covariates"))) {
      group.str <- sprintf("%s, covariates=[%s]", group.str, paste(attr(group.df, "covariates"), collapse=", "))
    }
    res <- c(res, group.str)
  }

  if (!is.null(attr(ftmsObj, "filters"))) {
    res <- c(res, unlist(lapply(attr(ftmsObj, "filters"), function(x) return(x$report_text))))
  }
  
  return(res)
}

#' @export
print.compoundData <- function(compObj) {
  if (!inherits(compObj, "compoundData")) stop("Not a compoundData object")
  if (is.null(compObj) | all(is.na(compObj))) {
    print(compObj)
    invisible(compObj)
  }
  res <- c("compoundData object", 
           sprintf("# Rows of data: %d", nrow(compObj$e_data)),
           sprintf("# Samples: %d", nrow(compObj$f_data)),
           sprintf("# Compounds: %d", length(unique(compObj$e_meta[, getCompoundColName(compObj)]))),
           sprintf("Database: %s", attr(compObj, "DB")))
  
  res <- c(res, .common_print_data(compObj))
  
  cat(res, sep="\n")
  invisible(compObj)
}

print.reactionData <- function(rxnObj) {
  if (!inherits(rxnObj, "reactionData")) stop("Not a 'reactionData' object")
  if (is.null(rxnObj) | all(is.na(rxnObj))) {
    print(rxnObj)
    invisible(rxnObj)
  }
  res <- c("reactionData object", 
           sprintf("# Reactions: %d", nrow(rxnObj$e_data)),
           sprintf("# Samples: %d", nrow(rxnObj$f_data)),
           sprintf("Database: %s", attr(rxnObj, "DB")))

  res <- c(res, .common_print_data(rxnObj))
  
  cat(res, sep="\n")
  invisible(rxnObj)
}

#' @export
print.moduleData <- function(moduleObj) {
  if (!inherits(moduleObj, "moduleData")) stop("Not a 'moduleData' object")
  if (is.null(moduleObj) | all(is.na(moduleObj))) {
    print(moduleObj)
    invisible(moduleObj)
  }
  res <- c("moduleData object", 
           sprintf("# Reactions: %d", nrow(moduleObj$e_data)),
           sprintf("# Samples: %d", nrow(moduleObj$f_data)),
           sprintf("Database: %s", attr(moduleObj, "DB")))
  
  res <- c(res, .common_print_data(moduleObj))
  
  cat(res, sep="\n")
  invisible(moduleObj)
}

#' @export
print.groupSummary <- function(ftmsObj) {
  if (!inherits(ftmsObj, "groupSummary")) stop("Not a groupSummary object")
  if (is.null(ftmsObj) | all(is.na(ftmsObj))) {
    print(ftmsObj)
    invisible(ftmsObj)
  }
  groupname <- unique(getGroupDF(ftmsObj)$Group)
  res <- c(sprintf("groupSummary object derived from %s", class(ftmsObj)[2]), 
           sprintf("# Rows: %d", nrow(ftmsObj$e_data)),
           sprintf("Summary columns: [%s]", paste(ftmsObj$f_data[, getFDataColName(ftmsObj)], collapse = ", ")),
           sprintf("Groups: %s", paste(groupname, collapse=", ")),
           sprintf("# Samples in group: %d", nrow(getGroupDF(ftmsObj))))
  
  res <- c(res, .common_print_data(ftmsObj))
  
  # remove Group info line:
  ind <- grepl("^Group info: ", res)
  res <- res[!ind]
  
  cat(res, sep="\n")
  invisible(ftmsObj)
}
