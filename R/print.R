# Print methods for peakData, compoundData, reactionData and moduleData

#' @export
print.peakData <- function(picr) {
  if (!inherits(picr, "peakData")) stop("Not a peakData object")
  if (is.null(picr) | all(is.na(picr))) {
    print(picr)
    invisible(picr)
  }
  res <- c("peakData object", 
           sprintf("# Peaks: %d", nrow(picr$e_data)),
           sprintf("# Samples: %d", nrow(picr$f_data)))
  
  res <- c(res, .common_print_data(picr))
    
  cat(res, sep="\n")
  invisible(picr)
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
print.compoundData <- function(cicr) {
  if (!inherits(cicr, "compoundData")) stop("Not a compoundData object")
  if (is.null(cicr) | all(is.na(cicr))) {
    print(cicr)
    invisible(cicr)
  }
  res <- c("compoundData object", 
           sprintf("# Rows of data: %d", nrow(cicr$e_data)),
           sprintf("# Samples: %d", nrow(cicr$f_data)),
           sprintf("# Compounds: %d", length(unique(cicr$e_meta[, getCompoundColName(cicr)]))),
           sprintf("Database: %s", attr(cicr, "DB")))
  
  res <- c(res, .common_print_data(cicr))
  
  cat(res, sep="\n")
  invisible(cicr)
}

print.reactionData <- function(ricr) {
  if (!inherits(ricr, "reactionData")) stop("Not a 'reactionData' object")
  if (is.null(ricr) | all(is.na(ricr))) {
    print(ricr)
    invisible(ricr)
  }
  res <- c("reactionData object", 
           sprintf("# Reactions: %d", nrow(ricr$e_data)),
           sprintf("# Samples: %d", nrow(ricr$f_data)),
           sprintf("Database: %s", attr(ricr, "DB")))

  res <- c(res, .common_print_data(ricr))
  
  cat(res, sep="\n")
  invisible(ricr)
}

#' @export
print.moduleData <- function(micr) {
  if (!inherits(micr, "moduleData")) stop("Not a 'moduleData' object")
  if (is.null(micr) | all(is.na(micr))) {
    print(micr)
    invisible(micr)
  }
  res <- c("moduleData object", 
           sprintf("# Reactions: %d", nrow(micr$e_data)),
           sprintf("# Samples: %d", nrow(micr$f_data)),
           sprintf("Database: %s", attr(micr, "DB")))
  
  res <- c(res, .common_print_data(micr))
  
  cat(res, sep="\n")
  invisible(micr)
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
