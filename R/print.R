# Print methods for peakData, compoundData, reactionData and moduleData

#' @export
print.peakData <- function(x, ...) {
  if (!inherits(x, "peakData")) stop("Not a peakData object")
  if (is.null(x) | all(is.na(x))) {
    print(x)
    invisible(x)
  }
  res <- c("peakData object", 
           sprintf("# Peaks: %d", nrow(x$e_data)),
           sprintf("# Samples: %d", nrow(x$f_data)))
  
  res <- c(res, .common_print_data(x))
    
  cat(res, sep="\n")
  invisible(x)
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
print.compoundData <- function(x, ...) {
  if (!inherits(x, "compoundData")) stop("Not a compoundData object")
  if (is.null(x) | all(is.na(x))) {
    print(x)
    invisible(x)
  }
  res <- c("compoundData object", 
           sprintf("# Rows of data: %d", nrow(x$e_data)),
           sprintf("# Samples: %d", nrow(x$f_data)),
           sprintf("# Compounds: %d", length(unique(x$e_meta[, getCompoundColName(x)]))),
           sprintf("Database: %s", attr(x, "DB")))
  
  res <- c(res, .common_print_data(x))
  
  cat(res, sep="\n")
  invisible(x)
}

print.reactionData <- function(x, ...) {
  if (!inherits(x, "reactionData")) stop("Not a 'reactionData' object")
  if (is.null(x) | all(is.na(x))) {
    print(x)
    invisible(x)
  }
  res <- c("reactionData object", 
           sprintf("# Reactions: %d", nrow(x$e_data)),
           sprintf("# Samples: %d", nrow(x$f_data)),
           sprintf("Database: %s", attr(x, "DB")))

  res <- c(res, .common_print_data(x))
  
  cat(res, sep="\n")
  invisible(x)
}

#' @export
print.moduleData <- function(x, ...) {
  if (!inherits(x, "moduleData")) stop("Not a 'moduleData' object")
  if (is.null(x) | all(is.na(x))) {
    print(x)
    invisible(x)
  }
  res <- c("moduleData object", 
           sprintf("# Reactions: %d", nrow(x$e_data)),
           sprintf("# Samples: %d", nrow(x$f_data)),
           sprintf("Database: %s", attr(x, "DB")))
  
  res <- c(res, .common_print_data(x))
  
  cat(res, sep="\n")
  invisible(x)
}

#' @export
print.groupSummary <- function(x, ...) {
  if (!inherits(x, "groupSummary")) stop("Not a groupSummary object")
  if (is.null(x) | all(is.na(x))) {
    print(x)
    invisible(x)
  }
  groupname <- unique(getGroupDF(x)$Group)
  res <- c(sprintf("groupSummary object derived from %s", class(x)[2]), 
           sprintf("# Rows: %d", nrow(x$e_data)),
           sprintf("Summary columns: [%s]", paste(x$f_data[, getFDataColName(x)], collapse = ", ")),
           sprintf("Groups: %s", paste(groupname, collapse=", ")),
           sprintf("# Samples in group: %d", nrow(getGroupDF(x))))
  
  res <- c(res, .common_print_data(x))
  
  # remove Group info line:
  ind <- grepl("^Group info: ", res)
  res <- res[!ind]
  
  cat(res, sep="\n")
  invisible(x)
}
