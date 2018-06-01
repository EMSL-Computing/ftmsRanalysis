# Print methods for peakIcrData, compoundIcrData, reactionIcrData and moduleIcrData

#' @export
print.peakIcrData <- function(picr) {
  if (!inherits(picr, "peakIcrData")) stop("Not a peakIcrData object")
  if (is.null(picr) | all(is.na(picr))) {
    print(picr)
    invisible(picr)
  }
  res <- c("peakIcrData object", 
           sprintf("# Peaks: %d", nrow(picr$e_data)),
           sprintf("# Samples: %d", nrow(picr$f_data)))
  
  res <- c(res, .common_print_data(picr))
    
  cat(res, sep="\n")
  invisible(picr)
}

# internal only method for constructing print info for common parts of icrData objects
.common_print_data <- function(icrObj) {
  if (!inherits(icrObj, "icrData")) stop("Not a icrData object")
  
  res <- NULL
  if (!is.null(icrObj$e_meta)) {
    res <- c(res, sprintf("Meta data columns: [%s]", paste(colnames(icrObj$e_meta), collapse = ", ")))
  }
  
  if (!is.null(getGroupDF(icrObj))) {
    group.df <- getGroupDF(icrObj)
    group.str <- sprintf("Group info: main effects=[%s]", paste(attr(group.df, "main_effects"), collapse=", "))
    if (!is.null(attr(group.df, "covariates"))) {
      group.str <- sprintf("%s, covariates=[%s]", group.str, paste(attr(group.df, "covariates"), collapse=", "))
    }
    res <- c(res, group.str)
  }

  if (!is.null(attr(icrObj, "filters"))) {
    res <- c(res, unlist(lapply(attr(icrObj, "filters"), function(x) return(x$report_text))))
  }
  
  return(res)
}

#' @export
print.compoundIcrData <- function(cicr) {
  if (!inherits(cicr, "compoundIcrData")) stop("Not a compoundIcrData object")
  if (is.null(cicr) | all(is.na(cicr))) {
    print(cicr)
    invisible(cicr)
  }
  res <- c("compoundIcrData object", 
           sprintf("# Rows of data: %d", nrow(cicr$e_data)),
           sprintf("# Samples: %d", nrow(cicr$f_data)),
           sprintf("# Compounds: %d", length(unique(cicr$e_meta[, getCompoundColName(cicr)]))),
           sprintf("Database: %s", attr(cicr, "DB")))
  
  res <- c(res, .common_print_data(cicr))
  
  cat(res, sep="\n")
  invisible(cicr)
}

print.reactionIcrData <- function(ricr) {
  if (!inherits(ricr, "reactionIcrData")) stop("Not a reactionIcrData object")
  if (is.null(ricr) | all(is.na(ricr))) {
    print(ricr)
    invisible(ricr)
  }
  res <- c("reactionIcrData object", 
           sprintf("# Reactions: %d", nrow(ricr$e_data)),
           sprintf("# Samples: %d", nrow(ricr$f_data)),
           sprintf("Database: %s", attr(ricr, "DB")))

  res <- c(res, .common_print_data(ricr))
  
  cat(res, sep="\n")
  invisible(ricr)
}

#' @export
print.moduleIcrData <- function(micr) {
  if (!inherits(micr, "moduleIcrData")) stop("Not a moduleIcrData object")
  if (is.null(micr) | all(is.na(micr))) {
    print(micr)
    invisible(micr)
  }
  res <- c("moduleIcrData object", 
           sprintf("# Reactions: %d", nrow(micr$e_data)),
           sprintf("# Samples: %d", nrow(micr$f_data)),
           sprintf("Database: %s", attr(micr, "DB")))
  
  res <- c(res, .common_print_data(micr))
  
  cat(res, sep="\n")
  invisible(micr)
}

#' @export
print.groupSummary <- function(icrData) {
  if (!inherits(icrData, "groupSummary")) stop("Not a groupSummary object")
  if (is.null(icrData) | all(is.na(icrData))) {
    print(icrData)
    invisible(icrData)
  }
  groupname <- unique(getGroupDF(icrData)$Group)
  res <- c(sprintf("groupSummary object derived from %s", class(icrData)[2]), 
           sprintf("# Rows: %d", nrow(icrData$e_data)),
           sprintf("Summary columns: [%s]", paste(icrData$f_data[, getFDataColName(icrData)], collapse = ", ")),
           sprintf("Groups: %s", paste(groupname, collapse=", ")),
           sprintf("# Samples in group: %d", nrow(getGroupDF(icrData))))
  
  res <- c(res, .common_print_data(icrData))
  
  # remove Group info line:
  ind <- grepl("^Group info: ", res)
  res <- res[!ind]
  
  cat(res, sep="\n")
  invisible(icrData)
}
