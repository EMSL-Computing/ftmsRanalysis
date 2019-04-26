#' Summary of an ftmsData object
#'
#' @param object an object of type ftmsData
#' @param ... other arguments
#'
#' @return list object containing summary statistics
#' @export
summary.ftmsData <- function(object, ...) {
  
  res <- list()
  samp_names <- as.character(unique(object$f_data[, getFDataColName(object)]))
  res$Samples <- length(samp_names)
  res$Molecules <- nrow(object$e_data)
  data_vals <- as.matrix(object$e_data[, samp_names])
  if (getDataScale(object) == "abundance" | getDataScale(object) == "pres") {
    nmissing <- sum(data_vals == 0 | is.na(data_vals))
  } else {
    nmissing <- sum(is.na(data_vals))
  }
  res$Percent_Missing <- nmissing/prod(dim(data_vals))*100
  
  #if group_DF attr is present 
  if(!is.null(attr(object, "group_DF"))){
    group_vec<- attr(object, "group_DF")$Group
    levels<- unique(group_vec)
    counts <- vector(mode="numeric", length=length(levels))
    
    for(i in 1:length(levels)){
      counts[i]<- length(which(group_vec == levels[i]))
    }
    res2<- as.list(counts)
    names(res2)<- levels
    newres2 <- lapply(res2, function(x) ifelse(is.null(x), "NA", as.character(x)))
    
    newres2<- data.frame(unlist(newres2))
    names(newres2) = "N"
    res$Group_Sizes = newres2
  }
  class(res) <- c("ftmsDataSummary", "list")
  return(res)
}

#' @export
print.ftmsDataSummary <- function(x, ...) {
  if(length(x) == 3){
  cat(sprintf("Samples: %d\nMolecules: %d\nPercent Missing: %.3f%%\n", x$Samples, x$Molecules, x$Percent_Missing))
  }
  if(length(x) > 3){
  grp_info = paste(row.names(x$Group_Sizes), rep(":",nrow(x$Group_Sizes)), x$Group_Sizes$N, c(rep("\n", (nrow(x$Group_Sizes) -1)), ""))
  cat(sprintf("Samples: %d\nMolecules: %d\nPercent Missing: %.3f%%\n", x$Samples, x$Molecules, x$Percent_Missing))  
  cat("Group Sizes:\n", grp_info)
  }
}  