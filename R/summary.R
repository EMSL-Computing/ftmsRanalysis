#' Summary of icrData object
#'
#' @param icrData object of type icrData
#'
#' @return list object containing summary statistics
#' @export
summary.icrData <- function(icrData) {
  
  res <- list()
  samp_names <- as.character(unique(icrData$f_data[, getFDataColName(icrData)]))
  res$Samples <- length(samp_names)
  res$Molecules <- nrow(icrData$e_data)
  data_vals <- as.matrix(icrData$e_data[, samp_names])
  if (getDataScale(icrData) == "abundance" | getDataScale(icrData) == "pres") {
    nmissing <- sum(data_vals == 0 | is.na(data_vals))
  } else {
    nmissing <- sum(is.na(data_vals))
  }
  res$Percent_Missing <- nmissing/prod(dim(data_vals))*100
  
  #if group_DF attr is present 
  if(!is.null(attr(icrData, "group_DF"))){
    group_vec<- attr(icrData, "group_DF")$Group
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
  class(res) <- c("icrDataSummary", "list")
  return(res)
}

#' @export
print.icrDataSummary <- function(obj) {
  if(length(obj) == 3){
  cat(sprintf("Samples: %d\nMolecules: %d\nPercent Missing: %.3f%%\n", obj$Samples, obj$Molecules, obj$Percent_Missing))
  }
  if(length(obj) > 3){
  grp_info = paste(row.names(obj$Group_Sizes), rep(":",nrow(obj$Group_Sizes)), obj$Group_Sizes$N, c(rep("\n", (nrow(obj$Group_Sizes) -1)), ""))
  cat(sprintf("Samples: %d\nMolecules: %d\nPercent Missing: %.3f%%\n", obj$Samples, obj$Molecules, obj$Percent_Missing))  
  cat("Group Sizes:\n", grp_info)
  }
}  