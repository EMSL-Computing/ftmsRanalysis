#' Create a matrix of which pairwise comparisons to make
#' 
#' Construct a matrix of pairwise comparisons (pairs can be either samples or groups)
#' 
#' @param ftmsObj ftmsData object
#' @param comparisons dictates which pairwise comparisons to make. 'all' will create a matrix for all pairwise comparisons, 'control' will create a matrix for all comparisons against a specified control group, 'one-factor' will create a matrix of pairwise comparisons to be made where only one 'main_effect' changes between the two groups, or a list of specific comparisons to be made (e.g., list(c("Group1","Group2"),c("Group3","Group4"))) can be given.
#' @param control if wanting to only compare against a control, must specify which group or sample is the control
#' @return a matrix of which pairwise comparisons to make
comparisonMatrix <- function(ftmsObj, comparisons, control=NULL) {
  if (missing(ftmsObj)) stop("ftmsObj is missing")
  if (missing(comparisons)) stop("comparisons is missing")
  if (!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be of type ftmsData")
  
  groupDF <- attr(ftmsObj, "group_DF")
  if (is.null(groupDF)) {  ## this means each sample is its own group so construct a dummy groupDF
    
    if(all(tolower(comparisons) == "one-factor")){ stop("The 'one-factor' specification cannot be used without a group data frame. Please use 'group_designation' first.") }
    
    samp.names <- unique(ftmsObj$f_data[, getFDataColName(ftmsObj)])
    groupDF <- data.frame(Sample=samp.names, Group=samp.names)
    colnames(groupDF)[1] <- getFDataColName(ftmsObj)
  }
  
  if (!missing(control) & !is.null(control)) {
    if (!(control %in% groupDF$Group)) {
      stop(paste0("control column '", control, "' not found in the data"))
    }
  }
  
  # determine which pairwise comparisons to make
  if(is.list(comparisons)){
    if (!all(unlist(comparisons) %in% groupDF$Group)) {
      stop("not all groups specified in pairs parameter were found in the data")
    }
    
    pairs <- do.call(cbind, comparisons)
  } else if(tolower(comparisons) == "all"){
    pairs <- combn(as.character(unique(groupDF$Group)),2)
  }else if(tolower(comparisons) == "control"){
    pairs <- combn(as.character(unique(groupDF$Group)),2)
    if(ncol(pairs) > 1){
      if(length(unique(c(grep(control,pairs[1,]),grep(control,pairs[2,])))) > 1){
        pairs <- pairs[,unique(c(grep(control, pairs[1,]),grep(control, pairs[2,])))]
      }
      if(any(pairs[1,]==control)){
        pairs[,which(pairs[1,] == control)] <- apply(pairs[,which(pairs[1,] == control)], 2, rev)
      }
    }
  }else if(tolower(comparisons) == "one-factor"){
    pairs <- one_factor_change(ftmsObj, groupDF)
    pairs <- do.call(cbind, pairs)
  }else{
    stop("check that comparisons argument is either 'all', 'control', 'one-factor', or a list of specific comparisons")
  }
  
  return(pairs)
  
}

#' Create a list of which pairwise comparisons to make
#' 
#' Construct a list of pairwise comparisons, where only
#' one main effect changes between each group
#' 
#' @param ftmsObj ftmsData object
#' @param groupDF group data frame from ftmsData object
#' @return a list of which pairwise comparisons to make

one_factor_change <- function(ftmsObj, groupDF){
  
  # Get only uniuqe groups
  df <- groupDF[!duplicated(groupDF$Group),]
  
  # Remove Sample and Group columns
  if(ncol(df) > 2){
    df <- df[,-which(colnames(df) %in% c(getFDataColName(ftmsObj), "Group"))]
  }else{
    df <- as.data.frame(df[,-which(colnames(df) %in% getFDataColName(ftmsObj))])
    colnames(df) <- "Group"
  }
  
  # Make sure it's character, not factor
  df <- apply(df, 2, as.character)
  
  # Compare each row to every other row
  res <- lapply(c(1:(nrow(df)-1)), function(i){
    
    res <- lapply(c((i+1):nrow(df)), function(j){
      
      temp <- apply(as.data.frame(df[c(i,j),]), 2, function(k){
        
        k[1] == k[2]
        
      })
      
      data.frame(Grp1=paste(df[i,], collapse="_"), 
                 Grp2=paste(df[j,], collapse="_"), 
                 Compare = (sum(temp) == (ncol(df)-1)))
      
    })
    
    return(do.call(rbind, res))
    
  })
  
  res <- do.call(rbind, res)
  fin <- subset(res, Compare)
  
  ret <- lapply(c(1:nrow(fin)), function(x) c(as.character(fin$Grp1)[x], as.character(fin$Grp2)[x]))
  
  return(ret)
  
}