#' Replace Values Equal to x with y
#'
#' This function finds all values of x in the e_data element of ftmsObj and replaces them with y
#'
#' @param ftmsObj an object of the class 'peakData', or 'compoundData' 
#' @param x value to be replaced, usually numeric or NA
#' @param y replacment value, usually numeric or NA
#'
#' @details This function is often used to replace any 0 values in \code{e_data} with NA's.
#'
#' @return data object of the same class as ftmsObj
#'
#' @author Lisa Bramer
#'
#' @export
edata_replace <- function(ftmsObj, x, y){
  ## some initial checks ##
  
  # check that ftmsObj is of appropriate class #
  if(!(inherits(ftmsObj, "peakData")) & !(inherits(ftmsObj, "compoundData")))  stop("ftmsObj must be of class 'peakData' or 'compoundData'")
  
  edata_id = getEDataColName(ftmsObj)
  
  edata <- ftmsObj$e_data
  feature_names <- edata[,edata_id]
  
  # pull off the identifier column #
  edata <- edata[, -which(colnames(edata)==edata_id)]
  
  e_meta <- ftmsObj$e_meta
  
  # get count of the number of values replaced #
  if(is.na(x)){
    truefalse <- is.na(edata)
  }else{
    truefalse <- edata==x
  }
  num_replaced <- sum(truefalse, na.rm=TRUE)
  
  # apply vector_replace to the cols of edata (identifier column has been pulled off) #
  edata_new <- apply(edata, 2, vector_replace, x=x, y=y)
  
  # add the identifier column back #
  edata_new <- data.frame(edata_id=feature_names, edata_new)
  colnames(edata_new)[which(colnames(edata_new) == "edata_id")] <- edata_id
  
  ftmsObj$e_data = edata_new
  
  message(paste(num_replaced, "instances of", x, "have been replaced with", y, sep=" "))
  return(ftmsObj)
  
}



#' Replace x with y for a single vector
#'
#' @param one_vector numeric vector
#' @param x value to be replaced
#' @param y replacement value
#'
#' @return numeric vector
#'
#' @author Kelly Stratton
#'
vector_replace <- function(one_vector, x, y){
  # find indices where the value is x #
  if(is.na(x)){
    inds <- is.na(one_vector)
  }else{
    inds <- which(one_vector==x)
  }
  
  
  # initialize a new vector, which will be returned after we replace x with y #
  new_vector <- one_vector
  
  # replace x with y #
  new_vector[inds] <- y
  
  return(new_vector)
}
