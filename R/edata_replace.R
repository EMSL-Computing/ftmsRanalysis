#' Replace Values Equal to x with y
#'
#' This function finds all values of x in the e_data element of icrData and replaces them with y
#'
#' @param icrData an object of the class 'peakIcrData', or 'compoundIcrData' 
#' @param x value to be replaced, usually numeric or NA
#' @param y replacment value, usually numeric or NA
#'
#' @details This function is often used to replace any 0 values in \code{e_data} with NA's.
#'
#' @return data object of the same class as icrData
#'
#' @author Lisa Bramer
#'
#' @export
edata_replace <- function(icrData, x, y){
  ## some initial checks ##
  
  # check that icrData is of appropriate class #
  if(!(inherits(icrData, "peakIcrData")) & !(inherits(icrData, "compoundIcrData")))  stop("icrData must be of class 'peakIcrData' or 'compoundIcrData'")
  
  edata_id = getEDataColName(icrData)
  
  edata <- icrData$e_data
  feature_names <- edata[,edata_id]
  
  # pull off the identifier column #
  edata <- edata[, -which(colnames(edata)==edata_id)]
  
  e_meta <- icrData$e_meta
  
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
  
  icrData$e_data = edata_new
  
  message(paste(num_replaced, "instances of", x, "have been replaced with", y, sep=" "))
  return(icrData)
  
}
