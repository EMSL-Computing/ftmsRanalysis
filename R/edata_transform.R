#' Apply a Transformation to the Data
#'
#' This function applies a transformation to the e_data element of icrData
#'
#' @param icrData an object of the class 'peakIcrData' or 'compoundIcrData' , usually created by \code{\link{as.icrData}}
#' @param data_scale a character string indicating the type of transformation to be applied to the data. Valid values are: 'log2', 'log', 'log10', 'pres', or 'abundance'. A value of 'abundance' indicates the data has previously undergone one of the log transformations and should be transformed back to raw values with no transformation applied. A value of 'pres' indicates the data should be transformed to presence/absence data.
#' 
#' @details This function is intended to be used before analysis of the data begins. Data are typically analyzed on a log or presence/absence scale.
#'
#' @return data object of the same class as icrData
#'
#' @author Allison Thompson
#'
#' @export
edata_transform <- function(icrData, data_scale){
  
  ## some initial checks ##
  
  # check that icrData is of appropriate class #
  if(!inherits(icrData, c("peakIcrData","compoundIcrData"))) stop("icrData must be of class 'peakIcrData' or 'compoundIcrData'")
  
  # check that data_scale is one of the acceptable options #
  if(!(data_scale %in% c('log2', 'log10', 'log', 'pres', 'abundance'))) stop(paste(data_scale, " is not a valid option for 'data_scale'.", sep=""))
  
  # set data scale #
  if(is.null(attr(icrData, "data_info")$data_scale)){
    attr(icrData, "data_info")$data_scale = "abundance"
  }
  
  # if desired scale is log scale, check to make sure data is not already on log scale #
  if(attr(icrData, "data_info")$data_scale == "log2" & data_scale == "log2"){
    stop("Data is already on log2 scale.")
  }
  if(attr(icrData, "data_info")$data_scale == "log10" & data_scale == "log10"){
    stop("Data is already on log10 scale.")
  }
  if(attr(icrData, "data_info")$data_scale == "log" & data_scale == "log"){
    stop("Data is already on (natural) log scale.")
  }
  
  # if desired scale is abundance, check to make sure data is not already on abundance scale #
  if(data_scale=="abundance" & attr(icrData, "data_info")$data_scale == "abundance"){
    stop("Data is already on abundance scale.")
  }
  
  # check to make sure data isn't already transformed #
  if(data_scale != "abundance" & attr(icrData, "data_info")$data_scale != "abundance" & !is.null(attr(icrData, "data_info")$data_scale)){
    warning(paste("Data has already been transformed. Data is on the ", attr(icrData, "data_info")$data_scale, " scale. Changing to ", data_scale, " scale.",sep=""))
  }
  
  ## end initial checks ##
  
  edata_id = attr(icrData, "cnames")$edata_cname
  
  edata <- icrData$e_data
  feature_names <- edata[,which(names(edata)==edata_id)]
  
  # pull off the identifier column #
  edata <- edata[, -which(colnames(edata)==edata_id)]
  
  # apply the transformation #
  ## initial transformation
  if(attr(icrData, "data_info")$data_scale == "abundance"){
    if(data_scale == "log"){
      edata_new <- log(edata)
    }else if(data_scale == "log2"){
        edata_new <- log2(edata)
    }else if(data_scale == "log10"){
        edata_new <- log10(edata)
    }else if(data_scale == "pres"){
      edata_new <- apply(edata, 1:2, function(x) ifelse(!is.na(x) & x > 0, 1, 0))
    }
  ## prevoiusly natural log transformed
  }else if(attr(icrData, "data_info")$data_scale == "log"){
    if(data_scale == "abundance"){
      edata_new <- exp(edata)
    }else if(data_scale == "log2"){
      edata_new <- log2(exp(edata))
    }else if(data_scale == "log10"){
      edata_new <- log10(exp(edata))
    }else if(data_scale == "pres"){
      edata_new <- apply(edata, 1:2, function(x) ifelse(!is.na(x) & x > 0, 1, 0))
    }
  ## previously log2 transformed
  }else if(attr(icrData, "data_info")$data_scale == "log2"){
    if(data_scale == "abundance"){
      edata_new <- 2^(edata)
    }else if(data_scale == "log"){
      edata_new <- log(2^(edata))
    }else if(data_scale == "log10"){
      edata_new <- log10(2^(edata))
    }else if(data_scale == "pres"){
      edata_new <- apply(edata, 1:2, function(x) ifelse(!is.na(x) & x > 0, 1, 0))
    }
  ## previously log10 transformed
  }else if(attr(icrData, "data_info")$data_scale == "log10"){
    if(data_scale == "abundance"){
      edata_new <- 10^(edata)
    }else if(data_scale == "log"){
      edata_new <- log(10^(edata))
    }else if(data_scale == "log2"){
      edata_new <- log2(10^(edata))
    }else if(data_scale == "pres"){
      edata_new <- apply(edata, 1:2, function(x) ifelse(!is.na(x) & x > 0, 1, 0))
    }
  }else if(attr(icrData, "data_info")$data_scale == "pres"){
    stop("Cannot back-transform from presence-absence data.")
  }
  
  # add the identifier column back #
  edata_new <- data.frame(edata_id=feature_names, edata_new)
  colnames(edata_new)[1] <- edata_id

  # create object with new e_data #
  updated_data <- icrData
  updated_data$e_data <- edata_new
  
  return(updated_data)
   
}