#' Apply a S3 filter  object to an icrData S3 object
#'
#' This function takes a filter object of class 'moleculeFilt' and applies the filter to a dataset of class \code{icrData}
#'
#' @param filter_object an object of the class 'moleculeFilt' created by  \code{\link{molecule_filter}}
#' @param icrData an object of the class \code{icrData} usually created by \code{\link{as.icrData}}
#' @param ... further arguments
#'
#' @return An object of the class \code{icrData} with specified molecules filtered out of the appropriate datasets.
#'
#' @details Various further arguments can be specified depending on the class of the \code{filter_object} being applied.
#' For a \code{filter_object} of type 'moleculeFilt', resulting from calling \code{\link{molecule_filter}}:
#' \tabular{ll}{
#' \code{min_num} \tab an integer value specifying the minimum number of times each biomolecule must be observed across all samples in order to retain the biomolecule. Default value is 2. \cr
#' }
#' 
#' For a \code{filter_object} of type 'massFilt', resulting from calling \code{\link{mass_filter}}:
#' \tabular{ll}{
#' \code{min_mass} \tab an numeric value greater than 0, specifying the minimum mass a peak should have in order to retain the peak. Default value is 200. \cr
#' \tab \cr
#' \code{max_mass} \tab an numeric value greater than \code{min_mass}, specifying the maximum mass a peak should have in order to retain the peak. Default value is 900. \cr
#' }
#' 
#' For a \code{filter_object} of type 'formulaFilt', resulting from calling \code{\link{formula_filter}}:
#' \tabular{ll}{
#' \code{remove} \tab a character string specifying which set of peaks to filter. Valid options are "NoFormula" and "Formula", defaults to "NoFormula". \cr
#' }
#'
#' @seealso \code{\link{molecule_filter}}, \code{\link{mass_filter}}, \code{\link{formula_filter}}
#'
#' @author Lisa Bramer
#'
#' @export
applyFilt <- function(filter_object, icrData, ...){
  
  # check that icrData is of appropriate class #
  if(!inherits(icrData, "icrData")) stop("icrData must be of class 'icrData'")
  
  # check that filter_object is of an appropriate class #
  # if(!inherits(filter_object, "moleculeFilt") & !inherits(filter_object, "massFilt"))) stop("filter_object must be of class 'moleculeFilt' or 'massFilt' ")
  
  # pull column names from omicR_data attributes #
  samp_cname = getFDataColName(icrData)
  edata_cname = getEDataColName(icrData)
  
  UseMethod("applyFilt")
}

# function for moleculeFilt
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.moleculeFilt <- function(filter_object, icrData, min_num=2){
  
  # check to see whether a moleculeFilt has already been run on icrData #
  if("moleculeFilt" %in% names(attributes(icrData)$filters)){
    # get previous threshold #
    min_num_prev <- attributes(icrData)$filters$moleculeFilt$threshold
    
    stop(paste("A molecule filter has already been run on this dataset, using a 'min_num' of ", min_num_prev, ". See Details for more information about how to choose a threshold before applying the filter.", sep=""))
    
    
  }else{ # no previous moleculeFilt, so go ahead and run it like normal #
    
    
    # check that min_num is numeric and >=1 #
    if(!(class(min_num) %in% c("numeric","integer")) | min_num < 1) stop("min_num must be an integer greater than or equal to 1")
    # check that min_num is an integer #
    if(min_num %% 1 != 0) stop("min_num must be an integer greater than or equal to 1")
    # check that min_num is less than the number of samples #
    if(min_num > (ncol(icrData$e_data) + 1)) stop("min_num cannot be greater than the number of samples")
    # check that min_num is of length 1 #
    if(length(min_num) != 1) stop("min_num must be of length 1")
    
    edata_cname <- getEDataColName(icrData)

    num_obs <- filter_object$Num_Observations

    # get indices for which ones don't meet the min requirement #
    inds <- which(num_obs < min_num)
    
    if(length(inds) < 1){
      filter.edata <- NULL
    }
    
    else{
      filter.edata <- icrData$e_data[, which(names(icrData$e_data) == edata_cname)][inds]
    }
    
    # checking if filter specifies all of icrData$e_data
    if(all(icrData$e_data[,edata_cname] %in% filter.edata)) {stop("filter_object specifies all samples in icrData")}
    
    filter_object_new = list(edata_filt = filter.edata, emeta_filt = NULL, samples_filt = NULL)
    
    # call the function that does the filter application
    results_pieces <- icr_filter_worker(icrData = icrData, filter_object = filter_object_new)
    
    # return filtered data object #
    results <- icrData
    results$e_data <- results_pieces$new.edata
    results$f_data <- results_pieces$new.fdata
    results$e_meta <- results_pieces$new.emeta
    
    # set attributes for which filters were run
    attr(results, "filters")$moleculeFilt <- list(report_text = "", threshold = c(), filtered = c())
    attr(results, "filters")$moleculeFilt$report_text <- paste("A molecule filter was applied to the data, removing ", edata_cname, "s ", "that were present in fewer than ", min_num, " samples. A total of ", length(filter.edata), " ", edata_cname, "s ", "were filtered out of the dataset by this filter.", sep="")
    attr(results, "filters")$moleculeFilt$threshold <- min_num
    attr(results, "filters")$moleculeFilt$filtered <- filter.edata
    
  }
  
  return(results)
}


# function for massFilt
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.massFilt <- function(filter_object, icrData, min_mass = 200, max_mass = 900){
  
  # check to see whether a massFilt has already been run on icrData #
  if("massFilt" %in% names(attributes(icrData)$filters)){
    # get previous threshold #
    min_num_prev <- attributes(icrData)$filters$massFilt$threshold = c(min_mass, max_mass)
    
    stop(paste("A mass filter has already been run on this dataset, using a 'min_mass' and 'max_mass' of ", min_num_prev[1], "and ", min_num_prev[2], ".", sep=""))
    
    
  }else{ # no previous massFilt, so go ahead and run it like normal #
    
    
    # check that min_mass and max_mass are numeric and meet other constraints #
    if(!(class(min_mass) %in% c("numeric","integer")) | min_mass < 0) stop("min_mass must be must be a number greater than zero")
    if(!(class(max_mass) %in% c("numeric","integer")) | min_mass > max_mass) stop("max_mass must be must be a number greater than min_mass")

    # check that min_mass and max_mass are of length 1 #
    if(length(min_mass) != 1) stop("min_mass must be of length 1")
    if(length(max_mass) != 1) stop("max_mass must be of length 1")
    
    edata_cname <- getEDataColName(icrData)
    mass_cname = getMassColName(icrData)
    
    mass_info <- filter_object[,mass_cname]
    
    # get indices for which ones meet the requirement #
    inds <- which(mass_info <= max_mass & mass_info >= min_mass)
    
    if(length(inds) < 1) stop("Filtering using the specified minimum and maximum masses results in no peaks left in the data.")
    
    # sample identifiers to keep #
    edata_ids = filter_object[inds, edata_cname]

    temp_edata = icrData$e_data[which(icrData$e_data[,edata_cname] %in% edata_ids),]
    temp_emeta = icrData$e_meta[which(icrData$e_meta[,edata_cname] %in% edata_ids),]
    
    num_rmv = length(icrData$e_data[,edata_cname]) - length(inds)

    # set attributes for which filters were run
    attr(icrData, "filters")$massFilt <- list(report_text = "", threshold = c(), filtered = c())
    attr(icrData, "filters")$massFilt$report_text <- paste("A mass filter was applied to the data, removing ", edata_cname, "s ", "that had a mass less than ", min_mass, " or a mass greater than ", max_mass, ". A total of ", num_rmv, " ", edata_cname, "s ", "were filtered out of the dataset by this filter.", sep="")
    attr(icrData, "filters")$massFilt$threshold <- c(min_mass, max_mass)
    attr(icrData, "filters")$massFilt$filtered <- icrData$e_data[which(!icrData$e_data[,edata_cname] %in% edata_ids),edata_cname]
    
    icrData$e_data = temp_edata
    icrData$e_meta = temp_emeta
    results = icrData
  }
  
  return(results)
}


# function for moleculeFilt
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.formulaFilt <- function(filter_object, icrData, remove = 'NoFormula'){
  
  # check to see whether a formulaFilt has already been run on icrData #
  if("formulaFilt" %in% names(attributes(icrData)$filters)){
    # get previous threshold #
    min_num_prev <- attributes(icrData)$filters$formulaFilt$threshold
    
    stop(paste("A formula filter has already been run on this dataset, using a 'remove' argument of ", min_num_prev, ".", sep=""))
    
    
  }else{ # no previous formulaFilt, so go ahead and run it like normal #
    
    
    # check that remove is a valid argument #
    if(!(remove %in% c("NoFormula","Formula"))) stop("'remove' can only take values 'NoFormula' and 'Formula'.")
    
    edata_cname <- getEDataColName(icrData)
    
    form_assigned <- filter_object$Formula_Assigned
    
    # get indices for which ones don't meet the min requirement #
    if(remove == "NoFormula"){
      inds <- which(form_assigned==FALSE)
    }else{
      inds <- which(form_assigned==TRUE)
    }
    
    if(length(inds) < 1){
      filter.edata <- NULL
    }
    
    else{
      filter.edata <- icrData$e_data[, which(names(icrData$e_data) == edata_cname)][inds]
    }
    
    # checking if filter specifies all of icrData$e_data
    if(all(icrData$e_data[,edata_cname] %in% filter.edata)) {stop("filter_object specifies all samples in icrData")}
    
    filter_object_new = list(edata_filt = filter.edata, emeta_filt = NULL, samples_filt = NULL)
    
    # call the function that does the filter application
    results_pieces <- icr_filter_worker(icrData = icrData, filter_object = filter_object_new)
    
    # return filtered data object #
    results <- icrData
    results$e_data <- results_pieces$new.edata
    results$f_data <- results_pieces$new.fdata
    results$e_meta <- results_pieces$new.emeta
    
    # set attributes for which filters were run
    attr(results, "filters")$formulaFilt <- list(report_text = "", threshold = c(), filtered = c())
    attr(results, "filters")$formulaFilt$report_text <- paste("A formula filter was applied to the data, removing ", edata_cname, "s ", "that had ", remove, " assigned. A total of ", length(filter.edata), " ", edata_cname, "s ", "were filtered out of the dataset by this filter.", sep="")
    attr(results, "filters")$formulaFilt$threshold <- remove
    attr(results, "filters")$moleculeFilt$filtered <- filter.edata
    
  }
  
  return(results)
}

#' Remove items that need to be filtered out
#'
#' This function removes
#'
#' @param icrData an object of the class \code{icrData} usually created by \code{\link{as.icrData}}
#' @param filter_object a list created by the functions above
#' @return list
#' @author Lisa Bramer
#'
icr_filter_worker <- function(filter_object, icrData){

  # pull column names from icrData attributes #
  samp_cname = getFDataColName(icrData)
  edata_cname = getEDataColName(icrData)

  # pull group_DF attribute #
  group_DF = attr(icrData, "group_DF")
  
  # initialize the new omicsData parts #
  temp.edata <- icrData$e_data
  temp.fdata <- icrData$f_data
  temp.emeta <- icrData$e_meta
  
  #check if filter object contains remove arguments
  if(!is.null(filter_object$edata_filt) | !is.null(filter_object$samples_filt)){
    
    # remove any samples from f_data and e_data #
    if(!is.null(filter_object$samples_filt)){
      inds <- which(temp.fdata[, which(names(temp.fdata) == samp_cname)] %in% filter_object$samples_filt)
      temp.fdata <- temp.fdata[-inds, ]
      
      inds <- which(names(temp.edata) %in% filter_object$samples_filt)
      temp.edata <- temp.edata[ ,-inds]
    }
    
    # remove any edata molecules from e_data and e_meta #
    if(!is.null(filter_object$edata_filt)){
      inds <- which(temp.edata[ , which(names(temp.edata) == edata_cname)] %in% filter_object$edata_filt)
      temp.edata <- temp.edata[-inds, ]
      
      # also remove these from e_meta #
      
        inds <- which(temp.emeta[ , which(names(temp.emeta) == edata_cname)] %in% filter_object$edata_filt)
        temp.emeta <- temp.emeta[-inds, ]
    
    }
    

  }else{ # filter object contains keep arguments #
    
    # keep samples in f_data and e_data #
    if(!is.null(filter_object$samples_keep)){
      inds <- which(temp.fdata[, which(names(temp.fdata) == samp_cname)] %in% filter_object$samples_keep)
      temp.fdata <- temp.fdata[inds, ]
      
      inds <- c(which(names(temp.edata) == edata_cname), which(names(temp.edata) %in% filter_object$samples_keep))
      temp.edata <- temp.edata[ , inds]
    }
    
    # keep edata molecules in e_data #
    if(!is.null(filter_object$edata_keep)){
      inds <- which(temp.edata[ , which(names(temp.edata) == edata_cname)] %in% filter_object$edata_keep)
      temp.edata <- temp.edata[inds, ]
      
          
        inds <- which(temp.emeta[ , which(names(temp.emeta) == edata_cname)] %in% filter_object$edata_keep)
        temp.emeta <- temp.emeta[inds, ]
      
    }
    

  }
  
  # return the pieces needed to assemble a proData/pepData/lipidData/metabData object #
  output <- list(new.edata = temp.edata, new.fdata = temp.fdata, new.emeta = temp.emeta, edata_cname = edata_cname, samp_cname = samp_cname)
  
  return(output)  
  }
