#' Apply a S3 filter  object to an ftmsData S3 object
#'
#' This function takes a filter object of class 'moleculeFilt' and applies the filter to a dataset of class \code{ftmsData}
#'
#' @param filter_object an object of the class 'moleculeFilt' created by  \code{\link{molecule_filter}}
#' @param ftmsObj an object of the class \code{ftmsData} usually created by \code{\link{as.peakData}}
#' @param ... further arguments as described below based on the class of \code{filter_object}
#'
#' @return An object of the class \code{ftmsData} with specified molecules filtered out of the appropriate datasets.
#' 
#' @seealso \code{\link{molecule_filter}}, \code{\link{mass_filter}}, \code{\link{formula_filter}}, \code{\link{emeta_filter}}
#'
#' @author Lisa Bramer
#'
#' @export
applyFilt <- function(filter_object, ftmsObj, ...){
  
  # check that ftmsObj is of appropriate class #
  if(!inherits(ftmsObj, "ftmsData")) stop("ftmsObj must be of class 'ftmsData'")
  
  # check that filter_object is of an appropriate class #
  # if(!inherits(filter_object, "moleculeFilt") & !inherits(filter_object, "massFilt"))) stop("filter_object must be of class 'moleculeFilt' or 'massFilt' ")
  
  # pull column names from ftmsData attributes #
  samp_cname = getFDataColName(ftmsObj)
  edata_cname = getEDataColName(ftmsObj)
  
  UseMethod("applyFilt")
}

# function for moleculeFilt
#' @param min_num [\code{moleculeFilt} object] an integer value specifying the minimum number of times each biomolecule must be observed across all samples in order to retain the biomolecule. Default value is 2. \cr
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.moleculeFilt <- function(filter_object, ftmsObj, min_num=2, ...){
  
  # check to see whether a moleculeFilt has already been run on ftmsObj #
  if("moleculeFilt" %in% names(attributes(ftmsObj)$filters)){
    # get previous threshold #
    min_num_prev <- attributes(ftmsObj)$filters$moleculeFilt$threshold
    
    stop(paste("A molecule filter has already been run on this dataset, using a 'min_num' of ", min_num_prev, ". See Details for more information about how to choose a threshold before applying the filter.", sep=""))
    
    
  }else{ # no previous moleculeFilt, so go ahead and run it like normal #
    
    
    # check that min_num is numeric and >=1 #
    if(!(class(min_num) %in% c("numeric","integer")) | min_num < 1) stop("min_num must be an integer greater than or equal to 1")
    # check that min_num is an integer #
    if(min_num %% 1 != 0) stop("min_num must be an integer greater than or equal to 1")
    # check that min_num is less than the number of samples #
    if(min_num > (ncol(ftmsObj$e_data) + 1)) stop("min_num cannot be greater than the number of samples")
    # check that min_num is of length 1 #
    if(length(min_num) != 1) stop("min_num must be of length 1")
    
    edata_cname <- getEDataColName(ftmsObj)

    num_obs <- filter_object$Num_Observations

    # get indices for which ones don't meet the min requirement #
    inds <- which(num_obs < min_num)
    
    if(length(inds) < 1){
      filter.edata <- NULL
    }
    
    else{
      filter.edata <- ftmsObj$e_data[, which(names(ftmsObj$e_data) == edata_cname)][inds]
    }
    
    # checking if filter specifies all of ftmsObj$e_data
    if(all(ftmsObj$e_data[,edata_cname] %in% filter.edata)) {stop("filter_object specifies all samples in ftmsObj")}
    
    filter_object_new = list(edata_filt = filter.edata, emeta_filt = NULL, samples_filt = NULL)
    
    # call the function that does the filter application
    results_pieces <- filter_worker(ftmsObj = ftmsObj, filter_object = filter_object_new)
    
    # return filtered data object #
    results <- ftmsObj
    results$e_data <- results_pieces$new.edata
    results$f_data <- results_pieces$new.fdata
    results$e_meta <- results_pieces$new.emeta
    
    # set attributes for which filters were run
    attr(results, "filters")$moleculeFilt <- list(report_text = "", threshold = c(), filtered = c())
    attr(results, "filters")$moleculeFilt$report_text <- paste("A molecule filter was applied to the data, removing ", makePlural(edata_cname), " that were present in fewer than ", min_num, " samples. A total of ", length(filter.edata), " ", makePlural(edata_cname), " were filtered out of the dataset by this filter.", sep="")
    attr(results, "filters")$moleculeFilt$threshold <- min_num
    attr(results, "filters")$moleculeFilt$filtered <- filter.edata
    
  }
  
  return(results)
}


# function for massFilt
#' @param min_mass [\code{massFilt} object] a numeric value greater than 0, specifying the minimum mass a peak should have in order to retain the peak. Default value is 200. \cr
#' @param max_mass [\code{massFilt} object] a numeric value greater than \code{min_mass}, specifying the maximum mass a peak should have in order to retain the peak. Default value is 900. \cr
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.massFilt <- function(filter_object, ftmsObj, min_mass = 200, max_mass = 900, ...){
  
  # check to see whether a massFilt has already been run on ftmsObj #
  if("massFilt" %in% names(attributes(ftmsObj)$filters)){
    # get previous threshold #
    min_num_prev <- attributes(ftmsObj)$filters$massFilt$threshold
    
    stop(paste("A mass filter has already been run on this dataset, using a 'min_mass' and 'max_mass' of ", min_num_prev[1], "and ", min_num_prev[2], ".", sep=""))
    
    
  }else{ # no previous massFilt, so go ahead and run it like normal #
    
    
    # check that min_mass and max_mass are numeric and meet other constraints #
    if(!(class(min_mass) %in% c("numeric","integer")) | min_mass < 0) stop("min_mass must be must be a number greater than zero")
    if(!(class(max_mass) %in% c("numeric","integer")) | min_mass > max_mass) stop("max_mass must be must be a number greater than min_mass")

    # check that min_mass and max_mass are of length 1 #
    if(length(min_mass) != 1) stop("min_mass must be of length 1")
    if(length(max_mass) != 1) stop("max_mass must be of length 1")
    
    edata_cname <- getEDataColName(ftmsObj)
    mass_cname = getMassColName(ftmsObj)
    
    mass_info <- filter_object[,mass_cname]
    
    # get indices for which ones meet the requirement #
    inds <- which(mass_info <= max_mass & mass_info >= min_mass)
    
    if(length(inds) < 1) stop("Filtering using the specified minimum and maximum masses results in no peaks left in the data.")
    
    # sample identifiers to keep #
    edata_ids = filter_object[inds, edata_cname]

    temp_edata = ftmsObj$e_data[which(ftmsObj$e_data[,edata_cname] %in% edata_ids),]
    temp_emeta = ftmsObj$e_meta[which(ftmsObj$e_meta[,edata_cname] %in% edata_ids),]
    
    num_rmv = length(ftmsObj$e_data[,edata_cname]) - length(inds)

    # set attributes for which filters were run
    attr(ftmsObj, "filters")$massFilt <- list(report_text = "", threshold = c(), filtered = c())
    attr(ftmsObj, "filters")$massFilt$report_text <- paste("A mass filter was applied to the data, removing ", makePlural(edata_cname), " that had a mass less than ", min_mass, " or a mass greater than ", max_mass, ". A total of ", num_rmv, " ", makePlural(edata_cname), " were filtered out of the dataset by this filter.", sep="")
    attr(ftmsObj, "filters")$massFilt$threshold <- c(min_mass, max_mass)
    attr(ftmsObj, "filters")$massFilt$filtered <- ftmsObj$e_data[which(!ftmsObj$e_data[,edata_cname] %in% edata_ids),edata_cname]
    
    ftmsObj$e_data = temp_edata
    ftmsObj$e_meta = temp_emeta
    results = ftmsObj
  }
  
  return(results)
}


# function for formulaFilt
#' @param remove [\code{formulaFilt} object] a character string specifying which set of peaks to filter. Valid options are "NoFormula" and "Formula", defaults to "NoFormula". \cr
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.formulaFilt <- function(filter_object, ftmsObj, remove = 'NoFormula', ...){
  
  # check to see whether a formulaFilt has already been run on ftmsObj #
  if("formulaFilt" %in% names(attributes(ftmsObj)$filters)){
    # get previous threshold #
    min_num_prev <- attributes(ftmsObj)$filters$formulaFilt$threshold
    
    stop(paste("A formula filter has already been run on this dataset, using a 'remove' argument of ", min_num_prev, ".", sep=""))
    
    
  }else{ # no previous formulaFilt, so go ahead and run it like normal #
    
    
    # check that remove is a valid argument #
    if(!(remove %in% c("NoFormula","Formula"))) stop("'remove' can only take values 'NoFormula' and 'Formula'.")
    
    edata_cname <- getEDataColName(ftmsObj)
    
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
      filter.edata <- ftmsObj$e_data[, which(names(ftmsObj$e_data) == edata_cname)][inds]
    }
    
    # checking if filter specifies all of ftmsObj$e_data
    if(all(ftmsObj$e_data[,edata_cname] %in% filter.edata)) {stop("filter_object specifies all samples in ftmsObj")}
    
    filter_object_new = list(edata_filt = filter.edata, emeta_filt = NULL, samples_filt = NULL)
    
    # call the function that does the filter application
    results_pieces <- filter_worker(ftmsObj = ftmsObj, filter_object = filter_object_new)
    
    # return filtered data object #
    results <- ftmsObj
    results$e_data <- results_pieces$new.edata
    results$f_data <- results_pieces$new.fdata
    results$e_meta <- results_pieces$new.emeta
    
    # set attributes for which filters were run
    attr(results, "filters")$formulaFilt <- list(report_text = "", threshold = c(), filtered = c())
    attr(results, "filters")$formulaFilt$report_text <- paste("A formula filter was applied to the data, removing ", makePlural(edata_cname), " that had ", remove, " assigned. A total of ", length(filter.edata), " ", makePlural(edata_cname), " were filtered out of the dataset by this filter.", sep="")
    attr(results, "filters")$formulaFilt$threshold <- remove
    attr(results, "filters")$formulaFilt$filtered <- filter.edata
    
  }
  
  return(results)
}

# function for emetaFilt
#' @param min_val [\code{emetaFilt} object] a numeric value specifying the minimum value (inclusive) that a peak should have for the specified 'e_meta' column. \cr
#' @param max_val [\code{emetaFilt} object] a numeric value specifying the maximum value (inclusive) that a peak should have for the specified 'e_meta' column. \cr
#' @param cats [\code{emetaFilt} object] a vector of character values specifying the level(s) of the specified 'e_meta' column which should be retained. \cr
#' @param na.rm [\code{emetaFilt} object] logical value specifying if peaks with NA values for the specified 'e_meta' column should be removed. Default value is TRUE. \cr
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.emetaFilt <- function(filter_object, ftmsObj, min_val = NULL, max_val = NULL, cats = NULL, na.rm = TRUE, ...){
  
    # determine how many filters have already been implemented on the dataset #
    num_filts = length(attributes(ftmsObj)$filters)
    
    # create filter name #
    filt_name = paste("emetaFilt", attr(filter_object, "cname"), sep = "_")
    
    # check to see whether a formulaFilt has already been run on ftmsObj #
    if(filt_name %in% names(attributes(ftmsObj)$filters)){
      
      stop(paste("An emeta_filter using the variable '", attr(filter_object, "cname"), "' has already been run on this dataset.", sep=""))
  
      }else{ # if not go ahead an implement filter #
      
   
    # get variable type #
    var_type = attr(filter_object, "type")
    
    # set some defaults for quantitative variable #
    if(var_type == "quantitative"){
      # check that one of min_val and max_val is non NULL #
      if(is.null(min_val) & is.null(max_val)) stop("At least one of 'min_val' and 'max_val' must be provided to filter the data.")
      if(is.null(min_val)){min_val = min(filter_object[,2], na.rm = TRUE)}
      if(is.null(max_val)){max_val = max(filter_object[,2], na.rm = TRUE)}
      
      # check that min_val and max_val are numeric #
      if(!(class(min_val) %in% c("numeric","integer")) | !(class(max_val) %in% c("numeric","integer"))) stop("min_val and max_val must be numeric")
      
    }
    if(var_type == "categorical"){
      # check that levels are specified #
      if(is.null(cats)) stop("Levels of the categorical variable to retain must be specified using the 'cats' argument.")
    }
    

    edata_cname <- getEDataColName(ftmsObj)
    filter_object[edata_cname] <- as.character(filter_object[,edata_cname])
    
    # implement filter #
    if(var_type == "quantitative"){
      kp_masses = filter_object[which(filter_object$emeta_value >= min_val & filter_object$emeta_value <= max_val), edata_cname]
      if(na.rm == FALSE){
        kp_masses = c(kp_masses, filter_object[which(is.na(filter_object$emeta_value)), edata_cname])
      }
      if(length(kp_masses) == 0) stop("Current min_val and max_val specifications lead to all data being filtered.")
      
      rmv_masses = setdiff(filter_object[, edata_cname], kp_masses)
    }
    
    if(var_type == "categorical"){
      kp_masses = filter_object[which(filter_object$emeta_value %in% cats), edata_cname]
      if(na.rm == FALSE){
        kp_masses = c(kp_masses, filter_object[which(is.na(filter_object$emeta_value)), edata_cname])
      }
      if(length(kp_masses) == 0) stop("Current min_val and max_val specifications lead to all data being filtered.")
      
      rmv_masses = setdiff(filter_object[, edata_cname], kp_masses)
    }
    
    if(rlang::is_empty(rmv_masses)){rmv_masses <- NULL}
    filter_object_new = list(edata_filt = rmv_masses, emeta_filt = NULL, samples_filt = NULL)
    
    # call the function that does the filter application
    results_pieces <- filter_worker(ftmsObj = ftmsObj, filter_object = filter_object_new)
    
    # return filtered data object #
    results <- ftmsObj
    results$e_data <- results_pieces$new.edata
    results$f_data <- results_pieces$new.fdata
    results$e_meta <- results_pieces$new.emeta
    
    # set attributes for which filters were run #
  
    
    attr(results, "filters")[[num_filts + 1]] <- list(report_text = "", variable = c(), threshold = c(), filtered = c())
    names(attr(results, "filters")) = c(names(attr(results, "filters"))[1:num_filts], filt_name)
    if(var_type == "categorical"){
      attr(results, "filters")[[num_filts + 1]]$report_text <- paste("An e_meta filter was applied to the data, removing ", makePlural(edata_cname), " that had a ", attr(filter_object, "cname"), " value which was not in the following categories: ", cats, ".", sep="")
      attr(results, "filters")[[num_filts + 1]]$threshold <- cats
    }
    if(var_type == "quantitative"){
      attr(results, "filters")[[num_filts + 1]]$report_text <- paste("An e_meta filter was applied to the data, removing ", makePlural(edata_cname), " that had a ", attr(filter_object, "cname"), " value which was less than ", min_val, " or greater than ", max_val, ".", sep="")
      attr(results, "filters")[[num_filts + 1]]$threshold <- c(min_val, max_val)
    }
    attr(results, "filters")[[num_filts + 1]]$variable = attr(filter_object, "cname")
    
    attr(results, "filters")[[num_filts + 1]]$filtered <- rmv_masses
    
    attr(results, "filters")[[num_filts + 1]]$na.rm <- na.rm
    
  
    return(results)
  }  
}


#' Remove items that need to be filtered out
#'
#' This function removes
#'
#' @param ftmsObj an object of the class \code{ftmsData} usually created by \code{\link{as.peakData}}
#' @param filter_object a list created by the functions above
#' @return list
#' @author Lisa Bramer
#'
filter_worker <- function(filter_object, ftmsObj){

  # pull column names from ftmsObj attributes #
  samp_cname = getFDataColName(ftmsObj)
  edata_cname = getEDataColName(ftmsObj)

  # pull group_DF attribute #
  group_DF = attr(ftmsObj, "group_DF")
  
  # initialize the new omicsData parts #
  temp.edata <- ftmsObj$e_data
  temp.fdata <- ftmsObj$f_data
  temp.emeta <- ftmsObj$e_meta
  
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


# internal function to make a word plural but be slightly smarter than just adding an 's' to the end
# (only slightly smarter, not comprehensive)
makePlural <- function(thetext) {
  if (endsWith(thetext, 's') | endsWith(thetext, 'ch') | endsWith(thetext, 'sh') | 
      endsWith(thetext, 'x') | endsWith(thetext, 'z')  | endsWith(thetext, 'o'))
    return(paste0(thetext, "es"))
  else 
    return(paste0(thetext, "s"))
}
