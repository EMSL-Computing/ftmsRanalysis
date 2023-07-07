#' Apply an S3 filter  object to an ftmsData or CoreMSData S3 object
#'
#' This function takes a filter object of class 'moleculeFilt', 'massFilt', 'formulaFilt', 'emetaFilt', or 'confFilt' and applies the filter to a dataset of class \code{ftmsData} or \code{CoreMSData}
#'
#' @param filter_object an object of the class 'moleculeFilt', 'massFilt', 'formulaFilt', 'emetaFilt', or 'confFilt'
#' @param msObj an object of the class \code{ftmsData} or \code{CoreMSData}, created by \code{\link{as.peakData}} or \code{\link{as.CoreMSData}}, respectively
#' @param ... further arguments as described below based on the class of \code{filter_object}
#'
#' @return An object of the class \code{ftmsData} or \code{CoreMSData} with specified molecules filtered out of the appropriate datasets
#' 
#' @seealso \code{\link{molecule_filter}}, \code{\link{mass_filter}}, \code{\link{formula_filter}}, \code{\link{emeta_filter}}, \code{\link{conf_filter}}
#'
#' @author Lisa Bramer
#'
#' @export
applyFilt <- function(filter_object, msObj, ...){
  
  # check that msObj is of appropriate class #
  if(!inherits(msObj, "ftmsData") & !inherits(msObj, "CoreMSData")) stop("msObj must be of class 'ftmsData' or 'CoreMSData")
  
  # check that filter_object is of an appropriate class #
  if(!inherits(filter_object, "moleculeFilt") & 
     !inherits(filter_object, "massFilt") &
     !inherits(filter_object, "formulaFilt") &
     !inherits(filter_object, "emetaFilt") &
     !inherits(filter_object, "confFilt")) stop("filter_object must be of class 'moleculeFilt' or 'massFilt' ")
  
  # pull column names from ftmsData attributes #
  if (inherits(msObj, "ftmsData")) {
    samp_cname = getFDataColName(msObj)
    edata_cname = getEDataColName(msObj)
  }
  
  UseMethod("applyFilt")
}

# function for moleculeFilt
#' @param min_num [\code{moleculeFilt} object] an integer value specifying the minimum number of times each biomolecule must be observed across all samples in order to retain the biomolecule. Default value is 2. \cr
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.moleculeFilt <- function(filter_object, msObj, min_num=2, ...){
  
  # check to see whether a moleculeFilt has already been run on msObj #
  if("moleculeFilt" %in% names(attributes(msObj)$filters)){
    # get previous threshold #
    min_num_prev <- attributes(msObj)$filters$moleculeFilt$threshold
    
    stop(paste("A molecule filter has already been run on this dataset, using a 'min_num' of ", min_num_prev, ". See Details for more information about how to choose a threshold before applying the filter.", sep=""))
    
    
  }else{ # no previous moleculeFilt, so go ahead and run it like normal #
    
    
    # check that min_num is numeric and >=1 #
    if(!(class(min_num) %in% c("numeric","integer")) | min_num < 1) stop("min_num must be an integer greater than or equal to 1")
    # check that min_num is an integer #
    if(min_num %% 1 != 0) stop("min_num must be an integer greater than or equal to 1")
    # check that min_num is less than the number of samples #
    if(min_num > (ncol(msObj$e_data) + 1)) stop("min_num cannot be greater than the number of samples")
    # check that min_num is of length 1 #
    if(length(min_num) != 1) stop("min_num must be of length 1")
    
    edata_cname <- getEDataColName(msObj)

    num_obs <- filter_object$Num_Observations

    # get indices for which ones don't meet the min requirement #
    inds <- which(num_obs < min_num)
    
    if(length(inds) < 1){
      filter.edata <- NULL
    }
    
    else{
      filter.edata <- msObj$e_data[, which(names(msObj$e_data) == edata_cname)][inds]
    }
    
    # checking if filter specifies all of msObj$e_data
    if(all(msObj$e_data[,edata_cname] %in% filter.edata)) {stop("filter_object specifies all samples in msObj")}
    
    filter_object_new = list(edata_filt = filter.edata, emeta_filt = NULL, samples_filt = NULL)
    
    # call the function that does the filter application
    results_pieces <- filter_worker(msObj = msObj, filter_object = filter_object_new)
    
    # return filtered data object #
    results <- msObj
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
applyFilt.massFilt <- function(filter_object, msObj, min_mass = 200, max_mass = 900, ...){
  
  # check to see whether a massFilt has already been run on msObj #
  if("massFilt" %in% names(attributes(msObj)$filters)){
    # get previous threshold #
    min_num_prev <- attributes(msObj)$filters$massFilt$threshold
    
    stop(paste("A mass filter has already been run on this dataset, using a 'min_mass' and 'max_mass' of ", min_num_prev[1], "and ", min_num_prev[2], ".", sep=""))
    
    
  }else{ # no previous massFilt, so go ahead and run it like normal #
    
    
    # check that min_mass and max_mass are numeric and meet other constraints #
    if(!(class(min_mass) %in% c("numeric","integer")) | min_mass < 0) stop("min_mass must be must be a number greater than zero")
    if(!(class(max_mass) %in% c("numeric","integer")) | min_mass > max_mass) stop("max_mass must be must be a number greater than min_mass")

    # check that min_mass and max_mass are of length 1 #
    if(length(min_mass) != 1) stop("min_mass must be of length 1")
    if(length(max_mass) != 1) stop("max_mass must be of length 1")
    
    edata_cname <- getEDataColName(msObj)
    
    mass_info <- filter_object[,"Mass"]
    
    # get indices for which ones meet the requirement #
    inds <- which(mass_info <= max_mass & mass_info >= min_mass)
    
    if(length(inds) < 1) stop("Filtering using the specified minimum and maximum masses results in no peaks left in the data.")
    
    # sample identifiers to keep #
    edata_ids = filter_object[inds, "ID__"]

    temp_edata = msObj$e_data[which(msObj$e_data[,edata_cname] %in% edata_ids),]
    temp_emeta = msObj$e_meta[which(msObj$e_meta[,edata_cname] %in% edata_ids),]
    
    num_rmv = length(msObj$e_data[,edata_cname]) - length(inds)

    # set attributes for which filters were run
    attr(msObj, "filters")$massFilt <- list(report_text = "", threshold = c(), filtered = c())
    attr(msObj, "filters")$massFilt$report_text <- paste("A mass filter was applied to the data, removing ", makePlural(edata_cname), " that had a mass less than ", min_mass, " or a mass greater than ", max_mass, ". A total of ", num_rmv, " ", makePlural(edata_cname), " were filtered out of the dataset by this filter.", sep="")
    attr(msObj, "filters")$massFilt$threshold <- c(min_mass, max_mass)
    attr(msObj, "filters")$massFilt$filtered <- msObj$e_data[which(!msObj$e_data[,edata_cname] %in% edata_ids),edata_cname]
    
    msObj$e_data = temp_edata
    msObj$e_meta = temp_emeta
    results = msObj
  }
  
  return(results)
}


# function for formulaFilt
#' @param remove [\code{formulaFilt} object] a character string specifying which set of peaks to filter. Valid options are "NoFormula" and "Formula", defaults to "NoFormula". \cr
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.formulaFilt <- function(filter_object, msObj, remove = 'NoFormula', ...){
  
  # check to see whether a formulaFilt has already been run on msObj #
  if("formulaFilt" %in% names(attributes(msObj)$filters)){
    # get previous threshold #
    min_num_prev <- attributes(msObj)$filters$formulaFilt$threshold
    
    stop(paste("A formula filter has already been run on this dataset, using a 'remove' argument of ", min_num_prev, ".", sep=""))
    
    
  }else{ # no previous formulaFilt, so go ahead and run it like normal #
    
    
    # check that remove is a valid argument #
    if(!(remove %in% c("NoFormula","Formula"))) stop("'remove' can only take values 'NoFormula' and 'Formula'.")
    
    edata_cname <- getEDataColName(msObj)
    
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
      filter.edata <- msObj$e_data[, which(names(msObj$e_data) == edata_cname)][inds]
    }
    
    # checking if filter specifies all of msObj$e_data
    if(all(msObj$e_data[,edata_cname] %in% filter.edata)) {stop("filter_object specifies all samples in msObj")}
    
    filter_object_new = list(edata_filt = filter.edata, emeta_filt = NULL, samples_filt = NULL)
    
    # call the function that does the filter application
    results_pieces <- filter_worker(msObj = msObj, filter_object = filter_object_new)
    
    # return filtered data object #
    results <- msObj
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
applyFilt.emetaFilt <- function(filter_object, msObj, min_val = NULL, max_val = NULL, cats = NULL, na.rm = TRUE, ...){
  
    # determine how many filters have already been implemented on the dataset #
    num_filts = length(attributes(msObj)$filters)
    
    # create filter name #
    filt_name = paste("emetaFilt", attr(filter_object, "cname"), sep = "_")
    
    # check to see whether a formulaFilt has already been run on msObj #
    if(filt_name %in% names(attributes(msObj)$filters)){
      
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
    

    edata_cname <- getEDataColName(msObj)
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
    results_pieces <- filter_worker(msObj = msObj, filter_object = filter_object_new)
    
    # return filtered data object #
    results <- msObj
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
#' @param msObj an object of the class \code{ftmsData} usually created by \code{\link{as.peakData}}
#' @param filter_object a list created by the functions above
#' @return list
#' @author Lisa Bramer
#'
filter_worker <- function(filter_object, msObj){

  # pull column names from msObj attributes #
  samp_cname = getFDataColName(msObj)
  edata_cname = getEDataColName(msObj)

  # pull group_DF attribute #
  group_DF = attr(msObj, "group_DF")
  
  # initialize the new omicsData parts #
  temp.edata <- msObj$e_data
  temp.fdata <- msObj$f_data
  temp.emeta <- msObj$e_meta
  
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


# function for 'confFilt'
#' @param min_conf a numeric value greater than 0 specifying the minimum confidence score a peak should have in order to be retained
#' @export
#' @name applyFilt
#' @rdname applyFilt
applyFilt.confFilt <- function(filter_object, msObj, min_conf = 0.5, ...) {
  
  if ("confFilt" %in% names(attr(msObj, "filters"))) {
    prev_min_conf <- attr(msObj, "filters")$confFilt$threshold
    stop(paste0("A confidence filter has already been applied to this dataset using a 'min_conf' of ", prev_min_conf))
  } else {    # no previous confFilt
    # check min_conf is numeric and of length 1
    if(!class(min_conf) %in% c("numeric", "integer") | min_conf < 0 | min_conf > 1 | length(min_conf) != 1) stop("min_conf must be a single numeric value between 0 and 1")
    
    monoiso_orig_nrow <- nrow(msObj$monoiso_data)
    iso_orig_nrow <- nrow(msObj$iso_data)
    
    index <- attr(msObj, "cnames")$index_cname
    obs_mass <- attr(msObj, "cnames")$obs_mass_cname
    calc_mass <- attr(msObj, "cnames")$calc_mass_cname
    conf_cname <- attr(msObj, "cnames")$conf_cname
    monoiso_index <- attr(msObj, "cnames")$monoiso_index_cname
    filename <- attr(msObj, "cnames")$file_cname
    
    # get monoiso peaks to keep
    monoiso_filtered_msObj <- msObj$monoiso_data %>% dplyr::filter(.data[[conf_cname]] >= min_conf)

    # create column with unique monoisotopic index/filename identifier for pulling out associated isotopic peaks
    monoiso_peaks_to_remove <- msObj$monoiso_data %>%
      dplyr::filter(.data[[conf_cname]] < min_conf | is.na(.data[[conf_cname]])) %>% 
      dplyr::select(.data[[index]], .data[[calc_mass]], .data[[filename]]) %>% 
      dplyr::mutate(MonoIndexFile = paste0(.data[[index]], "_", .data[[filename]]))
    
    # get mass IDs of removed peaks 
    monoiso_peaks_removed <- monoiso_peaks_to_remove %>% 
      dplyr::select(calc_mass) %>% 
      as.list()
    
    # get iso peaks to remove - those associated with removed monoiso peaks and those below min_conf threshold
    iso_peaks_to_remove <- msObj$iso_data %>% 
      dplyr::select(calc_mass, monoiso_index, conf_cname, filename) %>%
      dplyr::mutate(MonoIndexFile = paste0(.data[[monoiso_index]], "_", .data[[filename]])) %>% 
      dplyr::filter(MonoIndexFile %in% monoiso_peaks_to_remove$MonoIndexFile | .data[[conf_cname]] < min_conf)
    
    # get mass ID of iso peaks removed
    iso_peaks_removed <- iso_peaks_to_remove %>% 
      dplyr::select(calc_mass) %>% 
      as.list()
    
    # get iso peaks to keep
    iso_filtered_msObj <- suppressMessages(dplyr::anti_join(x = msObj$iso_data, y = iso_peaks_to_remove))
    
    if((nrow(monoiso_filtered_msObj) < 1) & (nrow(iso_filtered_msObj) < 1)) 
      stop("Filtering using specified minimum confidence results in no peaks left in the data.")
    
    monoiso_num_rmv <- monoiso_orig_nrow - nrow(monoiso_filtered_msObj) 
    monoiso_num_na <- sum(is.na(dplyr::pull(msObj$monoiso_data, conf_cname)))

    iso_num_rmv <- iso_orig_nrow - nrow(iso_filtered_msObj) 
    iso_num_na <- sum(is.na(dplyr::pull(msObj$iso_data, conf_cname)))
        
    res <- list("monoiso_data" = monoiso_filtered_msObj, "iso_data" = iso_filtered_msObj)
    
    class(res) <- c("CoreMSData", "list")
    
    # retain 'cnames' attributes
    attr(res, "cnames") <- attr(msObj, "cnames")
    
    # add 'filters' attributes
    attr(res, "filters")$confFilt <- list(report_text = "", threshold = c(), removed = c())
    attr(res, "filters")$confFilt$report_text <- paste0("A confidence filter was applied to the data, removing peaks with a confidence score of less than ", min_conf, ". A total of ", monoiso_num_rmv, " monoisotopic peaks were removed by this filter including ", monoiso_num_na, " missing values. A total of ", iso_num_rmv, " isotopic peaks were removed by this filter including ", iso_num_na, " missing values.")
    attr(res, "filters")$confFilt$threshold <- min_conf
    attr(res, "filters")$confFilt$removed <- list("monoisotopic" = monoiso_peaks_removed, "isotopic" = iso_peaks_removed)
    
    return(res)
  }
}
