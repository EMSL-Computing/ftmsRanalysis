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
    results$e_data <- results_pieces$temp.pep2
    results$f_data <- results_pieces$temp.samp2
    results$e_meta <- results_pieces$temp.meta1
    
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
    results$e_data <- results_pieces$temp.pep2
    results$f_data <- results_pieces$temp.samp2
    results$e_meta <- results_pieces$temp.meta1
    
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
  # pull column names from omicR_data attributes #
  samp_cname = getFDataColName(icrData)
  edata_cname = getEDataColName(icrData)

  # pull group_DF attribute #
  group_DF = attr(icrData, "group_DF")
  
  #check if filter object contains remove arguments
  if(!is.null(filter_object$edata_filt)||!is.null(filter_object$emeta_filt)||!is.null(filter_object$samples_filt)){
    
    ## check to see if e_meta is provided ##
    # if not provided we only need to worry about e_data and f_data #
    if(is.null(icrData$e_meta)){
      
      ## remove entries in edata ##
      if(!is.null(filter_object$edata_filt) & !is.null(edata_cname)){
        
        temp.pep = icrData$e_data
        
        # have to check that at least one of the items is present in the data #
        edat_ids = which(temp.pep[,edata_cname] %in% filter_object$edata_filt)
        
        if(length(edat_ids) > 0){
          # identify which peptides in e_data match filter list and remove #
          temp.pep1 = temp.pep[-which(temp.pep[,edata_cname] %in% filter_object$edata_filt),]
        }else{temp.pep1 = temp.pep}
        
      }else{ # no entries in edata need to be removed
        temp.pep1 = icrData$e_data
      }
      
      ## remove samples ##
      if(!is.null(filter_object$samples_filt) & !is.null(samp_cname)){
        # identify which samples in f_data match filter list #
        temp.samp = icrData$f_data
        
        # check that at least one sample is in f_data and e_data #
        fdat_ids = which(temp.samp[,samp_cname] %in% filter_object$samples_filt)
        edat_ids2 = which(names(temp.pep1) %in% filter_object$samples_filt)
        
        if(length(fdat_ids) > 0){
          temp.samp2 = temp.samp[-which(temp.samp[,samp_cname] %in% filter_object$samples_filt),]
        }else{temp.samp2 = temp.samp}
        
        # identify which samples in e_data match filter list and remove #
        if(length(edat_ids2) > 0){
          temp.pep2 = temp.pep1[, -which(names(temp.pep1) %in% filter_object$samples_filt)]
        }else{temp.pep2 = temp.pep1}
        
      }else{ # no entries in f_data need to be removed
        temp.samp2 = icrData$f_data
        temp.pep2 = temp.pep1
      }
      
      temp.meta2 = NULL
      
      
    }else{ # e_meta is present, so we need to work with it as well
      ## remove entries in edata ##
      if(!is.null(filter_object$edata_filt) & !is.null(edata_cname)){
        
        temp.pep = icrData$e_data
        
        # have to check that at least one of the items is present in the data #
        edat_ids = which(temp.pep[,edata_cname] %in% filter_object$edata_filt)
        
        if(length(edat_ids) > 0){
          # identify which peptides in e_data and e_meta match filter list and remove#
          temp.pep1 = temp.pep[-which(temp.pep[,edata_cname] %in% filter_object$edata_filt),]
        }else{temp.pep1 = temp.pep}
        
        temp.meta = icrData$e_meta
        
        # check that at least one of the peptides is present in e_meta #
        emeta_ids = which(temp.meta[,edata_cname] %in% filter_object$edata_filt)
        
        if(length(emeta_ids) > 0){
          temp.meta1 = temp.meta[-which(temp.meta[,edata_cname] %in% filter_object$edata_filt),]
        }else{temp.meta1 = temp.meta}
        
      }else{
        temp.pep1 = icrData$e_data
        temp.meta1 = icrData$e_meta
      }
      
      ## remove samples ##
      if(!is.null(filter_object$samples_filt) & !is.null(samp_cname)){
        # identify which samples in f_data match filter list #
        temp.samp = icrData$f_data
        
        # check that at least one sample is in f_data and e_data #
        fdat_ids = which(temp.samp[,samp_cname] %in% filter_object$samples_filt)
        edat_ids2 = which(names(temp.pep1) %in% filter_object$samples_filt)
        
        if(length(fdat_ids) > 0){
          temp.samp2 = temp.samp[-which(temp.samp[,samp_cname] %in% filter_object$samples_filt),]
        }else{temp.samp2 = temp.samp}
        
        # identify which samples in e_data match filter list and remove #
        if(length(edat_ids2) > 0){
          inds = which(names(temp.pep1) %in% filter_object$samples_filt)
          temp.pep2 = temp.pep1[, -inds]
        }else{temp.pep2 = temp.pep1}
        
      }else{
        temp.samp2 = icrData$f_data
        temp.pep2 = temp.pep1
      }
      
      ## remove entries in emeta ##
      if(!is.null(filter_object$emeta_filt) & !is.null(edata_cname)){
        # identify which proteins in data match filter list and remove from e_meta #
        temp.meta = temp.meta1
        
        # check that at least one of the proteins is in e_meta #
        if(!is.null(ncol(temp.meta))){
          emeta_ids2 = which(as.character(temp.meta[,edata_cname]) %in% filter_object$emeta_filt)
        }else{
          emeta_ids2 = which(temp.meta %in% filter_object$emeta_filt)
        }
        
        
        if(length(emeta_ids2) > 0){
          if(!is.null(ncol(temp.meta))){
            temp.meta2 = temp.meta[-which(temp.meta[,edata_cname] %in% filter_object$emeta_filt),]
          }else{
            temp.meta2 = temp.meta[-which(temp.meta %in% filter_object$emeta_filt)]
          }
        }else{temp.meta2 = temp.meta}
      }else{
        temp.meta2 = temp.meta1
      }
      
      
      # check for rogue entries in edata #
      if(!is.null(ncol(temp.meta2))){
        edat_ids2 = which(!(temp.pep2[,edata_cname] %in% temp.meta2[,edata_cname]))
      }else{
        edat_ids2 = which(!(temp.pep2[,edata_cname] %in% temp.meta2))
      }
      
      
      # filter out edata entries which no longer have mappings to emeta entries #
      if(length(edat_ids2) > 0){
        #temp.pep2 = temp.pep2[-which(!(temp.pep2[,edata_cname] %in% temp.meta2[,edata_cname])),]
        temp.pep2 = temp.pep2[-edat_ids2,]
      }
      
    }
    
    output <- list(temp.pep2 = temp.pep2, temp.samp2 = temp.samp2, temp.meta1 = temp.meta2, edata_cname = edata_cname, samp_cname = samp_cname)
  }
  
  
  #if filter object contains keep arguments
  else{
    
    ## check to see if e_meta is provided ##
    # if not provided we only need to worry about e_data and f_data #
    if(is.null(icrData$e_meta)){
      
      ## keep entries in edata ##
      if(!is.null(filter_object$edata_keep) & !is.null(edata_cname)){
        
        temp.pep = icrData$e_data
        
        # have to check that at least one of the items is present in the data #
        edat_ids = which(temp.pep[,edata_cname] %in% filter_object$edata_keep)
        
        if(length(edat_ids) > 0){
          # identify which peptides in e_data match filter list and keep #
          temp.pep1 = temp.pep[which(temp.pep[,edata_cname] %in% filter_object$edata_keep),]
        }else{temp.pep1 = temp.pep}
        
      }else{ # no entries in edata need to be removed
        temp.pep1 = icrData$e_data
      }
      
      ## keep samples ##
      if(!is.null(filter_object$samples_keep) & !is.null(samp_cname)){
        # identify which samples in f_data match filter list #
        temp.samp = icrData$f_data
        
        # check that at least one sample is in f_data and e_data #
        fdat_ids = which(temp.samp[,samp_cname] %in% filter_object$samples_keep)
        edat_ids2 = which(names(temp.pep1) %in% filter_object$samples_keep)
        
        if(length(fdat_ids) > 0){
          temp.samp2 = temp.samp[which(temp.samp[,samp_cname] %in% filter_object$samples_keep),]
        }else{temp.samp2 = temp.samp}
        
        # identify which samples in e_data match filter list and keep #
        if(length(edat_ids2) > 0){
          edata_cname_id = which(names(temp.pep1) == edata_cname)
          temp.pep2 = temp.pep1[,c(edata_cname_id,(which(names(temp.pep1) %in% filter_object$samples_keep)))]
        }else{temp.pep2 = temp.pep1}
        
      }else{ # no entries in f_data need to be removed
        temp.samp2 = icrData$f_data
        temp.pep2 = temp.pep1
      }
      
      temp.meta2 = NULL
      
      
      
      
    }else{ # e_meta is present, so we need to work with it as well
      ## keep entries in edata ##
      if(!is.null(filter_object$edata_keep) & !is.null(edata_cname)){
        
        temp.pep = icrData$e_data
        
        # have to check that at least one of the items is present in the data #
        edat_ids = which(temp.pep[,edata_cname] %in% filter_object$edata_keep)
        
        if(length(edat_ids) > 0){
          # identify which peptides in e_data and e_meta match filter list and keep#
          temp.pep1 = temp.pep[which(temp.pep[,edata_cname] %in% filter_object$edata_keep),]
        }else{temp.pep1 = temp.pep}
        
        temp.meta = icrData$e_meta
        
        # check that at least one of the peptides is present in e_meta #
        emeta_ids = which(temp.meta[,edata_cname] %in% filter_object$edata_keep)
        
        if(length(emeta_ids) > 0){
          temp.meta1 = temp.meta[which(temp.meta[,edata_cname] %in% filter_object$edata_keep),]
        }else{temp.meta1 = temp.meta}
        
      }else{
        temp.pep1 = icrData$e_data
        temp.meta1 = icrData$e_meta
      }
      
      ## keep samples ##
      if(!is.null(filter_object$samples_keep) & !is.null(samp_cname)){
        # identify which samples in f_data match filter list #
        temp.samp = icrData$f_data
        
        # check that at least one sample is in f_data and e_data #
        fdat_ids = which(temp.samp[,samp_cname] %in% filter_object$samples_keep)
        edat_ids2 = which(names(temp.pep1) %in% filter_object$samples_keep)
        
        if(length(fdat_ids) > 0){
          temp.samp2 = temp.samp[which(temp.samp[,samp_cname] %in% filter_object$samples_keep),]
        }else{temp.samp2 = temp.samp}
        
        # identify which samples in e_data match filter list and keep #
        if(length(edat_ids2) > 0){
          edata_cname_id = which(names(temp.pep1) == edata_cname)
          
          inds = which(names(temp.pep1) %in% filter_object$samples_keep)
          temp.pep2 = temp.pep1[,c(edata_cname_id,inds)]
        }else{temp.pep2 = temp.pep1}
        
      }else{
        temp.samp2 = icrData$f_data
        temp.pep2 = temp.pep1
      }
      
      ## keep entries in emeta ##
      if(!is.null(filter_object$emeta_keep) & !is.null(edata_cname)){
        # identify which proteins in data match filter list and keep in e_meta #
        temp.meta = temp.meta1
        temp.meta_not_kept = icrData$e_meta[-which(icrData$e_meta[,edata_cname] %in% temp.meta[,edata_cname]),]
        
        # check that at least one of the proteins is in e_meta (this is e_meta after e_data_keep has been applied) #
        if(!is.null(ncol(temp.meta))){
          emeta_ids2 = which(as.character(temp.meta[,edata_cname]) %in% filter_object$emeta_keep)
        }else{
          emeta_ids2 = which(temp.meta %in% filter_object$emeta_keep)
        }
        
        # check that at least one of the proteins is in temp_meta_not_kept #
        if(!is.null(ncol(temp.meta_not_kept))){
          emeta_ids_not_kept = which(as.character(temp.meta_not_kept[,edata_cname]) %in% filter_object$emeta_keep)
        }else{
          emeta_ids_not_kept = which(temp.meta_not_kept %in% filter_object$emeta_keep)
        }
        
        #if there are e_meta_keep (proteins) in the part of e_meta that we previously kept, then these proteins have already been kept
        if(length(emeta_ids2) > 0){
          if(!is.null(ncol(temp.meta))){
            temp.meta2 = temp.meta
          }else{
            temp.meta2 = temp.meta
          }
        }else{temp.meta2 = temp.meta}
        
        #if there are e_meta_keep(proteins) outside of e_meta that we previously kept we will keep these 
        if(length(emeta_ids_not_kept) > 0){
          if(!is.null(ncol(temp.meta_not_kept))){
            temp.meta3 = temp.meta_not_kept[which(temp.meta_not_kept[,edata_cname] %in% filter_object$emeta_keep),]
            temp.meta2 = rbind(temp.meta2,temp.meta3)
          }else{
            temp.meta3 = temp.meta_not_kept[which(temp.meta_not_kept %in% filter_object$emeta_keep)]
            temp.meta2 = rbind(temp.meta2, temp.meta3)
          }
        }else{temp.meta2 = temp.meta}
        
      }else{
        temp.meta2 = temp.meta1
      }
      
      
      # check for entries in e_meta[,emeta_cname] that are not in e_data[,edata_cname]#
      if(!is.null(ncol(temp.meta2))){
        edat_ids2 = which(!temp.meta2[,edata_cname] %in% (temp.pep2[,edata_cname]))
      }else{
        edat_ids2 = which(!(temp.meta2 %in% temp.pep2[,edata_cname]))
      }
      
      
      # add edata entries which were present in emeta but not edata #
      if(length(edat_ids2) > 0){
        additional_peps<- temp.meta2[edat_ids2, edata_cname]
        edata_cname_id = which(names(temp.pep1) == edata_cname)
        
        if(is.null(filter_object$samples_keep)){
          inds = which(names(temp.pep1) %in% temp.samp2[,samp_cname]) 
        }
        else inds = which(names(temp.pep1) %in% filter_object$samples_keep)
        
        temp.pep2 = rbind(temp.pep2, icrData$e_data[which(icrData$e_data[,edata_cname] %in% additional_peps) ,c(edata_cname_id,inds)])
      }
      
      
      
    }
    
    output <- list(temp.pep2 = temp.pep2, temp.samp2 = temp.samp2, temp.meta1 = temp.meta2, edata_cname = edata_cname, samp_cname = samp_cname)
    
  }
  
  
  # return the pieces needed to assemble a proData/pepData/lipidData/metabData object
  return(output)
}
