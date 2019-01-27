#'@export
#'@rdname summary.moleculeFilt
#'@name summary.moleculeFilt
#'
#'@param filter_object an object of class 'moleculeFilt' created by running \code{\link{molecule_filter}}
#'@param min_num an integer value specifying the minimum number of times each peak must be observed across all samples
#'
#'@return If \code{min_num} is provided, a summary of the effect of implementing a filter with the specified threshold. Otherwise, a summary of the number of peaks which were observed over the number of possible samples.
summary.moleculeFilt <- function(filter_object, min_num=NULL){
  
  if(!is.null(min_num)) {
    # check that min_num is not a vector #
    if(length(min_num) > 1) stop("min_num must be of length 1")
    # check that min_num is numeric >= 0 #
    if(!is.numeric(min_num) | min_num < 0) stop("min_num must be an integer >= 0")
    # check that min_num is an integer #
    if(min_num %% 1 != 0) stop("min_num must be an integer >= 0")
    # check that min_num is less than the max number of observations #
    if(min_num > max(filter_object$Num_Observations)) stop("min_num cannot be greater than the number of samples")
  }
  
  # return the numeric version of plot, the threshold used, the number that would be tested and the number that would not be tested
  
  # how many peptides appear in the dataset once, twice, 3 times, etc.
  cut_data <- table(cut(filter_object$Num_Observations, breaks = -1:max(filter_object$Num_Observations)))
  pep_observation_counts <- data.frame(num_observations=0:(length(cut_data)-1), num_peaks=cut_data)
  pep_observation_counts = pep_observation_counts[,-2]
  names(pep_observation_counts)[2] = "num_peaks"
  
  if(!is.null(min_num)){
    # get number molecules tested
    num_not_filtered<- sum(pep_observation_counts$num_peaks[pep_observation_counts$num_observations >= min_num])
    
    # get number molecules not tested
    num_filtered <- sum(pep_observation_counts$num_peaks[pep_observation_counts$num_observations < min_num])
    
  }
  
  res <- list(pep_observation_counts=pep_observation_counts, min_num=min_num, num_not_filtered=num_not_filtered, num_filtered=num_filtered)
  
  
  if(is.null(min_num)){
    print(pep_observation_counts, row.names = F)
  }else{
    catmat <- data.frame(c("Minimum Number:"=min_num, "Filtered:"=res$num_filtered, "Not Filtered:"=res$num_not_filtered))  
    names(catmat) = ""
    print(catmat)
  }
}

#'@export
#'@rdname summary.moleculeFilt
#'@name summary.moleculeFilt
#'
#'@param filter_object an object of class 'moleculeFilt' created by running \code{\link{molecule_filter}}
#'@param min_num an integer value specifying the minimum number of times each peak must be observed across all samples
#'
#'@return If \code{min_num} is provided, a summary of the effect of implementing a filter with the specified threshold. Otherwise, a summary of the number of peaks which were observed over the number of possible samples.
summary.moleculeFilt <- function(filter_object, min_num=NULL){
  
  if(!is.null(min_num)) {
    # check that min_num is not a vector #
    if(length(min_num) > 1) stop("min_num must be of length 1")
    # check that min_num is numeric >= 0 #
    if(!is.numeric(min_num) | min_num < 0) stop("min_num must be an integer >= 0")
    # check that min_num is an integer #
    if(min_num %% 1 != 0) stop("min_num must be an integer >= 0")
    # check that min_num is less than the max number of observations #
    if(min_num > max(filter_object$Num_Observations)) stop("min_num cannot be greater than the number of samples")
  }
  
  # return the numeric version of plot, the threshold used, the number that would be tested and the number that would not be tested
  
  # how many peptides appear in the dataset once, twice, 3 times, etc.
  cut_data <- table(cut(filter_object$Num_Observations, breaks = -1:max(filter_object$Num_Observations)))
  pep_observation_counts <- data.frame(num_observations=0:(length(cut_data)-1), num_peaks=cut_data)
  pep_observation_counts = pep_observation_counts[,-2]
  names(pep_observation_counts)[2] = "num_peaks"
  
  if(!is.null(min_num)){
    # get number molecules tested
    num_not_filtered<- sum(pep_observation_counts$num_peaks[pep_observation_counts$num_observations >= min_num])
    
    # get number molecules not tested
    num_filtered <- sum(pep_observation_counts$num_peaks[pep_observation_counts$num_observations < min_num])
    
  }
  
  res <- list(pep_observation_counts=pep_observation_counts, min_num=min_num, num_not_filtered=num_not_filtered, num_filtered=num_filtered)
  
  
  if(is.null(min_num)){
    print(pep_observation_counts, row.names = F)
  }else{
    catmat <- data.frame(c("Minimum Number:"=min_num, "Filtered:"=res$num_filtered, "Not Filtered:"=res$num_not_filtered))  
    names(catmat) = ""
    print(catmat)
  }
}

#'@export
#'@rdname summary.moleculeFilt
#'@name summary.moleculeFilt
#'
#'@param filter_object an object of class 'moleculeFilt' created by running \code{\link{molecule_filter}}
#'@param min_num an integer value specifying the minimum number of times each peak must be observed across all samples
#'
#'@return If \code{min_num} is provided, a summary of the effect of implementing a filter with the specified threshold. Otherwise, a summary of the number of peaks which were observed over the number of possible samples.
summary.moleculeFilt <- function(filter_object, min_num=NULL){
  
  if(!is.null(min_num)) {
    # check that min_num is not a vector #
    if(length(min_num) > 1) stop("min_num must be of length 1")
    # check that min_num is numeric >= 0 #
    if(!is.numeric(min_num) | min_num < 0) stop("min_num must be an integer >= 0")
    # check that min_num is an integer #
    if(min_num %% 1 != 0) stop("min_num must be an integer >= 0")
    # check that min_num is less than the max number of observations #
    if(min_num > max(filter_object$Num_Observations)) stop("min_num cannot be greater than the number of samples")
  }
  
  # return the numeric version of plot, the threshold used, the number that would be tested and the number that would not be tested
  
  # how many peptides appear in the dataset once, twice, 3 times, etc.
  cut_data <- table(cut(filter_object$Num_Observations, breaks = -1:max(filter_object$Num_Observations)))
  pep_observation_counts <- data.frame(num_observations=0:(length(cut_data)-1), num_peaks=cut_data)
  pep_observation_counts = pep_observation_counts[,-2]
  names(pep_observation_counts)[2] = "num_peaks"
  
  if(!is.null(min_num)){
    # get number molecules tested
    num_not_filtered<- sum(pep_observation_counts$num_peaks[pep_observation_counts$num_observations >= min_num])
    
    # get number molecules not tested
    num_filtered <- sum(pep_observation_counts$num_peaks[pep_observation_counts$num_observations < min_num])
    
  }
  
  res <- list(pep_observation_counts=pep_observation_counts, min_num=min_num, num_not_filtered=num_not_filtered, num_filtered=num_filtered)
  
  
  if(is.null(min_num)){
    print(pep_observation_counts, row.names = F)
  }else{
    catmat <- data.frame(c("Minimum Number:"=min_num, "Filtered:"=res$num_filtered, "Not Filtered:"=res$num_not_filtered))  
    names(catmat) = ""
    print(catmat)
  }
}

#'@export
#'@rdname summary.massFilt
#'@name summary.massFilt
#'
#'@param filter_object an object of class 'massFilt' created by running \code{\link{mass_filter}}
#'@param min_mass an integer value specifying the minimum mass a peak must have to be retained in the dataset (inclusive). If only \code{max_mass} is provided, then this values is assumed to be the minimum observed mass value.
#'@param max_mass an integer value specifying the maximum mass a peak can have to be retained in the dataset (inclusive). If only \code{min_mass} is provided, then this value is assumed to be the maximum observed mass value.
#'
#'@return If \code{min_mass} and/or \code{max_mass} is provided, a summary of the effect of implementing a filter with the specified thresholds. Otherwise, a five-number summary and the mean value of the mass values observed across all samples are given.
summary.massFilt <- function(filter_object, min_mass = NULL, max_mass = NULL){
  
  # check to see if only one of min or max is provided #
  # if so, set the null one to the observed min/max #
  if(!is.null(min_mass) & is.null(max_mass)){max_mass = max(filter_object$Mass)}
  if(is.null(min_mass) & !is.null(max_mass)){min_mass = min(filter_object$Mass)}

  if(!is.null(min_mass)) {
  # check that min_mass and max_mass are numeric and meet other constraints #
  if(!(class(min_mass) %in% c("numeric","integer")) | min_mass < 0) stop("min_mass must be must be a number greater than zero")
  if(length(min_mass) != 1) stop("min_mass must be of length 1")
  }
  if(!is.null(max_mass)){
  if(!(class(max_mass) %in% c("numeric","integer")) | min_mass > max_mass) stop("max_mass must be must be a number greater than min_mass")
  if(length(max_mass) != 1) stop("max_mass must be of length 1")
  }  
  
  # if both min and max are null calculate the five number summary #  
  if(is.null(min_mass) & is.null(max_mass)) {
    summary(filter_object$Mass)
  }else{
    num_not_filtered = sum(filter_object$Mass >= min_mass & filter_object$Mass <= max_mass)
    num_filtered = nrow(filter_object) - num_not_filtered
    catmat <- data.frame(c("Minimum Mass:"=min_mass, "Maximum Mass:"=max_mass, "Filtered:"=round(num_filtered,0), "Not Filtered:"=round(num_not_filtered,0)))  
    names(catmat) = ""
    print(catmat)
  }
  
}


#'@export
#'@rdname summary.formulaFilt
#'@name summary.formulaFilt
#'
#'@param filter_object an object of class 'formulaFilt' created by running \code{\link{formula_filter}}
#'@param remove an character string specifying whether to remove peaks without formulae assigned ('NoFormula') or with formulae assigned ('Formula')
#'
#'@return If \code{remove} is provided, a summary of the effect of implementing a filter. Otherwise, a summary of the number of peaks with assigned and unassigned formulae
summary.formulaFilt <- function(filter_object, remove = NULL){
  if(!is.null(remove)){
  if(!(remove %in% c("NoFormula","Formula"))) stop("'remove' can only take values 'NoFormula' and 'Formula'.")
  }
  # if remove is NULL return distribution #  
  if(is.null(remove)) {
    catmat = data.frame(c("Formula Assigned:"=sum(filter_object$Formula_Assigned), "No Formula Assigned:"=nrow(filter_object)-sum(filter_object$Formula_Assigned)))
    names(catmat) = ""
    print(catmat)
  }else{
    num_noform = sum(filter_object$Formula_Assigned)
    num_form = nrow(filter_object) - num_noform
    if(remove == "NoFormula"){
    catmat <- data.frame(c("Remove:"=remove, "Filtered:"=num_noform, "Not Filtered:"=num_form))  
    names(catmat) = ""
    print(catmat)
    }else{
      catmat <- data.frame(c("Remove:"=remove, "Filtered:"=num_form, "Not Filtered:"=num_noform))  
      names(catmat) = ""
      print(catmat)  
    }
  
  }
}



#' Summarize emetaFilt object
#'
#'@rdname summary.emetaFilt
#'@name summary.emetaFilt
#'
#'@param filter_object an object of class 'emetaFilt' created by running \code{\link{emeta_filter}}
#'@param min_val used only if filter variable specified was quantitative. Minimum value which the filter variable can take (inclusive) to remain in the data
#'@param max_val used only if filter variable specified was quantitative. Maximum value which the filter variable can take (inclusive) to remain in the data
#'@param cats used only if filter variable specified was categorical. Levels of categorical variable which should be retained in the data
#'@param na.rm should peaks with an NA value for the filter variable be removed. Defaults to TRUE.
#'
#'@return If relevant parameter(s) are provided, a summary of the effect of implementing a filter. Otherwise, a five number summary and the mean value for the filter variable (quantitative) or a summary of the number of peaks per category (categorical).
#'
#'@export

summary.emetaFilt <- function(filter_object, min_val = NULL, max_val = NULL, cats = NULL, na.rm = TRUE){
  # get variable type #
  var_type = attr(filter_object, "type")
  
  # get variable name #
  var_name = attr(filter_object, "cname")
  
  # set some defaults for quantitative variable #
  if(var_type == "quantitative"){
    # check to see if only one of min or max is provided #
    # if so, set the null one to the observed min/max #
    if(!is.null(min_val) & is.null(max_val)){max_val = max(filter_object$emeta_value, na.rm = T)}
    if(is.null(min_val) & !is.null(max_val)){min_val = min(filter_object$emeta_value, na.rm = T)}
    
    if(!is.null(min_val)) {
      if(length(min_val) != 1) stop("min_val must be of length 1")
    }
    if(!is.null(max_val)){
      if(length(max_val) != 1) stop("max_val must be of length 1")
    }  
  }
  if(var_type == "categorical"){
    # check that cats is of class character #
    if(!is.null(cats) & !inherits(cats, "character")) stop("'cats' must be a character string or vector of character strings")
  }
  
  
  # implement filter #
  if(var_type == "quantitative"){
    # if both min and max are null #
    if(is.null(min_val) & is.null(max_val)){
      print(summary(filter_object$emeta_value))
    }else{
      if(na.rm == TRUE){
      num_not_filtered = sum(filter_object$emeta_value >= min_val & filter_object$emeta_value <= max_val, na.rm = T)
      num_filtered = nrow(filter_object) - num_not_filtered
      catmat <- data.frame(c("Filter Variable:"=var_name,"Minimum Value:"=min_val, "Maximum Value:"=max_val, "Filtered:"=round(num_filtered,0), "Not Filtered:"=round(num_not_filtered,0)))  
      names(catmat) = ""
      print(catmat)
      }else{
        num_not_filtered = sum(filter_object$emeta_value >= min_val & filter_object$emeta_value <= max_val, na.rm = T) + sum(is.na(filter_object$emeta_value))
        num_filtered = nrow(filter_object) - num_not_filtered
        catmat <- data.frame(c("Filter Variable:"=var_name,"Minimum Value:"=min_val, "Maximum Value:"=max_val, "Filtered:"=round(num_filtered,0), "Not Filtered:"=round(num_not_filtered,0)))  
        names(catmat) = ""
        print(catmat)  
      }
    }
  }
  if(var_type == "categorical"){
    # if cats is null #
    if(is.null(cats)){
      table(filter_object$emeta_value)
    }else{
      if(na.rm == TRUE){
        num_not_filtered = sum(filter_object$emeta_value %in% cats)
        num_filtered = nrow(filter_object) - num_not_filtered
        catmat <- data.frame(c("Filter Variable:"=var_name,"Categories Kept:"=paste(cats, collapse = ","), "Filtered:"=round(num_filtered,0), "Not Filtered:"=round(num_not_filtered,0)))  
        names(catmat) = ""
        print(catmat)
      }else{
        num_not_filtered = sum(filter_object$emeta_value %in% cats) + sum(is.na(filter_object$emeta_value))
        num_filtered = nrow(filter_object) - num_not_filtered
        catmat <- data.frame(c("Filter Variable:"=var_name,"Categories Kept:"=paste(c("NA",cats), collapse = ","), "Filtered:"=round(num_filtered,0), "Not Filtered:"=round(num_not_filtered,0)))  
        names(catmat) = ""
        print(catmat)
      }
    }
  }
}
