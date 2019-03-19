#' Creates Data Frame for Group Membership Based on Specified Main Effects
#'
#' The method assigns each sample to a group, for future analyses, based on the variable(s) specified as main effects.
#'
#' @param ftmsObj an object of the class 'ftmsData', usually created by \code{\link{as.peakData}}.
#' @param main_effects a character vector with no more than two variable names that should be used as main effects to determine group membership of samples. The variable name must match a column name from \code{f_data}.
#' @param covariates a character vector of no more than two variable names that should be used as covariates in downstream analyses. Covariates are typically variables that a user wants to account for in the analysis but quantifying/examining the effect of the variable is not of interest.
#' @param var.name logical, if there are more than one main effect, when making a group, include (TRUE) or exclude (FALSE) main effect names in group For example, if TRUE, "Location_M;Crop_S". If FALSE, "M_S". Default is FALSE.
#'
#' @details Groups are formed based on the levels of the main effect variables. One or two main effect variables are allowed. In the case of two main effect variables, groups are formed based on unique combinations of the levels of the two main effect variables. Any samples with level NA for a main effect variable will be removed from the data and will not be included in the final group designation results. Any groups with less than 2 samples will be designated to group NA and the affected sample(s) will be removed from the data.
#'
#' @return An object of the same class as the input \code{ftmsObj} object - the provided object with the samples filtered out, if any NAs were produced in designating groups. An attribute 'group_DF', a data.frame with columns for sample id and group, is added to the object. If two main effects are provided the original main effect levels for each sample are returned as the third and fourth columns of the data.frame. If time_course is included, a column for 'TimeCourse' will be output as well. Additionally, the covariates provided will be listed as attributes of this data.frame.
#'
#'
#' @author Lisa Bramer
#'
#' @export


group_designation <- function(ftmsObj, main_effects, covariates=NULL, var.name=FALSE){
  
  ### perform some intial checks that data is in an acceptable format ###
  
  # check that ftmsObj is of appropriate class #
  if(!inherits(ftmsObj, "ftmsData")) stop("ftmsObj is not an object of appropriate class")
  
  # Check that main_effects are character vector #
  if( !is.character(main_effects) ) stop("main_effects must be a character vector.")
  
  # Check that main_effects is of an appropriate length #
  if (length(main_effects) < 1) stop("No main effects were provided")
  #if (length(main_effects) > 2) stop("No more than two main effects can be provided")
  
  # Check that covariates is of an appropriate length #
  if( !is.null(covariates)){
    if (length(covariates) > 2){
      stop("No more than two covariates can be provided")
    }
  }
  
  # Check that main_effects given are in f_data #
  if(sum(main_effects %in% names(ftmsObj$f_data))!=length(main_effects)) stop("One or more of the main_effects is not found in f_data of ftmsObj")
  
  ### end of preliminary checks ###
  
  # store f_data temporarily #
  temp_data = ftmsObj$f_data
  
  # pull sample id column name #
  samp_id = attr(ftmsObj, "cnames")$fdata_cname
  
  # Get number of main effects #
  n.maineffects = length(main_effects)
  
  # Case 1: 1 main effect variable #
  if(n.maineffects==1){
    # return groups as levels of the main effect variable #
    Group = temp_data[,names(temp_data) %in% main_effects]
    
    # create output formatted with first column being sample id and second column group id #
    output = data.frame(Sample.ID = temp_data[,samp_id], Group = factor(as.character(Group)))
    names(output)[1] = samp_id
    
  }
  
  # Case 2: 2 main effect variables #
  if(n.maineffects>=2){
    
    # get main effect variables #
    obs.effects = temp_data[,names(temp_data)%in% main_effects]
    obs.effects <- apply(obs.effects, 2, as.character)
    
    # create a group variable and paste main effect levels together for samples #
    # samples with a value of NA for either main effect will have a Group value of NA #
    Group = rep(NA, nrow(temp_data))
    
    # identify samples that will have a Group membership that is not missing #
    #nonna.group = (!is.na(obs.effects[,1]) & !is.na(obs.effects[,2]) )
    nonna.group = apply(obs.effects, 1, function(x) all(!is.na(x)))
    
    #Group[nonna.group] = paste(as.character(obs.effects[nonna.group,1]), as.character(obs.effects[nonna.group,2]), sep = "_")
    if(var.name){
      Group[nonna.group] = apply(obs.effects[nonna.group,], 1, function(x) paste(sapply(c(1:ncol(obs.effects)), function(y) paste(colnames(obs.effects)[y], x[y], sep=".")),collapse="_"))
    }else{
      Group[nonna.group] = apply(obs.effects[nonna.group,], 1, function(x) paste(x, collapse="_"))
    }

    
    # create output formatted with first column being sample id and second column group id #
    # third+ columns are the original main effect levels #
    output = data.frame(Sample.ID = temp_data[,samp_id], Group = factor(Group), obs.effects)
    names(output)[1] = samp_id
  }
  
  
  # check for group sizes <2; if there are any, set them to NA's
  if(any(table(output$Group)<2)){
    output$Group = as.character(output$Group)
    sm.groups = which(table(output$Group)<2)
    sm.groups = names(sm.groups)
    n.sm.groups = length(sm.groups)
    if(n.sm.groups>0){
      for(i in 1:n.sm.groups){
        output$Group[output$Group==sm.groups[i]] = NA
      }
    }
    
    # output a warning message listing the groups and samples that are removed (7/8/2016 KS)
    sm.samps = as.character(output[is.na(output$Group), samp_id])
    n.samps = length(sm.samps)
    mystr <- paste("The following ", n.sm.groups, " groups have been removed from the dataset due to a group size of less than 2 samples: ", sep="")
    mystr2 <- paste(as.character(sm.groups), sep="' '", collapse=", ")
    mystr3 <- paste(". This corresponds to the following ", n.samps, " samples: ", sep="")
    mystr4 <- paste(as.character(sm.samps), sep="' '", collapse=", ")
    warning(paste(mystr, mystr2, mystr3, mystr4, sep=""))
  }
  
  # find any samples with NA for group membership #
  if(length(which(is.na(output$Group))) > 0){
    na.group = as.character(output[which(is.na(output$Group)), samp_id])
  }else{na.group = NULL}
  
  all.na = na.group
  
  # remove samples with group or time NA from output and data
  if(length(all.na) > 0){
    # remove from output #
    output2 = output[-which(output[,samp_id] %in% all.na),]
    
    # remove from e_data and f_data #
    edat_ids = which(names(ftmsObj$e_data) %in% all.na)
    fdat_ids = which(temp_data[ , samp_id] %in% all.na)
    
    ftmsObj$e_data = ftmsObj$e_data[,-edat_ids]
    ftmsObj$f_data = ftmsObj$f_data[-fdat_ids,]
  }else{output2 = output}
  
  attr(output2, "main_effects") = main_effects
  attr(output2, "covariates") = covariates
  attr(ftmsObj, "group_DF") = output2
  
  
  return(ftmsObj)
}
