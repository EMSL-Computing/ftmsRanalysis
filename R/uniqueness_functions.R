#' Determine Unique and Common Masses/Peaks for Two Groups
#' 
#' Use number of samples for which a mass/peak is present to compare two groups and determine which peaks are uniquely expressed in each group
#' 
#' @param edata_df data.frame giving samples expression profiles for samples belonging to one of two groups
#' @param group_df data.frame giving sample names and column indicating group membership. Column indicating group membership should be named 'Group' and have exactly two unique values
#' @param data_scale character string giving scale the data is on. Current acceptable values are: "log", "log2", "log10", "abundance", "pres"
#' @param pres_thresh integer value indicating the minimum number of samples per group that are required for a peak to be considered present/observed in a group.
#' @param absn_thresh integer value indicating the maximum number of samples per group that can be observed for a peak to be considered absent in a group.
#' 
#' @name uniqueness_nsamps
#' @author Lisa Bramer
#' 

uniqueness_nsamps <- function(edata_df, group_df, data_scale, pres_thresh, absn_thresh){
  
  # check that edata_df is data.frame #
  if(!inherits(edata_df, "data.frame")) stop("edata_df must be of class 'data.frame'")
  
  # check that group_df is data.frame #
  if(!inherits(group_df, "data.frame")) stop("group_df must be of class 'data.frame'")
  
  # check that group_df has two columns and one is title 'Group' #
  if(ncol(group_df) != 2) stop("group_df should be a data.frame with two columns")
  if(!("Group" %in% names(group_df))) stop("group_df must have one column named 'Group' giving sample group membership")
  
  # check that there are exactly two group levels #
  if(length(unique(group_df$Group)) != 2) stop("Group column in group_df must contain exactly two group levels")

  # check that data_scale is valid #
  if(class(data_scale) != "character") stop("data_scale must be of class 'character'")
  if(!(data_scale %in% c("log", "log2", "log10", "abundance", "pres"))) stop("data_scale value is not an invalid option.")
  
  # determine which column gives the samples in group_df #
  samp_colname = names(group_df)[which(names(group_df) != "Group")]
  
  # get unique group levels #
  grps = as.character(unique(group_df$Group))
  
  # get sample names which belong to each group level #
  grp1_samps = group_df[which(group_df$Group == grps[1]), samp_colname]
  grp2_samps = group_df[which(group_df$Group == grps[2]), samp_colname]
  
  max_n_samps = max(c(length(grp1_samps), length(grp2_samps)))
  min_n_samps = min(c(length(grp1_samps), length(grp2_samps)))
  
  # ensure that pres_thresh and absn_thresh are integers #
  pres_thresh = as.integer(pres_thresh)
  absn_thresh = as.integer(absn_thresh)
  
  # check that pres_thresh is greater than 0 and less than min number of samples in groups #
  if(!(pres_thresh > 0 & pres_thresh <= min_n_samps)) stop("pres_thresh must be an integer greater than 0 and less than the minimum number of samples in a group")
  
  # check that absn_thresh is greater than or equal to 0 and less than the min number of samples in groups #
  if(!(absn_thresh >= 0 & absn_thresh <= min_n_samps)) stop("absn_thresh must be an integer greater than or equal to 0 and less than the minimum number of samples in a group")
  
  # check that absn_thresh is less than pres_thresh #
  if(!(absn_thresh < pres_thresh)) stop("absn_thresh must be less than pres_thresh")
  # if data is not in presence/absence form, convert to pres/abs (number or NA) #
  if(data_scale != "pres"){
    for(i in 1:ncol(edata_df)){
      edata_df[which(edata_df[,i] == 0), i] = NA
    }
  }
  
  # pull group membership column and ensure that order matches edata_df column order #
  ord_groupdf = group_df[match(names(edata_df), group_df[,samp_colname]),]
  

  
  # calculate the maximum (over the two groups) number of samples observed per group #
  numpres = data.frame(N_grp1 = apply(!is.na(edata_df[,which(names(edata_df) %in% grp1_samps)]), 1, sum),
                            N_grp2 = apply(!is.na(edata_df[,which(names(edata_df) %in% grp2_samps)]), 1, sum)
                        )
  
  
  # construct results vector #
  res_vec = rep(NA, nrow(edata_df))
  res_vec[which(apply(numpres, 1, min) >= pres_thresh)] = "Observed in Both"
  res_vec[which(numpres$N_grp1 >= pres_thresh & numpres$N_grp2 <= absn_thresh)] = paste("Unique to", grps[1], sep = " ")
  res_vec[which(numpres$N_grp1 <= absn_thresh & numpres$N_grp2 >= pres_thresh)] = paste("Unique to", grps[2], sep = " ")
  
  data.frame(uniqueness_nsamps = res_vec)
}


#' Determine Unique and Common Masses/Peaks for Two Groups
#' 
#' Use proportion of samples for which a mass/peak is present to compare two groups and determine which peaks are uniquely expressed in each group
#' 
#' @param edata_df data.frame giving samples expression profiles for samples belonging to one of two groups
#' @param group_df data.frame giving sample names and column indicating group membership. Column indicating group membership should be named 'Group' and have exactly two unique values
#' @param data_scale character string giving scale the data is on. Current acceptable values are: "log", "log2", "log10", "abundance", "pres"
#' @param pres_thresh numeric value between 0 and 1, inclusive, indicating the minimum proportion of samples per group that are required for a peak to be considered present/observed in a group.
#' @param absn_thresh numeric value between 0 and 1, inclusive, indicating the maximum proportion of samples per group that can be observed for a peak to be considered absent in a group.
#' 
#' @name uniqueness_prop
#' @author Lisa Bramer
#' 

uniqueness_prop <- function(edata_df, group_df, data_scale, pres_thresh, absn_thresh){
  
  # check that edata_df is data.frame #
  if(!inherits(edata_df, "data.frame")) stop("edata_df must be of class 'data.frame'")
  
  # check that group_df is data.frame #
  if(!inherits(group_df, "data.frame")) stop("group_df must be of class 'data.frame'")
  
  # check that group_df has two columns and one is title 'Group' #
  if(ncol(group_df) != 2) stop("group_df should be a data.frame with two columns")
  if(!("Group" %in% names(group_df))) stop("group_df must have one column named 'Group' giving sample group membership")
  
  # check that there are exactly two group levels #
  if(length(unique(group_df$Group)) != 2) stop("Group column in group_df must contain exactly two group levels")
  
  # check that data_scale is valid #
  if(class(data_scale) != "character") stop("data_scale must be of class 'character'")
  if(!(data_scale %in% c("log", "log2", "log10", "abundance", "pres"))) stop("data_scale value is not an invalid option.")
  
  # determine which column gives the samples in group_df #
  samp_colname = names(group_df)[which(names(group_df) != "Group")]
  
  # get unique group levels #
  grps = as.character(unique(group_df$Group))
  
  # get sample names which belong to each group level #
  grp1_samps = group_df[which(group_df$Group == grps[1]), samp_colname]
  grp2_samps = group_df[which(group_df$Group == grps[2]), samp_colname]
  
  
  # ensure that pres_thresh and absn_thresh are between 0 and 1 #
  if(!(pres_thresh >= 0 & pres_thresh <= 1)) stop("pres_thresh must be a numeric value between 0 and 1 inclusive")  
  if(!(absn_thresh >= 0 & absn_thresh <= 1)) stop("absn_thresh must be a numeric value between 0 and 1 inclusive")  
  
  # check that absn_thresh is less than pres_thresh #
  if(!(absn_thresh < pres_thresh)) stop("absn_thresh must be less than pres_thresh")

  # if data is not in presence/absence form, convert to pres/abs (number or NA) #
  if(data_scale != "pres"){
    for(i in 1:ncol(edata_df)){
      edata_df[which(edata_df[,i] == 0), i] = NA
    }
  }
  
  # pull group membership column and ensure that order matches edata_df column order #
  ord_groupdf = group_df[match(names(edata_df), group_df[,samp_colname]),]
  
  
  
  # calculate the maximum (over the two groups) number of samples observed per group #
  prop_pres = data.frame(prop_grp1 = apply(!is.na(edata_df[,which(names(edata_df) %in% grp1_samps)]), 1, sum)/length(grp1_samps),
                       prop_grp2 = apply(!is.na(edata_df[,which(names(edata_df) %in% grp2_samps)]), 1, sum)/length(grp2_samps)
  )
  
  
  # construct results vector #
  res_vec = rep(NA, nrow(edata_df))
  res_vec[which(apply(prop_pres, 1, min) >= pres_thresh)] = "Observed in Both"
  res_vec[which(prop_pres$prop_grp1 >= pres_thresh & prop_pres$prop_grp2 <= absn_thresh)] = paste("Unique to", grps[1], sep = " ")
  res_vec[which(prop_pres$prop_grp1 <= absn_thresh & prop_pres$prop_grp2 >= pres_thresh)] = paste("Unique to", grps[2], sep = " ")
  
  data.frame(uniqueness_prop = res_vec)
}


#' Determine Unique and Common Masses Between Two Groups
#' 
#' Use a g-test to compare two groups and determine which peaks are uniquely expressed in each group based on a p-value threshold
#' 
#' @param edata_df data.frame giving samples expression profiles for samples belonging to one of two groups
#' @param group_df data.frame giving sample names and column indicating group membership. Column indicating group membership should be named 'Group' and have exactly two unique values
#' @param data_scale character string giving scale the data is on. Current acceptable values are: "log", "log2", "log10", "abundance", "pres"
#' @param pres_fn character string indicating the function to use when determining if a peak/compound is present for a group. Current options include "nsamps" and "prop".
#' @param pres_thresh numeric value indicating the threshold (e.g. number of samples, proportion of samples, etc.), inclusive, that are required for a peak to be considered present/observed in a group.
#' @param pvalue_thresh p-value threshold at which masses with a g-test p-value less than or equal to \code{pval_threshold} are considered to be unique to the group with more observed proportion/number of samples, which also exceeds \code{pres_thresh} 
#' 
#' @name uniqueness_gtest
#' @author Lisa Bramer
#' 

uniqueness_gtest <- function(edata_df, group_df, data_scale, pres_fn, pres_thresh, pvalue_thresh = 0.05){
  
  # check that edata_df is data.frame #
  if(!inherits(edata_df, "data.frame")) stop("edata_df must be of class 'data.frame'")
  
  # check that group_df is data.frame #
  if(!inherits(group_df, "data.frame")) stop("group_df must be of class 'data.frame'")
  
  # check that group_df has two columns and one is title 'Group' #
  if(ncol(group_df) != 2) stop("group_df should be a data.frame with two columns")
  if(!("Group" %in% names(group_df))) stop("group_df must have one column named 'Group' giving sample group membership")
  
  # check that data_scale is valid #
  if(class(data_scale) != "character") stop("data_scale must be of class 'character'")
  if(!(data_scale %in% c("log", "log2", "log10", "abundance", "pres"))) stop("data_scale value is not an invalid option.")
  
  # check that pres_fn is valid #
  if(!(pres_fn %in% c("nsamps", "prop"))) stop("Specified 'pres_fn' is not a valid option.")
  
  # determine which column gives the samples in group_df #
  samp_colname = names(group_df)[which(names(group_df) != "Group")]
  
  # get unique group levels #
  grps = as.character(unique(group_df$Group))
  
  # get sample names which belong to each group level #
  grp1_samps = group_df[which(group_df$Group == grps[1]), samp_colname]
  grp2_samps = group_df[which(group_df$Group == grps[2]), samp_colname]
  
  min_n_samps = min(c(length(grp1_samps), length(grp2_samps)))
  
  # ensure that pres_thresh is valid #
  if(pres_fn == "nsamps"){
    pres_thresh = as.integer(pres_thresh)
    if(!(pres_thresh >= 0 & pres_thresh <= min_n_samps)) stop("pres_thresh must be an integer between 1 and the minimum number of samples in a group")
  }
  if(pres_fn == "prop"){
  if(!(pres_thresh >= 0 & pres_thresh <= 1)) stop("pres_thresh must be a numeric value between 0 and 1 inclusive")  
  }
  
  # if data is not in presence/absence form, convert to pres/abs #
  if(data_scale != "pres"){
    for(i in 1:ncol(edata_df)){
      edata_df[which(edata_df[,i] == 0), i] = NA
    }
  }
  
  # pull group membership column and ensure that order matches edata_df column order #
  ord_groupdf = group_df[match(names(edata_df), group_df[,samp_colname]),]
  
  # run g-test on data #
  gtest_res = gtest.standard(data = as.matrix(edata_df), group = ord_groupdf$Group)
  
  # summarize presence by group #
  if(pres_fn == "nsamps"){
    numpres = data.frame(N_grp1 = apply(!is.na(edata_df[,which(names(edata_df) %in% grp1_samps)]), 1, sum),
                       N_grp2 = apply(!is.na(edata_df[,which(names(edata_df) %in% grp2_samps)]), 1, sum)
    )
  }
  if(pres_fn == "prop"){
    # calculate the maximum (over the two groups) number of samples observed per group #
    numpres = data.frame(N_grp1 = apply(!is.na(edata_df[,which(names(edata_df) %in% grp1_samps)]), 1, sum)/length(grp1_samps),
                         N_grp2 = apply(!is.na(edata_df[,which(names(edata_df) %in% grp2_samps)]), 1, sum)/length(grp2_samps)
    )
    
  }
  
  max_val_grp = apply(numpres, 1, max)
  min_val_grp = apply(numpres, 1, min)
  
  # construct results vector #
  res_vec = rep(NA, nrow(edata_df))
  res_vec[which(min_val_grp >= pres_thresh)] = "Observed in Both"
  res_vec[which(gtest_res$pvals <= pvalue_thresh & max_val_grp >= pres_thresh)] = paste("Unique to", gtest_res$major.group[which(gtest_res$pvals <= pvalue_thresh & max_val_grp >= pres_thresh)], sep = " ")
  
  data.frame(uniqueness_gtest = res_vec)
}


#' G-test function
#' 
#' G-test function
#' 
#' @param data p x n matrix, where n = # of observations and p = # of variables to be tested
#' @param group an n x 1 vector specifying group membership -- group names can be of type numeric or character
#' 
#' @name gtest.standard
#' @author Lisa Bramer
#' 

gtest.standard = function(data, group){
  
  ## store some basic summary statistics ##
  p = nrow(data)
  n = ncol(data)
  grps = as.factor(group)
  num.grps = length(levels(grps))
  
  ## basic tests before proceeding ##
  if(length(group)!=n)stop("Group vector length does not match number of rows in the data provided")
  if(num.grps < 2)stop("Must have at least two groups to conduct test")
  
  ## store the group names ##
  grp.names = levels(grps)
  
  grp.ns = NULL
  ## calculate group sample sizes ##
  for(i in 1:num.grps){
    curgrp = grp.names[i]
    curdat = grps[grps==curgrp]
    grp.ns[i] = length(curdat)
  }
  
  
  ## results placeholders ##
  Gstat = NULL
  pvalue = NULL
  major.group = NULL
  
  ## run the Gtest on each variable ##
  for(j in 1:p){
    
    ## pull data for current variable ##
    test.data = unlist(data[j,])
    obs.miss = matrix(0, nrow = 2, ncol = num.grps)
    
    ## calculate observed number of missing/non-missing data values per group ## 
    ## first row is the number of missing values, second row is the number of nonmissing values ##
    ## each column corresponds to a group ##
    for(i in 1:num.grps){
      cur.grp = grp.names[i]
      cur.dat = test.data[grps==cur.grp]
      obs.miss[,i] = c(sum(is.na(cur.dat)),sum(!is.na(cur.dat)))
    }
    
    ## summarize the total number of missing and non-missing values over all groups ##
    tot.miss  =  apply(obs.miss,1,sum)
    
    ## If all data is missing or all data is present, standard calculation will fail. ##
    ## In these cases, there is no evidence of a signficant difference, so we set the p-value to 1 ##
    if(min(tot.miss) > 0){
      exp.miss = matrix(0, nrow = 2, ncol = num.grps)
      ## calculate expected number of missing/non-missing values per group under the null hypothesis of no difference##
      ## expected value is the overall proportion of missing/non-missing times number of samples in the group ##
      for(i in 1:num.grps){
        exp.miss[,i] = (tot.miss/n)*grp.ns[i]
      }	
    }
    
    if(min(tot.miss) > 0){
      ## calculate preliminary value for each cell ##
      prelim.g = obs.miss*log(obs.miss/exp.miss)
      
      ## G-statistic sums over non-empty observed cells and then multiplies by 2 ##
      Gstat[j] = 2*sum(prelim.g[!is.na(prelim.g)])
      df = num.grps - 1
      pvalue[j] = pchisq(Gstat[j],df,lower.tail=FALSE)
      major.group[j] = ifelse(obs.miss[2,1] > obs.miss[2,2], grp.names[1], grp.names[2])
    }else{
      Gstat[j] = 0
      pvalue[j] = 1
      major.group[j] = "Common to Both"
    }
    
  }
  
  return(list(pvals = pvalue, major.group = major.group))
}


