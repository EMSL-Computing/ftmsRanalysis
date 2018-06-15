#' Determine Unique and Common Masses Between Two Groups
#' 
#' Use a g-test to compare two groups and determine which peaks are uniquely expressed in each group based on a p-value threshold
#' 
#' @param edata_df data.frame giving samples expression profiles for samples belonging to one of two groups
#' @param group_df data.frame giving sample names and column indicating group membership. Column indicating group membership should be named 'Group' and have exactly two unique values
#' @param data_scale character string indicating the scale of the data. Acceptable values are 'log', log2', 'log10', 'abundance', and 'pres'
#' @param pval_threshold p-value threshold at which masses with a g-test p-value less than \code{pval_threshold} are considered to be unique to the group with more samples having a non-zero observed value for that mass. Masses with a g-test p-value greater than the threshold are not considered to be unique to either group
#' 
#' @name uniqueness_gtest
#' @author Lisa Bramer
#' 

uniqueness_gtest <- function(edata_df, group_df, data_scale, pval_threshold = 0.05){
  
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
  
  # determine which column gives the samples in group_df #
  samp_colname = names(group_df)[which(names(group_df) != "Group")]
  
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

  # construct results vector #
  gtest_vec = rep("Common to Both", nrow(edata_df))
  gtest_vec[which(gtest_res$pvals <= pval_threshold)] = paste("Unique to", gtest_res$major.group[which(gtest_res$pvals <= pval_threshold)], sep = " ")
  
  data.frame(unique_gtest = gtest_vec)
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



#' @title Group comparison summary functions
#' @description \code{getGroupComparisonSummaryFunctionNames} returns the names of valid group comparison summary
#' functions that may be used with the \code{\link{summarizeGroupComparisons}} function.
# @export
getGroupComparisonSummaryFunctionNames <- function() {
  return(c("uniqueness_gtest"))
}
