#' Calculate and Count Observed Mass Transformations
#' 
#' 
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData', typically a result of \code{\link{as.peakIcrData}} or \code{\link{mapPeaksToCompounds}}.
#' @param transformDF a data.frame of known transformations of interest, with a minimum of two columns (in any order): column 'transMass_cname' gives the transformation masses which should be counted and column 'transID_cname' gives a unique identifier (usually character string) for each transformation mass
#' @param transformDigits the number of decimal places that should be retained for the purposes of matching observed transformations to transformations in \code{transformDF}. Defaults to 4
#' @param transMass_cname a character string giving the name of the column in \code{transformDF} which gives the mass of the transformations of interest
#' @param transID_cname a character string giving the name of the column in \code{transformDF} which gives a unique identifier for each transformation of interest
#' @param transOther_cname a character vector specifying any other columns in \code{transformDF} which should be returned with the transformation counts. This is only applicable if \code{transformDF} has more than 2 columns with information that should be carried through to results. Defaults to NULL
#' 
#' @return a data.frame with counts of the the number of times that each transformation in \code{transformDF} was observed in each sample
#' 
#' @author Lisa Bramer
#' @export

transformation_counts <- function(icrData, transformDF, transformDigits = 4, transMass_cname, transID_cname, transOther_cname = NULL){
  
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakIcrData") & !inherits(icrData, "compoundIcrData")) stop("icrData must be an object of class 'peakIcrData' or 'compoundIcrData'")
  
  # checks to column names #
  if(class(transMass_cname) != "character" | length(transMass_cname) != 1) stop("'transMass_cname' must be a character string")
  if(class(transID_cname) != "character" | length(transID_cname) != 1) stop("'transID_cname' must be a character string")
  if(!is.null(transOther_cname)){
    if(class(transOther_cname) != "character") stop("'transOther_cname' must be a character vector")
  }
  
  # checks on transformDF and column names in transformDF #
  if(class(transformDF) != "data.frame") stop("'transformDF' must be of class data.frame")
  if(ncol(transformDF) < 2) stop("'transformDF' must have two or more columns")
  if(!(transMass_cname %in% names(transformDF))) stop("'transMass_cname' not found in 'transformDF'") 
  if(!(transID_cname %in% names(transformDF))) stop("'transID_cname' not found in 'transformDF'") 
  if(!is.null(transOther_cname)){
    if(sum(transOther_cname %in% names(transformDF)) != length(transOther_cname)) stop("One or more of 'transOther_cname' entries are not found in 'transformDF'")
  }
  
  # check that transformDigits is an integer > 0 #
  if(transformDigits < 1) stop("'transformDigits' must be an integer greater than 1")
  
  # if the data is not p/a, then convert to p/a #
  if(getDataScale(icrData) != "pres"){
    temp = edata_transform(icrData, "pres")
  }else{
    temp = icrData
  }

  # pull edata and mass cnames #
  edata_id = getEDataColName(icrData)
  mass_id = getMassColName(icrData)
  
  # if mass information is in edata then we can just use edata #
  if(edata_id == mass_id){
    data = temp$e_data
  }else{ # otherwise we need to merge edata and emeta #
    data = merge(temp$e_meta[,c(edata_id, mass_id)], temp$e_data, by = edata_id)[,-(edata_id)]
  }
  
  # set a local dopar argument #
  `%dopar%` <- foreach::`%dopar%`
  
  # prepare transformation data.frame info #
  transformDF[,transMass_cname] = round(transformDF[,transMass_cname], transformDigits)
  
  # setup parallelization variables #
  num_cores = parallel::detectCores()
  
  cl = parallel::makeCluster(num_cores - 1)
  doParallel::registerDoParallel(cl)
  
  # helper function that counts occurences #
  f6 = function(x) {
    data.table::data.table(x)[, .N, keyby = x]
  }
  
  # produce vector of which columns are not mass_id #
  col_ids = which(names(data) != mass_id)
  
  # if there are no extra columns in transformDF #
  if(is.null(transOther_cname)){
    # produce counts of transformations for each sample #
    mass_diffs = foreach::foreach(i = col_ids, .packages = c("data.table")) %dopar% {	
      temp = data[which(data[,i] == 1), mass_id]
      temp_dists = round(c(dist(temp)),transformDigits)
      dists_counts = f6(temp_dists)
      trans_counts = merge(x = data.table::data.table(transformDF), y = dists_counts, by.x = transMass_cname, by.y = "x", all.x = T, all.y = F)
      trans_counts[,(transID_cname):=NULL]
    }
    comp_res = data.frame(mass_diffs[[1]][,transMass_cname, with = F], transformDF[,transID_cname],do.call(cbind, lapply(mass_diffs, function(x) x[,!(transMass_cname), with = F])))
    names(comp_res) = c(transMass_cname, transID_cname, names(data)[-1])
  }else{
    mass_diffs = foreach::foreach(i = col_ids, .packages = c("data.table")) %dopar% {	
      temp = data[which(data[,i] == 1), mass_id]
      temp_dists = round(c(dist(temp)),transformDigits)
      dists_counts = f6(temp_dists)
      trans_counts = merge(x = data.table::data.table(transformDF[,c(transMass_cname, transID_cname)]), y = dists_counts, by.x = transMass_cname, by.y = "x", all.x = T, all.y = F)
      trans_counts[,(transID_cname):=NULL]
    }
    comp_res = data.frame(mass_diffs[[1]][,transMass_cname, with = F], transformDF[,c(transID_cname, transOther_cname)], do.call(cbind, lapply(mass_diffs, function(x) x[,!(transMass_cname), with = F])))
    names(comp_res) = c(transMass_cname, transID_cname, transOther_cname, names(data)[-1])
  }  
  
  # stop the created cluster #
  parallel::stopCluster(cl)
  
  comp_res[is.na(comp_res)] = 0

  return(comp_res)
}



