#' Summary for mass filter
#'
#' @param fiter_obj massFilt object, created by \code{\link{mass_filter}} function
#' @param min_mass (optional) minimum mass
#' @param max_mass (optional) maximum mass
#' 
#' @export
summary.massFilt <- function(filter_obj, min_mass=NA, max_mass=NA) {
  if (identical(min_mass, NA) & identical(max_mass, NA)) {
    return(summary.data.frame(filter_obj))
  }
  if (identical(min_mass, NA)) min_mass <- min(filter_obj$Mass, na.rm=TRUE)
  if (identical(max_mass, NA)) max_mass <- max(filter_obj$Mass, na.rm=TRUE)
  
  dat_filt <- dplyr::filter(filter_obj, Mass >= min_mass & Mass <= max_mass)
  return(list(Original_Num_Peaks=nrow(filter_obj), Removed_Peaks=nrow(filter_obj) - nrow(dat_filt), 
       Retained_Peaks=nrow(dat_filt)))
}


#' Summary for molecule filter
#'
#' @param fiter_obj moleculeFilt object, created by \code{\link{molecule_filter}} function
#' @param min_num (optional) minimum number of observations
#' 
#' @export
summary.moleculeFilt <- function(filter_obj, min_num=NA) {
  if (identical(min_num, NA)) {
    return(summary.data.frame(filter_obj))
  }

  dat_filt <- dplyr::filter(filter_obj, Num_Observations >= min_num)
  return(list(Original_Num_Peaks=nrow(filter_obj), Removed_Peaks=nrow(filter_obj) - nrow(dat_filt), 
              Retained_Peaks=nrow(dat_filt)))
}


#' Summary for formula filter
#'
#' @param fiter_obj formulaFilt object, created by \code{\link{formula_filter}} function
#' @param remove (optional) 'NoFormula' or 'Formula'
#' 
#' @export
summary.formulaFilt <- function(filter_obj, remove=NA) {
  if (identical(remove, NA)) {
    return(summary.data.frame(filter_obj))
  }
  
  formula_tab <- table(filter_obj$Formula_Assigned)
  if (remove == 'NoFormula') removed <- formula_tab['FALSE']
  else if (remove == 'Formula') removed <- formula_tab['TRUE']
  else stop("Invalid value for remove, must be 'NoFormula' or 'Formula'")
  return(list(Original_Num_Peaks=nrow(filter_obj), Removed_Peaks=as.vector(removed), 
              Retained_Peaks=as.vector(nrow(filter_obj)-removed)))
}

