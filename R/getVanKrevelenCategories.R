#' Get Van Krevelen categories for icrData
#' 
#' Constructs a vector of Van Krevelen categories corresponding
#' to the rows in the input icrData object, whether or not
#' they are observed (i.e. have any positive values in e_data)
#' in this data or subset.
#' 
#' @param data.obj icrData object
#' 
getVanKrevelenCategories <- function(data.obj, boundary_set) {
  
  # Test inputs
  if (!inherits(data.obj, "icrData")) {
    stop("data.obj must be of type icrData")
  }
  if (is.null(data.obj$e_meta)) {
    stop("data.obj must have e_meta element")
  }

  OC.col <- getOCRatioColName(data.obj)
  if (is.null(OC.col) | !is.element(OC.col, colnames(data.obj$e_meta))) {
    stop("O:C ratio column attribute is not set or is not present in data.obj$e_meta")
  }
  HC.col <- getHCRatioColName(data.obj )
  if (is.null(HC.col) | !is.element(HC.col, colnames(data.obj$e_meta))) {
    stop("H:C ratio column attribute is not set or is not present in data.obj$e_meta")
  }
  
  emeta.df <- data.obj$e_meta

  vkBounds <- getVanKrevelenCategoryBounds(boundary_set = boundary_set)
  
  # Assign each row of e_meta to a category
  vk.class = rep("Other", nrow(data.obj$e_meta))

  for(i in 1:nrow(vkBounds)){
    vk.class[which(
      data.obj$e_meta[, HC.col] >= vkBounds$HC.low[i] & 
        data.obj$e_meta[, HC.col] <= vkBounds$HC.high[i] & 
        data.obj$e_meta[, OC.col] >= vkBounds$OC.low[i] & 
        data.obj$e_meta[, OC.col] <= vkBounds$OC.high[i])] = rownames(vkBounds)[i]
  }

  return(vk.class)
}

#' Get *observed* Van Krevelen categories for icrData
#' 
#' Constructs a vector of Van Krevelen categories corresponding
#' to the rows in the input icrData object. If a peak is not
#' observed (i.e. have any positive values in e_data) in this
#' data object, the corresponding value in the returned vector
#' is NA. Otherwise the returned value is a string name.
#' 
#' @param data.obj icrData object
#' 
getObservedVanKrevelenCategories <- function(data.obj) {

    vk.cat <- getVanKrevelenCategories(data.obj)

    if (is.null(data.obj$e_data)) {
        stop("data.obj must have e_data element")
    }
    
    # which peaks are observed in this dataset?
    obs.peaks <- rowSums(data.obj$e_data)-data.obj$e_data[,getEDataColName(data.obj)] > 0
    if (any(!obs.peaks)) {
        vk.cat[!obs.peaks] <- NA
    }
    return(vk.cat)
}

# Returns a data.frame of Van Krevelen category bounds (HC and OC lower 
# and upper limits) for each category.
# Internal only
getVanKrevelenCategoryBounds <- function(boundary_set) {
  
  if(!(boundary_set %in% c("bs1", "bs2"))) stop("Invalid option provided for boundary_set argument.")
  
  if(boundary_set == "bs1"){
  HC.low = c(1.55,0.7,1.45,0.81,1.48,1.34,0.7,0.3, 0)
  HC.high = c(2.25, 1.5,2,1.45,2.15,1.8,1.3,0.81, Inf)
  OC.low = c(0,0.05,0.3,0.28,0.68,0.54,0.65,0.12, 0)
  OC.high = c(0.3,0.15,0.55,0.65,1,0.71,1.05,0.7, Inf)
  }
  if(boundary_set == "bs2"){
  HC.low = c(1.5, 0.8, 1.5, 0.8, 1.5, 1.5, 0.8, 0.2, 0)
  HC.high = c(2.5, 1.5, 2.3, 1.5, 2.5, 2.2, 1.5, 0.8, Inf)
  OC.low = c(0, 0, 0.3, 0.125, 0.7, 0.55, 0.65, 0, 0)
  OC.high = c(0.3, 0.125, 0.55, 0.65, 1.5, 0.7, 1, 0.95, Inf)
  }
  category = c("Lipid","Unsat Hydrocarbons", "Proteins","Lignin","Carbohydrate","Amino Sugar","Tannin","Cond Hydrocarbons","Other")
  vanKrevlenCategoryBounds <- data.frame(HC.low, HC.high, OC.low, OC.high)
  rownames(vanKrevlenCategoryBounds) <- category
  return(vanKrevlenCategoryBounds)
}


# Returns a data.frame in a convenient format for plotting the boundaries of
# the Van Krevelen categories.
# Internal only
getVanKrevelenPlottingDF <- function(boundary_set) {
  vkBounds <- getVanKrevelenCategoryBounds(boundary_set = boundary_set)
  # data.frame for plotting purposes
  vankrev_categories <- data.frame(y0 = c(vkBounds$HC.low, vkBounds$HC.high, vkBounds$HC.low, vkBounds$HC.low), 
                                   y1 = c(vkBounds$HC.low, vkBounds$HC.high, vkBounds$HC.high, vkBounds$HC.high), 
                                   x0 = c(vkBounds$OC.low, vkBounds$OC.low, vkBounds$OC.low, vkBounds$OC.high), 
                                   x1 = c(vkBounds$OC.high, vkBounds$OC.high, vkBounds$OC.low, vkBounds$OC.high), 
                                   category = rep(rownames(vkBounds), 4))
  return(vankrev_categories)
}
