## A set of convenience methods for getting and setting the attributes that store
## the column names for particular information.

#' Get the name of the e_data unique identifier column
#' 
#' Returns the name of the column in the e_data element that 
#' is the unique identifier of each row.
#'
#' @param icrDataObj an object of type icrData
#' @return name of unique ID column 
#' @export
getEDataColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$edata_cname)
}

#' Set the name of the e_data unique identifier column
#' 
#' Sets the name of the column in the e_data element that 
#' is the unique identifier of each row.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setEDataColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  
  attr(icrDataObj, "cnames")$edata_cname <- cname
  return(icrDataObj)
}

#' Get the name of the f_data unique identifier column
#' 
#' Returns the name of the column in the f_data element that 
#' is the unique identifier of each row.
#'
#' @param icrDataObj an object of type icrData
#' @return name of unique ID column 
#' @export
getFDataColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$fdata_cname)
}

#' Set the name of the f_data unique identifier column
#' 
#' Sets the name of the column in the f_data element that 
#' is the unique identifier of each row.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setFDataColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$fdata_cname <- cname
  return(icrDataObj)
}

#' Get the name of the e_meta unique identifier column
#' 
#' Returns the name of the column in the e_meta element that 
#' is the unique identifier of each row.
#'
#' @param icrDataObj an object of type icrData
#' @return name of unique ID column 
#' @export
getMFColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$mf_cname)
}

#' Set the name of the e_meta unique identifier column
#' 
#' Sets the name of the column in the e_meta element that 
#' is the unique identifier of each row.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setMFColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$mf_cname <- cname
  return(icrDataObj)
}


#' Get the name of the O:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains O:C ratio data for Van Krevelen plots.
#'
#' @param icrDataObj an object of type icrData
#' @return name of O:C ratio column
#' @export
getOCRatioColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$o2c_cname)
}

#' Set the name of the O:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains O:C ratio data for Van Krevelen plots.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setOCRatioColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$o2c_cname <- cname
  return(icrDataObj)
}


#' Get the name of the H:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains H:C ratio data for Van Kevelen plots.
#'
#' @param icrDataObj an object of type icrData
#' @return name of H:C ratio column
#' @export
getHCRatioColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$h2c_cname)
}

#' Set the name of the H:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains H:C ratio data for Van Krevelen plots.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setHCRatioColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$h2c_cname <- cname
  return(icrDataObj)
}

#' Get the name of the mass column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains mass data for calculations.
#'
#' @param icrDataObj an object of type icrData
#' @return name of mass column
#' @export
getMassColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$mass_cname)
}

#' Set the name of the mass column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains mass data.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setMassColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$mass_cname <- cname
  return(icrDataObj)
}

#' Get the name of the carbon column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains carbon count data for calculations.
#'
#' @param icrDataObj an object of type icrData
#' @return name of carbon column
#' @export
getCarbonColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$c_cname)
}

#' Set the name of the carbon column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains carbon count data.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setCarbonColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$c_cname <- cname
  return(icrDataObj)
}

#' Get the name of the hydrogen column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains hydrogen count data for calculations.
#'
#' @param icrDataObj an object of type icrData
#' @return name of hydrogen column
#' @export
getHydrogenColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$h_cname)
}

#' Set the name of the hydrogen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains hydrogen count data.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setHydrogenColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$h_cname <- cname
  return(icrDataObj)
}

#' Get the name of the oxygen column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains oxygen count data for calculations.
#'
#' @param icrDataObj an object of type icrData
#' @return name of oxygen column
#' @export
getOxygenColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$o_cname)
}

#' Set the name of the oxygen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains oxygen count data.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setOxygenColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$o_cname <- cname
  return(icrDataObj)
}

#' Get the name of the nitrogen column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains nitrogen count data for calculations.
#'
#' @param icrDataObj an object of type icrData
#' @return name of nitrogen column
#' @export
getNitrogenColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$n_cname)
}

#' Set the name of the nitrogen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains nitrogen count data.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setNitrogenColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$n_cname <- cname
  return(icrDataObj)
}

#' Get the name of the sulfur column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains sulfur count data for calculations.
#'
#' @param icrDataObj an object of type icrData
#' @return name of sulfur column
#' @export
getSulfurColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$s_cname)
}

#' Set the name of the sulfur column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains sulfur count data.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setSulfurColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$s_cname <- cname
  return(icrDataObj)
}

#' Get the name of the phosphorus column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains phosphorus count data for calculations.
#'
#' @param icrDataObj an object of type icrData
#' @return name of phosphorus column
#' @export
getPhosphorusColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$p_cname)
}

#' Set the name of the phosphorus column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains phosphorus count data.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setPhosphorusColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$p_cname <- cname
  return(icrDataObj)
}

#' Get the name of the carbon13 column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains carbon13 count data for calculations.
#'
#' @param icrDataObj an object of type icrData
#' @return name of carbon13 column
#' @export
getC13ColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$c13_cname)
}

#' Set the name of the carbon13 column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains carbon13 count data.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setC13ColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$c13_cname <- cname
  return(icrDataObj)
}

#' Get the name of the Kendrick mass column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains Kendrick mass data for Kendrick plots.
#'
#' @param icrDataObj an object of type icrData
#' @return name of Kendrick mass column
#' @export
getKendrickMassColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$kmass_cname)
}

#' Set the name of the Kendrick mass column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains Kendrick mass data for Kendrick plots.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setKendrickMassColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$kmass_cname <- cname
  return(icrDataObj)
}

#' Get the name of the Kendrick defect column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains Kendrick defect data for Kendrick plots.
#'
#' @param icrDataObj an object of type icrData
#' @return name of Kendrick defect column
#' @export
getKendrickDefectColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$kdefect_cname)
}

#' Set the name of the Kendrick defect column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains Kendrick defect data for Kendrick plots.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setKendrickDefectColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$kdefect_cname <- cname
  return(icrDataObj)
}


#' Get the name of the extraction column
#' 
#' Returns the name of the column in the f_data element that 
#' contains extraction information.
#'
#' @param icrDataObj an object of type icrData
#' @return name of extraction column
#' @export
getExtractionColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$extraction_cname)
}

#' Set the name of the extraction column
#' 
#' Sets the name of the column in the f_data element that 
#' contains extraction information.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setExtractionColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$extraction_cname <- cname
  return(icrDataObj)
}


#' Get the name of the NOSC column
#' 
#' Gets the name of the column in the e\_meta element that contains NOSC values
#' 
#' @param icrDataObj an object of type icrData
#' @return name of NOSC column
#' @export
getNOSCColName <- function(icrDataObj){
  if(class(icrDataObj) != "icrData") stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "cnames")$nosc_cname)
}

#' Set the name of the NOSC column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains NOSC information.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setNOSCColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$nosc_cname <- cname
  return(icrDataObj)
}

#' Get the name of the mf name/description column
#' 
#' Gets the name of the column in the e\_meta element that contains the name/description when a molecular formula is assigned
#' 
#' @param icrDataObj an object of type icrData
#' @return name of column
#' @export
getMFNameColName <- function(icrDataObj){
  if(class(icrDataObj) != "icrData") stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "cnames")$mfname_cname)
}

#' Set the name of the mf name/description column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains name/description when a molecular formula is assigned 
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setMFNameColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$mfname_cname <- cname
  return(icrDataObj)
}

#' Get the name of the aromaticity column
#' 
#' Gets the name of the column in the e\_meta element that contains aromaticity values
#' 
#' @param icrDataObj an object of type icrData
#' @return name of aromaticity column
#' @export
getAromaColName <- function(icrDataObj){
  if(class(icrDataObj) != "icrData") stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "cnames")$aroma_cname)
}

#' Set the name of the aromaticity column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains aromaticity information.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setAromaColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$aroma_cname <- cname
  return(icrDataObj)
}

#' Get the name of the modified aromaticity column
#' 
#' Gets the name of the column in the e\_meta element that contains modified aromaticity values
#' 
#' @param icrDataObj an object of type icrData
#' @return name of modified aromaticity column
#' @export
getModAromaColName <- function(icrDataObj){
  if(class(icrDataObj) != "icrData") stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "cnames")$modaroma_cname)
}

#' Set the name of the modified aromaticity column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains modified aromaticity information.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setModAromaColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$modaroma_cname <- cname
  return(icrDataObj)
}

#' Get the name of the gibbs free energy column
#' 
#' Gets the name of the column in the e\_meta element that contains gibbs free energy values
#' 
#' @param icrDataObj an object of type icrData
#' @return name of gibbs free energy column
#' @export
getGibbsColName <- function(icrDataObj){
  if(class(icrDataObj) != "icrData") stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "cnames")$gfe_cname)
}

#' Set the name of the gibbs free energy column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains gibbs free energy information.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#'
#' 
setGibbsColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$gfe_cname <- cname
  return(icrDataObj)
}

#' Get the name of the double-bond equivalent column
#' 
#' Gets the name of the column in the e\_meta element that contains double-bond equivalent values
#' 
#' @param icrDataObj an object of type icrData
#' @return name of double-bond equivalent column
#' @export
getDBEColName <- function(icrDataObj){
  if(class(icrDataObj) != "icrData") stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "cnames")$dbe_cname)
}

#' Set the name of the double-bond equivalent column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains double-bond equivalent information.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setDBEColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$dbe_cname <- cname
  return(icrDataObj)
}

#' Get the name of the double-bond equivalent minus oxygen column
#' 
#' Gets the name of the column in the e\_meta element that contains double-bond equivalent values
#' 
#' @param icrDataObj an object of type icrData
#' @return name of double-bond equivalent column
#' @export
getDBEoColName <- function(icrDataObj){
  if(class(icrDataObj) != "icrData") stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "cnames")$dbeo_cname)
}

#' Set the name of the double-bond equivalent minus oxygen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains double-bond equivalent information.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setDBEoColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$dbeo_cname <- cname
  return(icrDataObj)
}

#' Get the name of the elemental composition column
#' 
#' Gets the name of the column in the e\_meta element that contains elemental composition
#' 
#' @param icrDataObj an object of type icrData
#' @return name of elemental composition column
#' @export
getElCompColName <- function(icrDataObj){
  if(class(icrDataObj) != "icrData") stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "cnames")$elcomp_cname)
}

#' Set the name of the elemental composition column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains elemental composition information.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setElCompColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$elcomp_cname <- cname
  return(icrDataObj)
}

#' Get the name of the peak class column
#' 
#' Gets the name of the column in the e\_meta element that contains peak class information
#' 
#' @param icrDataObj an object of type icrData
#' @return name of peak class column
#' @export
getClassColName <- function(icrDataObj){
  if(class(icrDataObj) != "icrData") stop("icrDataObj must be of type icrData")
  return(attr(icrDataObj, "cnames")$class_cname)
}

#' Set the name of the peak class column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains peak class information.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setClassColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$class_cname <- cname
  return(icrDataObj)
}

#' Get the name of the compound column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains compound IDs.
#'
#' @param icrDataObj an object of type icrData
#' @return name of mass column
#' @export
getCompoundColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$compound_cname)
}

#' Set the name of the compound column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains compound IDs.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setCompoundColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$compound_cname <- cname
  return(icrDataObj)
}

#' Get the name of the reaction column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains reaction IDs.
#'
#' @param icrDataObj an object of type icrData
#' @return name of mass column
#' @export
getReactionColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$reaction_cname)
}

#' Set the name of the reaction column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains reaction IDs.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setReactionColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% icrDataObj$e_meta)) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$reaction_cname <- cname
  return(icrDataObj)
}

#' Get the name of the module column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains module IDs.
#'
#' @param icrDataObj an object of type icrData
#' @return name of mass column
#' @export
getModuleColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$module_cname)
}

#' Set the name of the module column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains module IDs.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setModuleColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$module_cname <- cname
  return(icrDataObj)
}

#' Get the name of the module column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains module node IDs. A module node corresponds
#' to one or more reactions that make up a single 
#' graph node in the module graph.
#'
#' @param icrDataObj an object of type icrData
#' @return name of mass column
#' @export
getModuleNodeColName <- function(icrDataObj) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  return(attr(icrDataObj, "cnames")$module_node_cname)
}

#' Set the name of the module node column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains module node IDs. A module node corresponds
#' to one or more reactions that make up a single 
#' graph node in the module graph.
#'
#' @param icrDataObj an object of type icrData
#' @param cname column name
#' @return updated icrDataObj
#' 
#' 
setModuleNodeColName <- function(icrDataObj, cname) {
  if (!inherits(icrDataObj, "icrData")) {
    stop("icrDataObj must be of type icrData")
  } 
  if (!(cname %in% names(icrDataObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrDataObj, "cnames")$module_node_cname <- cname
  return(icrDataObj)
}



############# TESTS ################

# data(exIcrData)
# 
# getEDataColName(exIcrData) == "peak"
# getFDataColName(exIcrData) == "Sample"
# getEMetaColName(exIcrData) == "mf"
# getOCRatioColName(exIcrData) == "O/C"
# getHCRatioColName(exIcrData) == "H/C"
# getKendrickMassColName(exIcrData) == "kendrick_m"
# getKendrickDefectColName(exIcrData) == "kendrick_defect"
# getExtractionColName(exIcrData) == "Extraction"
# 
# getEDataColName(setEDataColName(exIcrData, "test")) == "test"
# getFDataColName(setFDataColName(exIcrData, "test")) == "test"
# getEMetaColName(setEMetaColName(exIcrData, "test")) == "test"
# getOCRatioColName(setOCRatioColName(exIcrData, "test")) == "test"
# getHCRatioColName(setHCRatioColName(exIcrData, "test")) == "test"
# getKendrickMassColName(setKendrickMassColName(exIcrData, "test")) == "test"
# getKendrickDefectColName(setKendrickDefectColName(exIcrData, "test")) == "test"
# getExtractionColName(setExtractionColName(exIcrData, "test")) == "test"
# 
# attr(exIcrData, "cnames")<- NULL
# getEDataColName(setEDataColName(exIcrData, "test")) == "test"
# getFDataColName(setFDataColName(exIcrData, "test")) == "test"
# getEMetaColName(setEMetaColName(exIcrData, "test")) == "test"
# getOCRatioColName(setOCRatioColName(exIcrData, "test")) == "test"
# getHCRatioColName(setHCRatioColName(exIcrData, "test")) == "test"
# getKendrickMassColName(setKendrickMassColName(exIcrData, "test")) == "test"
# getKendrickDefectColName(setKendrickDefectColName(exIcrData, "test")) == "test"
# getExtractionColName(setExtractionColName(exIcrData, "test")) == "test"


