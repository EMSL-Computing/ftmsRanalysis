## A set of convenience methods for getting and setting the attributes that store
## the column names for particular information.

#' Get the name of the e_data unique identifier column
#' 
#' Returns the name of the column in the e_data element that 
#' is the unique identifier of each row.
#'
#' @param icrData an object of type icrData
#' @return name of unique ID column 
#' @export
getEDataColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$edata_cname)
}

#' Set the name of the e_data unique identifier column
#' 
#' Sets the name of the column in the e_data element that 
#' is the unique identifier of each row.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setEDataColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  
  attr(icrData, "cnames")$edata_cname <- cname
  return(icrData)
}

#' Get the name of the f_data unique identifier column
#' 
#' Returns the name of the column in the f_data element that 
#' is the unique identifier of each row.
#'
#' @param icrData an object of type icrData
#' @return name of unique ID column 
#' @export
getFDataColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$fdata_cname)
}

#' Set the name of the f_data unique identifier column
#' 
#' Sets the name of the column in the f_data element that 
#' is the unique identifier of each row.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setFDataColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$fdata_cname <- cname
  return(icrData)
}

#' Get the name of the e_meta unique identifier column
#' 
#' Returns the name of the column in the e_meta element that 
#' is the unique identifier of each row.
#'
#' @param icrData an object of type icrData
#' @return name of unique ID column 
#' @export
getMFColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$mf_cname)
}

#' Set the name of the e_meta unique identifier column
#' 
#' Sets the name of the column in the e_meta element that 
#' is the unique identifier of each row.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setMFColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$mf_cname <- cname
  return(icrData)
}


#' Get the name of the O:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains O:C ratio data for Van Krevelen plots.
#'
#' @param icrData an object of type icrData
#' @return name of O:C ratio column
#' @export
getOCRatioColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$o2c_cname)
}

#' Set the name of the O:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains O:C ratio data for Van Krevelen plots.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setOCRatioColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$o2c_cname <- cname
  return(icrData)
}


#' Get the name of the H:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains H:C ratio data for Van Kevelen plots.
#'
#' @param icrData an object of type icrData
#' @return name of H:C ratio column
#' @export
getHCRatioColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$h2c_cname)
}

#' Set the name of the H:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains H:C ratio data for Van Krevelen plots.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setHCRatioColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$h2c_cname <- cname
  return(icrData)
}

#' Get the name of the N:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains N:C ratio data 
#'
#' @param icrData an object of type icrData
#' @return name of N:C ratio column
#' @export
getNCRatioColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$n2c_cname)
}

#' Set the name of the N:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains N:C ratio data.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setNCRatioColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$n2c_cname <- cname
  return(icrData)
}

#' Get the name of the P:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains P:C ratio data.
#'
#' @param icrData an object of type icrData
#' @return name of O:C ratio column
#' @export
getPCRatioColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$p2c_cname)
}

#' Set the name of the P:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains P:C ratio data.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setPCRatioColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$p2c_cname <- cname
  return(icrData)
}

#' Get the name of the N:P ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains N:P ratio data for Van Krevelen plots.
#'
#' @param icrData an object of type icrData
#' @return name of N:P ratio column
#' @export
getNPRatioColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$n2p_cname)
}

#' Set the name of the N:P ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains N:P ratio data for Van Krevelen plots.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setNPRatioColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$n2p_cname <- cname
  return(icrData)
}

#' Get the name of the mass column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains mass data for calculations.
#'
#' @param icrData an object of type icrData
#' @return name of mass column
#' @export
getMassColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$mass_cname)
}

#' Set the name of the mass column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains mass data.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setMassColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$mass_cname <- cname
  return(icrData)
}

#' Get the name of the carbon column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains carbon count data for calculations.
#'
#' @param icrData an object of type icrData
#' @return name of carbon column
#' @export
getCarbonColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$c_cname)
}

#' Set the name of the carbon column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains carbon count data.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setCarbonColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$c_cname <- cname
  return(icrData)
}

#' Get the name of the hydrogen column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains hydrogen count data for calculations.
#'
#' @param icrData an object of type icrData
#' @return name of hydrogen column
#' @export
getHydrogenColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$h_cname)
}

#' Set the name of the hydrogen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains hydrogen count data.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setHydrogenColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$h_cname <- cname
  return(icrData)
}

#' Get the name of the oxygen column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains oxygen count data for calculations.
#'
#' @param icrData an object of type icrData
#' @return name of oxygen column
#' @export
getOxygenColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$o_cname)
}

#' Set the name of the oxygen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains oxygen count data.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setOxygenColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$o_cname <- cname
  return(icrData)
}

#' Get the name of the nitrogen column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains nitrogen count data for calculations.
#'
#' @param icrData an object of type icrData
#' @return name of nitrogen column
#' @export
getNitrogenColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$n_cname)
}

#' Set the name of the nitrogen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains nitrogen count data.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setNitrogenColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$n_cname <- cname
  return(icrData)
}

#' Get the name of the sulfur column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains sulfur count data for calculations.
#'
#' @param icrData an object of type icrData
#' @return name of sulfur column
#' @export
getSulfurColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$s_cname)
}

#' Set the name of the sulfur column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains sulfur count data.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setSulfurColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$s_cname <- cname
  return(icrData)
}

#' Get the name of the phosphorus column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains phosphorus count data for calculations.
#'
#' @param icrData an object of type icrData
#' @return name of phosphorus column
#' @export
getPhosphorusColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$p_cname)
}

#' Set the name of the phosphorus column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains phosphorus count data.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setPhosphorusColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$p_cname <- cname
  return(icrData)
}

#' Get the name of the isotopic column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains isotopic peak information.
#'
#' @param icrData an object of type icrData
#' @return name of isotopic column
#' @export
getIsotopicColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$isotopic_cname)
}

#' Set the name of the isotopic column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains isotopic peak information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setIsotopicColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$isotopic_cname <- cname
  return(icrData)
}

#' Get the name of the Kendrick mass column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains Kendrick mass data for Kendrick plots.
#'
#' @param icrData an object of type icrData
#' @return name of Kendrick mass column
#' @export
getKendrickMassColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$kmass_cname)
}

#' Set the name of the Kendrick mass column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains Kendrick mass data for Kendrick plots.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setKendrickMassColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$kmass_cname <- cname
  return(icrData)
}

#' Get the name of the Kendrick defect column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains Kendrick defect data for Kendrick plots.
#'
#' @param icrData an object of type icrData
#' @return name of Kendrick defect column
#' @export
getKendrickDefectColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$kdefect_cname)
}

#' Set the name of the Kendrick defect column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains Kendrick defect data for Kendrick plots.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setKendrickDefectColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$kdefect_cname <- cname
  return(icrData)
}


#' Get the name of the extraction column
#' 
#' Returns the name of the column in the f_data element that 
#' contains extraction information.
#'
#' @param icrData an object of type icrData
#' @return name of extraction column
#' @export
getExtractionColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$extraction_cname)
}

#' Set the name of the extraction column
#' 
#' Sets the name of the column in the f_data element that 
#' contains extraction information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setExtractionColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$extraction_cname <- cname
  return(icrData)
}


#' Get the name of the NOSC column
#' 
#' Gets the name of the column in the e\_meta element that contains NOSC values
#' 
#' @param icrData an object of type icrData
#' @return name of NOSC column
#' @export
getNOSCColName <- function(icrData){
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$nosc_cname)
}

#' Set the name of the NOSC column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains NOSC information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setNOSCColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$nosc_cname <- cname
  return(icrData)
}

#' Get the name of the mf name/description column
#' 
#' Gets the name of the column in the e\_meta element that contains the name/description when a molecular formula is assigned
#' 
#' @param icrData an object of type icrData
#' @return name of column
#' @export
getMFNameColName <- function(icrData){
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$mfname_cname)
}

#' Set the name of the mf name/description column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains name/description when a molecular formula is assigned 
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setMFNameColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$mfname_cname <- cname
  return(icrData)
}

#' Get the name of the aromaticity column
#' 
#' Gets the name of the column in the e\_meta element that contains aromaticity values
#' 
#' @param icrData an object of type icrData
#' @return name of aromaticity column
#' @export
getAromaColName <- function(icrData){
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$aroma_cname)
}

#' Set the name of the aromaticity column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains aromaticity information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setAromaColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$aroma_cname <- cname
  return(icrData)
}

#' Get the name of the modified aromaticity column
#' 
#' Gets the name of the column in the e\_meta element that contains modified aromaticity values
#' 
#' @param icrData an object of type icrData
#' @return name of modified aromaticity column
#' @export
getModAromaColName <- function(icrData){
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$modaroma_cname)
}

#' Set the name of the modified aromaticity column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains modified aromaticity information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setModAromaColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$modaroma_cname <- cname
  return(icrData)
}

#' Get the name of the gibbs free energy column
#' 
#' Gets the name of the column in the e\_meta element that contains gibbs free energy values
#' 
#' @param icrData an object of type icrData
#' @return name of gibbs free energy column
#' @export
getGibbsColName <- function(icrData){
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$gfe_cname)
}

#' Set the name of the gibbs free energy column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains gibbs free energy information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#'
#' 
setGibbsColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$gfe_cname <- cname
  return(icrData)
}

#' Get the name of the double-bond equivalent column
#' 
#' Gets the name of the column in the e\_meta element that contains double-bond equivalent values
#' 
#' @param icrData an object of type icrData
#' @return name of double-bond equivalent column
#' @export
getDBEColName <- function(icrData){
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$dbe_cname)
}

#' Set the name of the double-bond equivalent column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains double-bond equivalent information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setDBEColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$dbe_cname <- cname
  return(icrData)
}

#' Get the name of the double-bond equivalent minus oxygen column
#' 
#' Gets the name of the column in the e\_meta element that contains double-bond equivalent values
#' 
#' @param icrData an object of type icrData
#' @return name of double-bond equivalent column
#' @export
getDBEoColName <- function(icrData){
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$dbeo_cname)
}

#' Set the name of the double-bond equivalent minus oxygen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains double-bond equivalent information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setDBEoColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$dbeo_cname <- cname
  return(icrData)
}

#' Get the name of the elemental composition column
#' 
#' Gets the name of the column in the e\_meta element that contains elemental composition
#' 
#' @param icrData an object of type icrData
#' @return name of elemental composition column
#' @export
getElCompColName <- function(icrData){
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$elcomp_cname)
}

#' Set the name of the elemental composition column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains elemental composition information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setElCompColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$elcomp_cname <- cname
  return(icrData)
}

#' Get the name of the peak class column
#' 
#' Gets the name of the column in the e\_meta element that contains peak class information
#' 
#' @param icrData an object of type icrData
#' @return name of peak class column
#' @export
getClassColName <- function(icrData){
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$class_cname)
}

#' Set the name of the peak class column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains peak class information.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setClassColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$class_cname <- cname
  return(icrData)
}

#' Get the name of the compound column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains compound IDs.
#'
#' @param icrData an object of type icrData
#' @return name of mass column
#' @export
getCompoundColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$compound_cname)
}

#' Set the name of the compound column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains compound IDs.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setCompoundColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$compound_cname <- cname
  return(icrData)
}

#' Get the name of the reaction column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains reaction IDs.
#'
#' @param icrData an object of type icrData
#' @return name of mass column
#' @export
getReactionColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$reaction_cname)
}

#' Set the name of the reaction column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains reaction IDs.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setReactionColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% icrData$e_meta)) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$reaction_cname <- cname
  return(icrData)
}

#' Get the name of the module column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains module IDs.
#'
#' @param icrData an object of type icrData
#' @return name of mass column
#' @export
getModuleColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$module_cname)
}

#' Set the name of the module column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains module IDs.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setModuleColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$module_cname <- cname
  return(icrData)
}

#' Get the name of the module column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains module node IDs. A module node corresponds
#' to one or more reactions that make up a single 
#' graph node in the module graph.
#'
#' @param icrData an object of type icrData
#' @return name of mass column
#' @export
getModuleNodeColName <- function(icrData) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  return(attr(icrData, "cnames")$module_node_cname)
}

#' Set the name of the module node column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains module node IDs. A module node corresponds
#' to one or more reactions that make up a single 
#' graph node in the module graph.
#'
#' @param icrData an object of type icrData
#' @param cname column name
#' @return updated icrData
#' 
#' 
setModuleNodeColName <- function(icrData, cname) {
  if (!inherits(icrData, "icrData")) {
    stop("icrData must be of type icrData")
  } 
  if (!(cname %in% names(icrData$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(icrData, "cnames")$module_node_cname <- cname
  return(icrData)
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


