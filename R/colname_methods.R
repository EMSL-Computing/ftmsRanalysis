## A set of convenience methods for getting and setting the attributes that store
## the column names for particular information.

#' Get the name of the e_data unique identifier column
#' 
#' Returns the name of the column in the e_data element that 
#' is the unique identifier of each row.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of unique ID column 
#' @export
getEDataColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$edata_cname)
}

#' Set the name of the e_data unique identifier column
#' 
#' Sets the name of the column in the e_data element that 
#' is the unique identifier of each row.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setEDataColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  
  attr(ftmsObj, "cnames")$edata_cname <- cname
  return(ftmsObj)
}

#' Get the name of the f_data unique identifier column
#' 
#' Returns the name of the column in the f_data element that 
#' is the unique identifier of each row.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of unique ID column 
#' @export
getFDataColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$fdata_cname)
}

#' Set the name of the f_data unique identifier column
#' 
#' Sets the name of the column in the f_data element that 
#' is the unique identifier of each row.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setFDataColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$fdata_cname <- cname
  return(ftmsObj)
}

#' Get the name of the e_meta unique identifier column
#' 
#' Returns the name of the column in the e_meta element that 
#' is the unique identifier of each row.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of unique ID column 
#' @export
getMFColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$mf_cname)
}

#' Set the name of the e_meta unique identifier column
#' 
#' Sets the name of the column in the e_meta element that 
#' is the unique identifier of each row.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setMFColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$mf_cname <- cname
  return(ftmsObj)
}

#' Get the name of a ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains the specified ratio data for Van Krevelen plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @param ratio string specifying ratio to retrieve. Written as "H:C"
#' @return name of specified ratio column
#' @export
getRatioColName <- function(ftmsObj, ratio) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  }
  ratio_col_name <- paste0(strsplit(ratio,":")[[1]][1], "2", strsplit(ratio,":")[[1]][2], "_cname")
  return(attr(ftmsObj, "cnames")[[ratio_col_name]])
}

#' Set the name of the specified ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains the specified ratio data for Van Krevelen plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @param ratio a string specifying the ratio pair for the column formatted as "C:H"
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setRatioColName <- function(ftmsObj, ratio, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$ratio_cnames[[ratio]] <- cname
  return(ftmsObj)
}

#' Get the name of the O:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains O:C ratio data for Van Krevelen plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of O:C ratio column
#' @export
getOCRatioColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$o2c_cname)
}

#' Set the name of the O:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains O:C ratio data for Van Krevelen plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setOCRatioColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$o2c_cname <- cname
  return(ftmsObj)
}


#' Get the name of the H:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains H:C ratio data for Van Kevelen plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of H:C ratio column
#' @export
getHCRatioColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$h2c_cname)
}

#' Set the name of the H:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains H:C ratio data for Van Krevelen plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setHCRatioColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$h2c_cname <- cname
  return(ftmsObj)
}

#' Get the name of the N:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains N:C ratio data 
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of N:C ratio column
#' @export
getNCRatioColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$n2c_cname)
}

#' Set the name of the N:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains N:C ratio data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setNCRatioColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$n2c_cname <- cname
  return(ftmsObj)
}

#' Get the name of the P:C ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains P:C ratio data.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of O:C ratio column
#' @export
getPCRatioColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$p2c_cname)
}

#' Set the name of the P:C ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains P:C ratio data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setPCRatioColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$p2c_cname <- cname
  return(ftmsObj)
}

#' Get the name of the N:P ratio column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains N:P ratio data for Van Krevelen plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of N:P ratio column
#' @export
getNPRatioColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$n2p_cname)
}

#' Set the name of the N:P ratio column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains N:P ratio data for Van Krevelen plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setNPRatioColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$n2p_cname <- cname
  return(ftmsObj)
}

#' Get the name of the mass column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains mass data for calculations.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of mass column
#' @export
getMassColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$mass_cname)
}

#' Set the name of the mass column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains mass data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setMassColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$mass_cname <- cname
  return(ftmsObj)
}

#' Get the name of the column for the specified element
#' 
#' Returns the name of the column in the e_meta element that 
#' contains element count data for calculations.
#'
#' @param ftmsObj an object of type ftmsData
#' @param element a string specifying the element or isotope name under its standard abbreviation
#' @return name of element column
#' @export
getElementColName <- function(ftmsObj, element) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$element_col_names[[element]])
}

#' Set the name of the column of the specified element
#' 
#' Sets the name of the column in the e_meta element that 
#' contains the element's count data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param element a string specifying the element or isotope name under its standard abbreviation
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setElementColName <- function(ftmsObj, element, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$element_col_names[[element]] <- cname
  return(ftmsObj)
}

#' Get the name of the carbon column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains carbon count data for calculations.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of carbon column
#' @export
getCarbonColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$element_col_names$C)
}

#' Set the name of the carbon column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains carbon count data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setCarbonColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$element_col_names$C <- cname
  return(ftmsObj)
}

#' Get the name of the hydrogen column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains hydrogen count data for calculations.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of hydrogen column
#' @export
getHydrogenColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$element_col_names$H)
}

#' Set the name of the hydrogen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains hydrogen count data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setHydrogenColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$element_col_names$H <- cname
  return(ftmsObj)
}

#' Get the name of the oxygen column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains oxygen count data for calculations.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of oxygen column
#' @export
getOxygenColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$element_col_names$O)
}

#' Set the name of the oxygen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains oxygen count data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setOxygenColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$element_col_names$O <- cname
  return(ftmsObj)
}

#' Get the name of the nitrogen column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains nitrogen count data for calculations.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of nitrogen column
#' @export
getNitrogenColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$element_col_names$N)
}

#' Set the name of the nitrogen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains nitrogen count data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setNitrogenColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$element_col_names$N <- cname
  return(ftmsObj)
}

#' Get the name of the sulfur column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains sulfur count data for calculations.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of sulfur column
#' @export
getSulfurColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$element_col_names$S)
}

#' Set the name of the sulfur column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains sulfur count data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setSulfurColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$element_col_names$S <- cname
  return(ftmsObj)
}

#' Get the name of the phosphorus column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains phosphorus count data for calculations.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of phosphorus column
#' @export
getPhosphorusColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$element_col_names$P)
}

#' Set the name of the phosphorus column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains phosphorus count data.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setPhosphorusColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$element_col_names$P <- cname
  return(ftmsObj)
}

#' Get the name of the isotopic column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains isotopic peak information.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of isotopic column
#' @export
getIsotopicColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$isotopic_cname)
}

#' Set the name of the isotopic column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains isotopic peak information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setIsotopicColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$isotopic_cname <- cname
  return(ftmsObj)
}

#' Get the name of the Kendrick mass column
#' 
#' Returns the names of the columns in the e_meta element that 
#' contains Kendrick mass data for Kendrick plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of Kendrick mass column
#' @export
getKendrickMassColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$kmass_cname)
}

#' Set the name of the Kendrick mass column
#' 
#' Sets the names of the columns in the e_meta element that 
#' contains Kendrick mass data for Kendrick plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cnames column names
#' @return updated ftmsObj
#' 
#' 
setKendrickMassColName <- function(ftmsObj, cnames) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!all(cnames %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Columns '%s' are not found in the e_meta data", paste(cnames, collapse = ', ')))
  }
  attr(ftmsObj, "cnames")$kmass_cname <- cnames
  return(ftmsObj)
}

#' Get the names of the Kendrick defect column
#' 
#' Returns the names of the columns in the e_meta element that 
#' contains Kendrick defect data for Kendrick plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of Kendrick defect column
#' @export
getKendrickDefectColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$kdefect_cname)
}

#' Set the names of the Kendrick defect column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains Kendrick defect data for Kendrick plots.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cnames column names
#' @return updated ftmsObj
#' 
#' 
setKendrickDefectColName <- function(ftmsObj, cnames) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!all(cnames %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Columns '%s' are not found in the e_meta data", paste(cnames, collapse = ', ')))
  }
  attr(ftmsObj, "cnames")$kdefect_cname <- cnames
  return(ftmsObj)
}


#' Get the name of the extraction column
#' 
#' Returns the name of the column in the f_data element that 
#' contains extraction information.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of extraction column
#' @export
getExtractionColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$extraction_cname)
}

#' Set the name of the extraction column
#' 
#' Sets the name of the column in the f_data element that 
#' contains extraction information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setExtractionColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$extraction_cname <- cname
  return(ftmsObj)
}


#' Get the name of the NOSC column
#' 
#' Gets the name of the column in the e\_meta element that contains NOSC values
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of NOSC column
#' @export
getNOSCColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$nosc_cname)
}

#' Set the name of the NOSC column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains NOSC information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setNOSCColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$nosc_cname <- cname
  return(ftmsObj)
}

#' Get the name of the mf name/description column
#' 
#' Gets the name of the column in the e\_meta element that contains the name/description when a molecular formula is assigned
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of column
#' @export
getMFNameColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$mfname_cname)
}

#' Set the name of the mf name/description column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains name/description when a molecular formula is assigned 
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setMFNameColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$mfname_cname <- cname
  return(ftmsObj)
}

#' Get the name of the aromaticity column
#' 
#' Gets the name of the column in the e\_meta element that contains aromaticity values
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of aromaticity column
#' @export
getAromaColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$aroma_cname)
}

#' Set the name of the aromaticity column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains aromaticity information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setAromaColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$aroma_cname <- cname
  return(ftmsObj)
}

#' Get the name of the modified aromaticity column
#' 
#' Gets the name of the column in the e\_meta element that contains modified aromaticity values
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of modified aromaticity column
#' @export
getModAromaColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$modaroma_cname)
}

#' Set the name of the modified aromaticity column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains modified aromaticity information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setModAromaColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$modaroma_cname <- cname
  return(ftmsObj)
}

#' Get the name of the gibbs free energy column
#' 
#' Gets the name of the column in the e\_meta element that contains gibbs free energy values
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of gibbs free energy column
#' @export
getGibbsColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$gfe_cname)
}

#' Set the name of the gibbs free energy column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains gibbs free energy information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#'
#' 
setGibbsColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$gfe_cname <- cname
  return(ftmsObj)
}

#' Get the name of the double-bond equivalent column
#' 
#' Gets the name of the column in the e_meta element that contains double-bond equivalent values
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of double-bond equivalent column
#' @export
getDBEColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$dbe_cname)
}

#' Set the name of the double-bond equivalent column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains double-bond equivalent information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cnames column names
#' @return updated ftmsObj
#' 
#' 
setDBEColName <- function(ftmsObj, cnames) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!all(cnames %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Columns '%s' are not found in the e_meta data", paste(cnames, collapse = ',')))
  }
  attr(ftmsObj, "cnames")$dbe_cname <- cnames
  return(ftmsObj)
}

#' Get the name of the double-bond equivalent minus oxygen column
#' 
#' Gets the name of the column in the e\_meta element that contains double-bond equivalent values
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of double-bond equivalent column
#' @export
getDBEoColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$dbeo_cname)
}

#' Set the name of the double-bond equivalent minus oxygen column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains double-bond equivalent information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setDBEoColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$dbeo_cname <- cname
  return(ftmsObj)
}

#' Get the name of the double-bond equivalent AI column
#' 
#' Gets the name of the column in the e\_meta element that contains double-bond equivalent AI values
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of double-bond equivalent AI column
#' @export
getDBEAIColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$dbeai_cname)
}

#' Set the name of the double-bond equivalent AI column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains double-bond equivalent AI information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setDBEAIColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$dbeai_cname <- cname
  return(ftmsObj)
}

#' Get the name of the elemental composition column
#' 
#' Gets the name of the column in the e\_meta element that contains elemental composition
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of elemental composition column
#' @export
getElCompColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$elcomp_cname)
}

#' Set the name of the elemental composition column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains elemental composition information.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setElCompColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$elcomp_cname <- cname
  return(ftmsObj)
}

#' Get the name of the peak class column based on boundary set 1
#' 
#' Gets the name of the column in the e\_meta element that contains peak class information based on "bs1"
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of peak class column
#' @export
getBS1ColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$bs1class_cname)
}

#' Set the name of the peak class column based on boundary set 1
#' 
#' Sets the name of the column in the e_meta element that 
#' contains peak class information based on "bs1".
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setBS1ColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$bs1class_cname <- cname
  return(ftmsObj)
}

#' Get the name of the peak class column based on boundary set 2
#' 
#' Gets the name of the column in the e\_meta element that contains peak class information based on "bs2"
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of peak class column
#' @export
getBS2ColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$bs2class_cname)
}

#' Set the name of the peak class column based on boundary set 2
#' 
#' Sets the name of the column in the e_meta element that 
#' contains peak class information based on "bs2".
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setBS2ColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$bs2class_cname <- cname
  return(ftmsObj)
}

#' Get the name of the peak class column based on boundary set 3
#' 
#' Gets the name of the column in the e\_meta element that contains peak class information based on "bs3"
#' 
#' @param ftmsObj an object of type ftmsData
#' @return name of peak class column
#' @export
getBS3ColName <- function(ftmsObj){
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$bs3class_cname)
}

#' Set the name of the peak class column based on boundary set 3
#' 
#' Sets the name of the column in the e_meta element that 
#' contains peak class information based on "bs3".
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setBS3ColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$bs3class_cname <- cname
  return(ftmsObj)
}

#' Get the name of the compound column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains compound IDs.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of mass column
#' @export
getCompoundColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$compound_cname)
}

#' Set the name of the compound column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains compound IDs.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setCompoundColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$compound_cname <- cname
  return(ftmsObj)
}

#' Get the name of the reaction column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains reaction IDs.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of mass column
#' @export
getReactionColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$reaction_cname)
}

#' Set the name of the reaction column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains reaction IDs.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setReactionColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% ftmsObj$e_meta)) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$reaction_cname <- cname
  return(ftmsObj)
}

#' Get the name of the module column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains module IDs.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of mass column
#' @export
getModuleColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$module_cname)
}

#' Set the name of the module column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains module IDs.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setModuleColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$module_cname <- cname
  return(ftmsObj)
}

#' Get the name of the module column
#' 
#' Returns the name of the column in the e_meta element that 
#' contains module node IDs. A module node corresponds
#' to one or more reactions that make up a single 
#' graph node in the module graph.
#'
#' @param ftmsObj an object of type ftmsData
#' @return name of mass column
#' @export
getModuleNodeColName <- function(ftmsObj) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  return(attr(ftmsObj, "cnames")$module_node_cname)
}

#' Set the name of the module node column
#' 
#' Sets the name of the column in the e_meta element that 
#' contains module node IDs. A module node corresponds
#' to one or more reactions that make up a single 
#' graph node in the module graph.
#'
#' @param ftmsObj an object of type ftmsData
#' @param cname column name
#' @return updated ftmsObj
#' 
#' 
setModuleNodeColName <- function(ftmsObj, cname) {
  if (!inherits(ftmsObj, "ftmsData")) {
    stop("ftmsObj must be of type ftmsData")
  } 
  if (!(cname %in% names(ftmsObj$e_meta))) {
    stop(sprintf("Column '%s' is not found in the e_meta data", cname))
  }
  attr(ftmsObj, "cnames")$module_node_cname <- cname
  return(ftmsObj)
}



############# TESTS ################

# data(exData)
# 
# getEDataColName(exData) == "peak"
# getFDataColName(exData) == "Sample"
# getEMetaColName(exData) == "mf"
# getOCRatioColName(exData) == "O/C"
# getHCRatioColName(exData) == "H/C"
# getKendrickMassColName(exData) == "kendrick_m"
# getKendrickDefectColName(exData) == "kendrick_defect"
# getExtractionColName(exData) == "Extraction"
# 
# getEDataColName(setEDataColName(exData, "test")) == "test"
# getFDataColName(setFDataColName(exData, "test")) == "test"
# getEMetaColName(setEMetaColName(exData, "test")) == "test"
# getOCRatioColName(setOCRatioColName(exData, "test")) == "test"
# getHCRatioColName(setHCRatioColName(exData, "test")) == "test"
# getKendrickMassColName(setKendrickMassColName(exData, "test")) == "test"
# getKendrickDefectColName(setKendrickDefectColName(exData, "test")) == "test"
# getExtractionColName(setExtractionColName(exData, "test")) == "test"
# 
# attr(exData, "cnames")<- NULL
# getEDataColName(setEDataColName(exData, "test")) == "test"
# getFDataColName(setFDataColName(exData, "test")) == "test"
# getEMetaColName(setEMetaColName(exData, "test")) == "test"
# getOCRatioColName(setOCRatioColName(exData, "test")) == "test"
# getHCRatioColName(setHCRatioColName(exData, "test")) == "test"
# getKendrickMassColName(setKendrickMassColName(exData, "test")) == "test"
# getKendrickDefectColName(setKendrickDefectColName(exData, "test")) == "test"
# getExtractionColName(setExtractionColName(exData, "test")) == "test"


