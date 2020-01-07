#' Calculate DBE and DBE-O Values
#' 
#' Calculate double bond equivalent (DBE) and double bond equivalent minus Oxygen (DBE-O) values for peaks where empirical formula is available
#' 
#' @param ftmsObj an object of class 'peakData' or 'compoundData', typically a result of \code{\link{as.peakData}} or \code{\link{mapPeaksToCompounds}}.
#' @param valences a named list with the valence for each element.  Names must be any of 'C', 'H', 'N', 'O', 'S', 'P'. Values must be integers corresponding to the valence for each element.  Defaults to NULL, in which case the valences that result in the formula given in the details section are used.
#' 
#' @details 
#' \tabular{ll}{
#'  \tab If no valences are provided, DBE \eqn{= 1 + C - O - S - 0.5*(N + P + H)} \cr
#'  \tab If valences are provided DBE, \eqn{= 1 + \frac{\sum_{i}N_i(V_i-2)}{2}} where \eqn{N_i} and \eqn{V_i} are the number of atoms and corresponding valences.\cr
#'  \tab DBE-0 \eqn{= 1 + C - O - S - 0.5(N + P + H) - O}{= 1 + C - O - S - 0.5*(N + P + H) - O} \cr
#' }
#' 
#' @references Koch, B. P., & Dittmar, T. (2006). From mass to structure: an aromaticity index for high‐resolution mass data of natural organic matter. Rapid communications in mass spectrometry, 20(5), 926-932. 
#' @references Errata: Koch, B. P., & Dittmar, T. (2016). From mass to structure: an aromaticity index for high-resolution mass data of natural organic matter. Rapid communications in mass spectrometery, 30(1), 250. DOI: 10.1002/rcm.7433
#' 
#' @return an object of the same class as \code{ftmsObj} with a column in \code{e\_meta} giving DBE, DBE-O, and DBE_AI values
#'  
#' @author Lisa Bramer, Allison Thompson
#' 

calc_dbe <- function(ftmsObj, valences = NULL){
  
  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  # get coefficients that will multiply each elemental count.  Each coefficient is equal to {valence}-2
  if(is.null(valences)){
    coefs <- data.frame('C' = 2, 'H' = -1, 'N' = 1, 'O' = 0, 'S' = 0, 'P' = 1) # coefficients that result in the equation given in @details
  }
  else{
    if(!inherits(valences, 'data.frame')) stop('valences must be a data frame')
    if(!all(names(valences) %in% c('C', 'H', 'N', 'O', 'S', 'P'))) stop("valences dataframe must have column names column names 'C', 'H', 'N', 'O', 'S', 'P'")
    if(!all(sapply(valences, is.numeric)) | any(is.na(rowSums(valences)))) stop('valences dataframe must contain all numeric, nonmissing values')
    coefs <- lapply(c('C', 'H', 'N', 'O', 'S', 'P'), function(x) if(!is.null(valences[[x]])) valences[[x]]-2 else 0) %>% data.frame()
    names(coefs) <- c('C', 'H', 'N', 'O', 'S', 'P')
  }
  
  # pull e_meta out of ftmsObj #
  temp = ftmsObj$e_meta
  
  # get existing elemental counts
  C_counts = if(getCarbonColName(ftmsObj) %in% colnames(temp)) temp[,getCarbonColName(ftmsObj)] else 0
  H_counts = if(getHydrogenColName(ftmsObj) %in% colnames(temp)) temp[,getHydrogenColName(ftmsObj)] else 0
  N_counts = if(getNitrogenColName(ftmsObj) %in% colnames(temp)) temp[,getNitrogenColName(ftmsObj)] else 0
  O_counts = if(getOxygenColName(ftmsObj) %in% colnames(temp)) temp[,getOxygenColName(ftmsObj)] else 0
  S_counts = if(getSulfurColName(ftmsObj) %in% colnames(temp)) temp[,getSulfurColName(ftmsObj)] else 0
  P_counts = if(getPhosphorusColName(ftmsObj) %in% colnames(temp)) temp[,getPhosphorusColName(ftmsObj)] else 0
  
  dbe_cols <- NULL
  
  for(i in 1:nrow(coefs)){
    row <- coefs[i,]
    DBE_id <- make.unique(c(colnames(temp$e_meta), paste0('DBE_', i)))[length(colnames(temp$e_meta)) + 1] #makes DBE_id unique if column already exists
    dbe_cols <- c(dbe_cols, DBE_id)
    
    temp[DBE_id] = 1 + 0.5*(row[['C']]*C_counts + row[['H']]*H_counts + row[['N']]*N_counts + row[['O']]*O_counts + row[['S']]*S_counts + row[['P']]*P_counts)
  }
  
  # reassign valences in case they input one with missing columns and it was fixed in the first if-else statement
  valences <- coefs + 2
  rownames(valences) = dbe_cols
  
  # DBE_O and DBE_AI have fixed valences
  temp$DBE_O = 1 + 0.5*(2*C_counts - H_counts + N_counts + P_counts) - O_counts
  temp$DBE_AI = 1 + C_counts - O_counts - S_counts - 0.5*(N_counts + P_counts + H_counts)
  
  if(length(which(is.na(temp[,getMFColName(ftmsObj)]))) > 0){
    temp[which(is.na(temp[,getMFColName(ftmsObj)])), dbe_cols] = NA
    temp$DBE_O[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
    temp$DBE_AI[which(is.na(temp[,getMFColName(ftmsObj)]))] = NA
  }
  
  ftmsObj$e_meta = temp
  
  ftmsObj = setDBEColName(ftmsObj, dbe_cols)
  ftmsObj = setDBEoColName(ftmsObj, "DBE_O")
  ftmsObj = setDBEAIColName(ftmsObj, "DBE_AI")
  ftmsObj = setDBEValenceDF(ftmsObj, valences)
  
  return(ftmsObj)
}