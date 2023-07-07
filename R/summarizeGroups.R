#' Summarize samples to group level estimates
#'
#' Summarize sample data by group using a specified summary function. The
#' summary function provided must take a data frame with one or more columns
#' corresponding to samples. It must return a data frame with the same number of
#' rows and one column.
#'
#' @param ftmsObj an object of class 'peakData' or 'compoundData' or a list of
#'   ftmsData objects (e.g. the output of \code{\link{divideByGroup}})
#' @param summary_functions vector of summary function names to apply to each
#'   row of \code{ftmsObj$e_data} for each group. Valid summary function names
#'   are given by \code{\link{getGroupSummaryFunctionNames}}.
#' @return If the input is an ftmsData object, the result will be a new
#'   \code{ftmsData} object where each provided summary function will be applied
#'   to each group found in \code{getGroupDF(ftmsObj)}. If
#'   \code{getGroupDF(ftmsObj) == null} the function will assume all samples
#'   belong to a single group. If the input is a list the result will be a list
#'   where each value is the result of applying \code{summarizeGroups} to each
#'   value of the input.
#'
#' @author Amanda White
#'
#' @export
#'
#' @examples
#' data("exampleProcessedPeakData")
#' summary1 <- summarizeGroups(exampleProcessedPeakData, 
#'                             summary_functions=c("n_present", "prop_present"))
#'
#' groupList <- divideByGroup(exampleProcessedPeakData)
#' summary2 <- summarizeGroups(groupList, summary_functions=c("n_present", "prop_present"))
summarizeGroups <- function(ftmsObj, summary_functions) {
  if (!(inherits(ftmsObj, "peakData") | !inherits(ftmsObj, "compoundData")) & !inherits(ftmsObj, "list") )
      stop("ftmsObj must be of type peakData, compoundData, or a list containing those objects")
  if (inherits(ftmsObj, "groupSummary") | inherits(ftmsObj, "groupComparison") | inherits(ftmsObj, "comparisonSummary")) 
    stop("ftmsObj cannot be a groupSummary, groupComparison or comparisonSummary object")
  if (missing(summary_functions)) stop("summary_function must be provided")
  if (is.vector(summary_functions)) summary_functions <- as.list(summary_functions)
  if (!is.list(summary_functions)) stop("summary_function must be a list")
  
  if (inherits(ftmsObj, "list")) {
    res <- lapply(ftmsObj, function(v) {
      ftmsRanalysis:::.summarizeGroupsInternal(v, summary_functions)
    })
  } else {
    res <- .summarizeGroupsInternal(ftmsObj, summary_functions)
  }
  return(res)
}
  
# Internal only function for use on 1 ftmsData object not a list of them
.summarizeGroupsInternal <- function(ftmsObj, summary_functions) {

  # Get function objects from names
  summary_func_names <- as.vector(unlist(summary_functions))
  validNames <- getGroupSummaryFunctionNames()
  summary_functions <- lapply(summary_functions, function(nn) {
    nn <- as.character(nn)
    if (!(nn %in% validNames)) stop(sprintf("'%s' is not a valid function name, see getGroupSummaryFunctionNames() for valid options", nn))
    return(get(nn, envir=asNamespace("ftmsRanalysis"), mode="function"))
  })
  
  # if summary_functions has any missing names, fill them in so they can be used to name output columns
  # if (is.null(names(summary_functions))) {
  #   names(summary_functions) <- unlist(lapply(summary_functions, function(f) attr(f, "function_name")))
  # } else if (any(is.na(names(summary_functions))) | any(nchar(names(summary_functions)) == 0)) {
  #   ind <- which(is.na(names(summary_functions)) | nchar(names(summary_functions)) == 0)
  #   names(summary_functions)[ind] <- unlist(lapply(summary_functions[ind], function(f) attr(f, "function_name")))
  # }

  data_scale <- getDataScale(ftmsObj)
  groupDF <- getGroupDF(ftmsObj)
  if (is.null(groupDF)) {
    samp_names <- unique(ftmsObj$f_data[, getFDataColName(ftmsObj)])
    groupDF <- data.frame(Sample=samp_names, Group=samp_names)
    colnames(groupDF)[1] <- getFDataColName(ftmsObj)
  }
  
  # make sure all samples in groupDF are represented in e_data
  if (!all(groupDF[, getFDataColName(ftmsObj)] %in% colnames(ftmsObj$e_data))) {
    stop(sprintf("Columns referenced in groupDF object are not found in e_data: %s", 
                 paste(setdiff(colnames(ftmsObj$e_meta), groupDF[, getFDataColName(ftmsObj)]), collapse=", ")))
  }

  # for each group of sample columns, apply all summary functions and recombine columns
  edata_cols <- lapply(unique(groupDF$Group), function(grp_name) {
    samp_cols <- as.character(dplyr::filter(groupDF, Group == grp_name)[, getFDataColName(ftmsObj)])
    grp_cols <- lapply(summary_functions, function(f) {
      f(ftmsObj$e_data[,samp_cols], data_scale)
    })
    grp_cols <- do.call(cbind, grp_cols)
    names(grp_cols) <- make.names(paste0(grp_name, "_", colnames(grp_cols)))
    tmp_fdata <- data.frame(Group_Summary_Column=names(grp_cols), Group=grp_name, Num_Samples=length(samp_cols), 
                            Summary_Function_Name=summary_func_names, stringsAsFactors = FALSE)
    attr(grp_cols, "f_data") <- tmp_fdata
    return(grp_cols)
  })
  
  new_fdata <- do.call(rbind, lapply(edata_cols, function(x) attr(x, "f_data")))
  
  # edata_cols <- unlist(edata_cols, recursive = FALSE)
  
  new_edata <- data.frame(ftmsObj$e_data[, getEDataColName(ftmsObj)], do.call(cbind, edata_cols))
  colnames(new_edata)[1] <- getEDataColName(ftmsObj)

  # new_fdata <- data.frame(Group.Summary.Column=names(edata_cols), Num.Samples=length(samp_cols), 
  #                            Summary.Function.Name=NA, stringsAsFactors = FALSE)
  
  if (inherits(ftmsObj, "peakData")) {
    res <- as.peakData(new_edata, new_fdata, ftmsObj$e_meta, getEDataColName(ftmsObj), "Group_Summary_Column", getMassColName(ftmsObj), mf_cname=getMFColName(ftmsObj), element_col_names = attr(ftmsObj, "cnames")$element_col_names )
  } else if (inherits(ftmsObj, "compoundData")) {
      res <- as.compoundData(new_edata, new_fdata, ftmsObj$e_meta, getEDataColName(ftmsObj), "Group_Summary_Column", getMassColName(ftmsObj), getCompoundColName(ftmsObj) )
  }

  # copy cnames  attributes to new object
  cnames.new <- attr(res, "cnames")
  cnames.old <- attr(ftmsObj, "cnames")
  for (cc in setdiff(names(cnames.old), c("edata_cname", "fdata_cname", "mass_cname", "mf_cname", "compound_cname"))) {
    if (!is.null(cnames.old[[cc]]))
      cnames.new[[cc]] <- cnames.old[[cc]]
  }
  attr(res, "cnames") <- cnames.new

  # copy attributes from original object to new
  diffAttrNames <- c("cnames", "class", "names", "split", "group_DF")  #attribute names that should not be the same in the result object
  for (attr_name in setdiff(names(attributes(ftmsObj)), diffAttrNames)) {
    attr(res, attr_name) <- attr(ftmsObj, attr_name)
  }
  
  new_group_df <- dplyr::select(new_fdata, Group_Summary_Column, Group) %>% 
    dplyr::left_join(dplyr::select(groupDF, -dplyr::one_of(getFDataColName(ftmsObj))), by="Group", multiple="all") %>%
    unique()
  attr(new_group_df, "main_effects") <- attr(groupDF, "main_effects")
  res <- ftmsRanalysis:::setGroupDF(res, new_group_df)
  
  # set class to include 'groupSummary'
  class(res) <- c("groupSummary", class(res))
  
  # set data scale
  res <- ftmsRanalysis:::setDataScale(res, "summary")
  
  res <- ftmsRanalysis:::setInstrumentType(res, getInstrumentType(ftmsObj))
  if (!is.null(getDatabase(ftmsObj))) res <- ftmsRanalysis:::setDatabase(res, getDatabase(ftmsObj))
  
  
  return(res)
}