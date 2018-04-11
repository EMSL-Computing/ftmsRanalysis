#' Summarize samples to group level estimates
#' 
#' Summarize sample data by group using a specified summary function. The summary
#' function provided must take a data frame with one or more columns corresponding to 
#' samples. It must return a data frame with the same number of rows and one column. 
#' 
#' @param icrData an object of class 'peakIcrData' or 'compoundIcrData' that has only columns of e_data corresponding to the samples in
#' a group (e.g. the output of \code{\link{subset.icrData}} or a ddo of icrData objects (e.g. the output of \code{\link{divideByGroup}})
#' @param summary_functions list or vector of summary function names to apply to each row of \code{icrData$e_data} for each group. Valid
#' summary function names are given by \code{\link{getGroupSummaryFunctionNames}}. If the
#' list or vector has names, those names will be used to name the resulting columns in the returned object.
#' @export
#' 
#' @examples
#' data("edata")
#' data("fdata")
#' data("emeta")
#' 
#' picr <- as.peakIcrData(edata, fdata, emeta, edata_cname="peak", fdata_cname="Sample.ID", mass_cname="peak",
#'                        c_cname="c.number", h_cname="h.number", o_cname="o.number",
#'                        n_cname="n.number", s_cname="s.number", p_cname="p.number",
#'                        isotopic_cname = "isotopic", isotopic_notation = "TRUE")
#' 
#' picr <- compound_calcs(picr)
#' picr <- applyFilt(mass_filter(picr), picr, min_mass = 200, max_mass = 900)
#' picr <- group_designation(picr, c("Location"))
#' picr_A <- subset(picr, groups="A")
#' 
#' groupASummary <- summarizeGroups(picr_A, summary_functions=list(count=n_present, proportion=prop_present))
summarizeGroups <- function(icrData, summary_functions) {
  require(datadr)
  if (!(inherits(icrData, "peakIcrData") | !inherits(icrData, "compoundIcrData")) & !inherits(icrData, "ddo") )
      stop("icrData must be of type peakIcrData, compoundIcrData, or a ddo containing those objects")
  if (missing(summary_functions)) stop("summary_function must be provided")
  if (is.vector(summary_functions)) summary_functions <- as.list(summary_functions)
  if (!is.list(summary_functions)) stop("summary_function must be a list")
  
  ## This is a test of the emergency get system. This is only a test
  # tmp1 <- get("n_present", envir=as.environment("package:fticRanalysis"), mode="function")
  # if (is.null(tmp1)) stop("Cannot get the n_present function")
  # else cat("That get command worked fine!\n")
  
  # Get function objects from names
  validNames <- getGroupSummaryFunctionNames()
  summary_functions <- lapply(summary_functions, function(nn) {
    nn <- as.character(nn)
    if (!(nn %in% validNames)) stop(sprintf("'%s' is not a valid function name, see getGroupSummaryFunctionNames() for valid options", nn))
    return(get(nn, envir=asNamespace("fticRanalysis"), mode="function"))
  })
  
  # if summary_functions has any missing names, fill them in so they can be used to name output columns
  if (is.null(names(summary_functions))) {
    names(summary_functions) <- unlist(lapply(summary_functions, function(f) attr(f, "default_column_name")))
  } else if (any(is.na(names(summary_functions))) | any(nchar(names(summary_functions)) == 0)) {
    ind <- which(is.na(names(summary_functions)) | nchar(names(summary_functions)) == 0)
    names(summary_functions)[ind] <- unlist(lapply(summary_functions[ind], function(f) attr(f, "default_column_name")))
  }
  
  if (inherits(icrData, "ddo")) {
    ## do something
    res <- drPersist(addTransform(icrData, function(v) {
      fticRanalysis:::.summarizeGroupsInternal(v, summary_functions)
    }))
  } else {
    res <- .summarizeGroupsInternal(icrData, summary_functions)
  }
  return(res)
}
  
# Internal only function for use on 1 icrData object not a ddo of them
.summarizeGroupsInternal <- function(icrData, summary_functions) {

  samp_cols <- setdiff(colnames(icrData$e_data), getEDataColName(icrData))
  data_scale <- getDataScale(icrData)
  edata_cols <- lapply(summary_functions, function(f) {
    f(icrData$e_data[,samp_cols], data_scale)
  })
  new_edata <- data.frame(icrData$e_data[, getEDataColName(icrData)], edata_cols)
  colnames(new_edata)[1] <- getEDataColName(icrData)

  new_fdata <- data.frame(Group.Summary.Column=names(edata_cols), Num.Samples=length(samp_cols), 
                             Summary.Function.Name=NA, stringsAsFactors = FALSE)
  
  if (inherits(icrData, "peakIcrData")) {
    res <- as.peakIcrData(new_edata, new_fdata, icrData$e_meta, getEDataColName(icrData), "Group.Summary.Column", getMassColName(icrData), mf_cname=getMFColName(icrData) )
  } else if (inherits(icrData, "compoundIcrData")) {
      res <- as.compoundIcrData(new_edata, new_fdata, icrData$e_meta, getEDataColName(icrData), "Group.Summary.Column", getMassColName(icrData), getCompoundColName(icrData) )
  # } else if (inherits(icrData, "reactionIcrData")) {
  #   res <- as.reactionIcrData(new.edata, new.fdata, icrData$e_meta, getEDataColName(icrData), "Group.Summary.Column", getReactionColName(icrData))
  # } else if (inherits(icrData, "moduleIcrData")) {
  #   res <- as.moduleIcrData(new.edata, new.fdata, icrData$e_meta, getEDataColName(icrData), "Group.Summary.Column", getModuleColName(icrData),
  #                           getModuleNodeColName(icrData))
  }

  # copy other attributes to new object
  cnames.new <- attr(res, "cnames")
  cnames.old <- attr(icrData, "cnames")
  for (cc in setdiff(names(cnames.old), c("edata_cname", "fdata_cname", "mass_cname", "mf_cname", "compound_cname"))) {
    if (!is.null(cnames.old[[cc]]))
      cnames.new[[cc]] <- cnames.old[[cc]]
  }
  attr(res, "group_DF") <- attr(icrData, "group_DF")
  attr(res, "cnames") <- cnames.new
  attr(res, "filters") <- attr(icrData, "filters")
  attr(res, "instrument_type") <- attr(icrData, "instrument_type")

  # set class to include 'groupSummary'
  class(res) <- c("groupSummary", class(res))
  
  return(res)
}