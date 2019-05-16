#' Plot to look at the percentage of each class in the data
#' 
#' Constructs stacked bar plot to look at the percentage of each class in the data
#' 
#' @param ftmsObj ftmsData object of class peakData or compoundData
#' @param xaxis x axis variable. If NULL, will use attr(ftmsObj, "cnames")$fdata_cname. Must be one of
#'                colnames(ftmsObj$f_data) or colnames(attr(ftmsObj, "group_DF")).
#' @param ylabel y axis label, default is "Density"
#' @param title plot title, default is NULL
#' @param vkBoundarySet character vector specifying which boundary set to use when determining class. Valid options are currently "bs1" and "bs2" and defaults to "bs1". See details of \code{\link{assign_class}} for differences in sets.
#' @param classColName column name of \code{ftmsData$e_meta} to use for classes (if classes have been precomputed)
#' 
#' @return a plotly object
#' 
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Allison Thompson
#' 
#'
classesPlot <- function(ftmsObj, xaxis=NULL, ylabel="Percentage of Chemical Classes", title=NULL, vkBoundarySet="bs1", classColName=NULL) {
  
  # Initial Checks #
  # check that ftmsObj is of the correct class #
  if(!inherits(ftmsObj, "peakData") & !inherits(ftmsObj, "compoundData")) stop("ftmsObj must be an object of class 'peakData' or 'compoundData'")
  
  
  if(is.null(xaxis)){
    xaxis <- ftmsRanalysis:::getFDataColName(ftmsObj)
  }
  
  if(!(xaxis %in% c(colnames(ftmsObj$f_data), colnames(attr(ftmsObj, "group_DF"))))){
    stop("xaxis must be one of the column names of ftmsObj$f_data or attr(ftmsObj, 'group_DF').")
  }
  
  # End Initial Checks #
  
  # calculate the number of peaks per sample
  classes <- ftmsObj$e_data
  
  if(is.null(classColName) & !is.null(vkBoundarySet)){
    if(!(vkBoundarySet) %in% c("bs1","bs2")){ stop("vkBoundarySet must be one of 'bs1' or 'bs2'.") }
    classes$VKClassesForPlot <- ftmsRanalysis:::assign_class(ftmsObj, vkBoundarySet)$e_meta[,paste(vkBoundarySet,"class",sep="_")]
    classes$VKClassesForPlot <- gsub(";.*", "",classes$VKClassesForPlot)
    classes$VKClassesForPlot <- factor(classes$VKClassesForPlot, levels=rownames(ftmsRanalysis:::getVanKrevelenCategoryBounds(vkBoundarySet)$VKbounds))
  }else if(!is.null(classColName)){
    if(!(classColName) %in% colnames(ftmsObj$e_meta)){ stop("classColName not found in ftmsObj$e_meta.") }
    classes$VKClassesForPlot <- ftmsObj$e_meta[match(ftmsObj$e_meta[,ftmsRanalysis:::getMassColName(ftmsObj)], classes[,ftmsRanalysis:::getMassColName(ftmsObj)]), classColName]
  }else{
    stop("Unsure what to use to color by, please specify either 'vkBoundarySet' or 'classColName'")
  }
  
  classes <- reshape2::melt(classes)
  colnames(classes)[which(colnames(classes) == "variable")] <- ftmsRanalysis:::getFDataColName(ftmsObj)
  classes <- merge(classes, ftmsObj$e_meta, by=getMassColName(ftmsObj))
  
  # remove unassigned 
  if(any(is.na(classes[,ftmsRanalysis:::getMFColName(ftmsObj)]))){
    classes <- classes[-which(is.na(classes[,ftmsRanalysis:::getMFColName(ftmsObj)])),]
    classes <- droplevels(classes)
  }
  
  # merge peaks with metadata or group_DF
  if(xaxis %in% colnames(attr(ftmsObj, "group_DF"))){
    classes <- merge(classes, attr(ftmsObj, "group_DF"), by=ftmsRanalysis:::getFDataColName(ftmsObj))
  }else{
    classes <- merge(classes, ftmsObj$f_data, by=ftmsRanalysis:::getFDataColName(ftmsObj)) 
  }
  
  if(xaxis == getFDataColName(ftmsObj)){
    # summarise to percentage in each class
    vars1 <- c(getFDataColName(ftmsObj), "VKClassesForPlot")
    vars1 <- lapply(vars1, as.symbol)
  
    class_grp <- classes %>% dplyr::group_by_(.dots=vars1) %>%
      dplyr::summarise(Num=length(which(value > 0 & !is.na(value))))
    

  }else{
    # if grouping, take median per group
    vars1 <- c(getFDataColName(ftmsObj), xaxis, "VKClassesForPlot")
    vars1 <- lapply(vars1, as.symbol)
    
    vars2 <- c(xaxis, "VKClassesForPlot")
    vars2 <- lapply(vars2, as.symbol)
    
    class_grp <- classes %>% dplyr::group_by_(.dots=vars1) %>%
      dplyr::summarise(Num=length(which(value > 0 & !is.na(value)))) %>% 
      dplyr::group_by_(.dots=vars2) %>%
      dplyr::summarise(Num=median(Num, na.rm=TRUE))
  }
  
  # get colors
  cc <- unique(as.data.frame(class_grp[, "VKClassesForPlot"]))
  cc <- cc[,1]
  
  temp <- c("lip","uns","pro","lig","car","ami","tan","con","oth")
  temp.id <- lapply(temp, function(x) which(sapply(cc, function(y) tolower(substr(y,1,3))) == x))
  
  if(all(unlist(lapply(temp.id, function(x) length(x)))) == 1){
    cc <- cc[unlist(temp.id)]
    cc <- factor(cc, levels=cc)
  }
  
  colorPal <- ftmsRanalysis:::getFactorColorPalette(cc)  
  col_vec <- colorPal(cc)
  names(col_vec) <- cc
  
  # Plot
  map <- ggplot2::aes_string(x=xaxis, y="Num", fill="VKClassesForPlot")
  p <- ggplot2::ggplot(class_grp, map)+
    ggplot2::geom_bar(stat="identity",position="fill")+
    ggplot2::scale_fill_manual(values=col_vec)+
    ggplot2::theme_bw()+
    ggplot2::labs(x=xaxis, y=ylabel, title=title)+
    guides(fill = guide_legend(title="Chemical Classes"))
  
  if(length(unique(as.data.frame(class_grp)[,xaxis])) > 10){
    p <- p + ggplot2::theme(axis.text.x = element_text(angle = 90))
  }
  
  plotly::ggplotly(p)
  
}

