#' Plot to look at the percentage of each class in the data
#' 
#' Constructs stacked bar plot to look at the percentage of each class in the data
#' 
#' @param icrData icrData object of class peakData or compoundData
#' @param xaxis x axis variable. If NULL, will use attr(icrData, "cnames")$fdata_cname. Must be one of
#'                colnames(icrData$f_data) or colnames(attr(icrData, "group_DF")).
#' @param ylabel y axis label, default is "Density"
#' @param title plot title, default is NULL
#' @param vkBoundarySet character vector specifying which boundary set to use when determining class. Valid options are currently "bs1" and "bs2" and defaults to "bs1". See details of \code{\link{assign_class}} for differences in sets.
#' 
#' @return a plotly object
#' 
#' @seealso \code{\link{plot_ly}}
#' 
#' @author Allison Thompson
#' 
#'
classesPlot <- function(icrData, xaxis=NULL, ylabel="Percentage of Chemical Classes", title=NULL, vkBoundarySet="bs1", classColName=NULL) {
  
  # Initial Checks #
  # check that icrData is of the correct class #
  if(!inherits(icrData, "peakData") & !inherits(icrData, "compoundData")) stop("icrData must be an object of class 'peakData' or 'compoundData'")
  
  
  if(is.null(xaxis)){
    xaxis <- fticRanalysis:::getFDataColName(icrData)
  }
  
  if(!(xaxis %in% c(colnames(icrData$f_data), colnames(attr(icrData, "group_DF"))))){
    stop("xaxis must be one of the column names of icrData$f_data or attr(icrData, 'group_DF').")
  }
  
  # End Initial Checks #
  
  # calculate the number of peaks per sample
  classes <- icrData$e_data
  
  if(is.null(classColName) & !is.null(vkBoundarySet)){
    if(!(vkBoundarySet) %in% c("bs1","bs2")){ stop("vkBoundarySet must be one of 'bs1' or 'bs2'.") }
    classes$VKClassesForPlot <- fticRanalysis:::assign_class(icrData, vkBoundarySet)$e_meta[,paste(vkBoundarySet,"class",sep="_")]
    classes$VKClassesForPlot <- gsub(";.*", "",classes$VKClassesForPlot)
    classes$VKClassesForPlot <- factor(classes$VKClassesForPlot, levels=rownames(fticRanalysis:::getVanKrevelenCategoryBounds(vkBoundarySet)$VKbounds))
  }else if(!is.null(classColName)){
    if(!(classColName) %in% colnames(icrData$e_meta)){ stop("classColName not found in icrData$e_meta.") }
    classes$VKClassesForPlot <- icrData$e_meta[match(icrData$e_meta[,fticRanalysis:::getMassColName(icrData)], classes[,fticRanalysis:::getMassColName(icrData)]), classColName]
  }else{
    stop("Unsure what to use to color by, please specify either 'vkBoundarySet' or 'classColName'")
  }
  
  classes <- reshape2::melt(classes)
  colnames(classes)[which(colnames(classes) == "variable")] <- fticRanalysis:::getFDataColName(icrData)
  classes <- merge(classes, icrData$e_meta, by=getMassColName(icrData))
  
  # remove unassigned 
  if(any(is.na(classes[,fticRanalysis:::getMFColName(icrData)]))){
    classes <- classes[-which(is.na(classes[,fticRanalysis:::getMFColName(icrData)])),]
    classes <- droplevels(classes)
  }
  
  # merge peaks with metadata or group_DF
  if(xaxis %in% colnames(attr(icrData, "group_DF"))){
    classes <- merge(classes, attr(icrData, "group_DF"), by=fticRanalysis:::getFDataColName(icrData))
  }else{
    classes <- merge(classes, icrData$f_data, by=fticRanalysis:::getFDataColName(icrData)) 
  }
  
  if(xaxis == getFDataColName(icrData)){
    # summarise to percentage in each class
    vars1 <- c(getFDataColName(icrData), "VKClassesForPlot")
    vars1 <- lapply(vars1, as.symbol)
  
    class_grp <- classes %>% dplyr::group_by_(.dots=vars1) %>%
      dplyr::summarise(Num=length(which(value > 0 & !is.na(value))))
    

  }else{
    # if grouping, take median per group
    vars1 <- c(getFDataColName(icrData), xaxis, "VKClassesForPlot")
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
  
  colorPal <- fticRanalysis:::getFactorColorPalette(cc)  
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

