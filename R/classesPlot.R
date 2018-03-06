#' Plot to look at the percentage of each class in the data
#' 
#' Constructs stacked bar plot to look at the percentage of each class in the data
#' 
#' @param dataObj icrData object
#' @param xaxis x axis variable. If NULL, will use attr(dataObj, "cnames")$fdata_cname. Must be one of
#'                colnames(dataObj$f_data) or colnames(attr(dataObj, "group_DF")).
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
#' @export
classesPlot <- function(dataObj, xaxis=NULL, ylabel="Percentage of Chemical Classes", title=NULL, vkBoundarySet="bs1", classColName=NULL) {
  
  # Initial Checks #
  if (!inherits(dataObj, "icrData")) {
    stop("dataObj must be of type icrData")
  }
  
  if(is.null(xaxis)){
    xaxis <- getFDataColName(dataObj)
  }
  
  if(!(xaxis %in% c(colnames(dataObj$f_data), colnames(attr(dataObj, "group_DF"))))){
    stop("xaxis must be one of the column names of dataObj$f_data or attr(dataObj, 'group_DF').")
  }
  
  # End Initial Checks #
  
  # calculate the number of peaks per sample
  classes <- dataObj$e_data
  
  if(is.null(classColName) & !is.null(vkBoundarySet)){
    if(!(vkBoundarySet) %in% c("bs1","bs2")){ stop("vkBoundarySet must be one of 'bs1' or 'bs2'.") }
    classes$VKClassesForPlot <- factor(fticRanalysis:::getVanKrevelenCategories(dataObj, vkBoundarySet), levels=rownames(fticRanalysis:::getVanKrevelenCategoryBounds(vkBoundarySet)))
  }else if(!is.null(classColName)){
    if(!(classColName) %in% colnames(dataObj$e_meta)){ stop("classColName not found in dataObj$e_meta.") }
    classes$VKClassesForPlot <- dataObj$e_meta[match(dataObj$e_meta[,getMassColName(dataObj)], classes[,getMassColName(dataObj)]), classColName]
  }else{
    stop("Unsure what to use to color by, please specify either 'vkBoundarySet' or 'classColName'")
  }
  
  classes <- melt(classes)
  colnames(classes)[which(colnames(classes) == "variable")] <- getFDataColName(dataObj)
  classes <- merge(classes, dataObj$e_meta, by=getMassColName(dataObj))
  
  # remove unassigned 
  if(any(is.na(classes[,getMFColName(dataObj)]))){
    classes <- classes[-which(is.na(classes[,getMFColName(dataObj)])),]
    classes <- droplevels(classes)
  }
  
  # merge peaks with metadata or group_DF
  if(xaxis %in% colnames(attr(dataObj, "group_DF"))){
    classes <- merge(classes, attr(dataObj, "group_DF"), by=getFDataColName(dataObj))
  }else{
    classes <- merge(classes, dataObj$f_data, by=getFDataColName(dataObj)) 
  }
  
  if(xaxis == getFDataColName(dataObj)){
    # summarise to percentage in each class
    vars1 <- c(getFDataColName(dataObj), "VKClassesForPlot")
    vars1 <- lapply(vars1, as.symbol)
  
    class_grp <- classes %>% dplyr::group_by_(.dots=vars1) %>%
      dplyr::summarise(Num=length(which(value > 0 & !is.na(value))))
    

  }else{
    # if grouping, take median per group
    vars1 <- c(getFDataColName(dataObj), xaxis, "VKClassesForPlot")
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
  
  colorPal <- getFactorColorPalette(cc)  
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

