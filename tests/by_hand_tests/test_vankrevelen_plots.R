## Test Van Krevelen plots

library(ftmsRanalysis)

# setwd("~/Files/MinT/github/ftmsRanalysis")
# load_all()

data(exampleProcessedPeakData)
peakObj <- exampleProcessedPeakData
rm(exampleProcessedPeakData)

vanKrevelenPlot(peakObj, title="Test1", colorPal=NA, colorCName=NA, vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Van Krevelen Category")

vanKrevelenPlot(peakObj, title="Test2", colorPal=NA, colorCName="kdefect", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Defect")

vanKrevelenPlot(peakObj, title="Test3", colorPal=scales::col_numeric("YlGnBu", domain=c(0,1)), colorCName="kdefect", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Defect")

peakObj$e_meta$random_factor_1 <- factor(paste("Category", sample(1:5, size=nrow(peakObj$e_meta), replace=TRUE)))
vanKrevelenPlot(peakObj, title="Test4", colorPal=NA, colorCName="random_factor_1", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_factor_1")

peakObj$e_meta$random_factor_2 <- factor(paste("Category", sample(1:12, size=nrow(peakObj$e_meta), replace=TRUE)))
vanKrevelenPlot(peakObj, title="Test5", colorPal=NA, colorCName="random_factor_2", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_factor_2")

peakObj$e_meta$random_char_1 <- paste("Char category", sample(1:5, size=nrow(peakObj$e_meta), replace=TRUE))
vanKrevelenPlot(peakObj, title="Test6", colorPal=NA, colorCName="random_char_1", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_char_1")

peakObj$e_meta$random_char_2 <- paste("Category", sample(1:12, size=nrow(peakObj$e_meta), replace=TRUE))
cpal <- scales::col_factor("YlOrRd", paste("Category", 1:12))
vanKrevelenPlot(peakObj, title="Test7", colorPal=cpal, colorCName="random_char_2", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_char_2")

peakObj$e_meta$random_factor_2 <- factor(peakObj$e_meta$random_factor_2, levels=paste("Category", 1:12))
cpal <- scales::col_factor("YlOrRd", levels=levels(peakObj$e_meta$random_factor_2))
vanKrevelenPlot(peakObj, title="Test8", colorPal=cpal, colorCName="random_factor_2", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_factor_2")

cpal <- scales::col_factor("YlOrRd", levels=levels(peakObj$e_meta$random_factor_2))
vanKrevelenPlot(peakObj, title="Test9", colorPal=cpal, colorCName="random_factor_2", vkBoundarySet="bs1",
                showVKBounds=FALSE, legendTitle="random_factor_2")

cpal <- scales::col_factor("Set3", levels=rownames(getVanKrevelenCategoryBounds("bs1")$VKbounds))
vanKrevelenPlot(peakObj, title="Test10", colorPal=cpal, colorCName=NA, vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="")

vanKrevelenPlot(peakObj, title="Test11", colorPal=NA, colorCName=NA, vkBoundarySet="bs1",
                showVKBounds=FALSE, legendTitle="Van Krevelen Category")

vanKrevelenPlot(peakObj, title="Test12", colorPal=cpal, colorCName=NA, vkBoundarySet="bs1",
                showVKBounds=FALSE, legendTitle="")

# color by column in e_data
vanKrevelenPlot(peakObj, colorCName="EM0011_sample", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Log EM0011<br>Abundance")

## This should fail: color palette does not match colorCName data type
vanKrevelenPlot(peakObj, title="Test13", colorPal=cpal, colorCName="kdefect", vkBoundarySet="bs1",
                showVKBounds=FALSE, legendTitle="")

# Use "Intensity" keyword for colorCName, but data is already logged
peakObj2 <- subset(edata_transform(peakObj, "log"), samples="EW0161_sample")
vanKrevelenPlot(peakObj2, title="Test14", colorCName="Intensity", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Log (already)<br>Intensity")

## this should automatically log transform the data, look for a message on the console:
peakSubset2 <- subset(peakObj, samples="EW0161_sample")
vanKrevelenPlot(peakSubset2, title="Test15", colorCName="Intensity", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Intensity")

# make sure legend has integer values to select/deselect:
msGroup <- subset(peakObj, groups="M_S")
msGroup <- summarizeGroups(msGroup, summary_functions = c("n_present", "prop_present"))
vanKrevelenPlot(msGroup, title="Test16", colorCName="M_S_n_present", vkBoundarySet="bs1",
                           showVKBounds=TRUE, legendTitle="Count<br>Present")

# regular continuous color bar
vanKrevelenPlot(msGroup, title="Test17", colorCName="M_S_prop_present", vkBoundarySet="bs1",
                           showVKBounds=TRUE, legendTitle="Proportion<br>Present")

# group overlay plot
peakGroupComp <- divideByGroupComparisons(peakObj, comparisons="one-factor")
peakCompSummary <- summarizeGroupComparisons(peakGroupComp[[1]], summary_functions="uniqueness_gtest", 
                                             summary_function_params=list(
                                               uniqueness_gtest=list(pres_fn="prop", pres_thresh=0.2,
                                                                     pvalue_thresh=0.05)
                                             ))
vanKrevelenPlot(peakCompSummary
               , title="Test18", colorPal=NA, colorCName="uniqueness_gtest", vkBoundarySet = "bs1", showVKBounds=TRUE, 
                                xlabel="O:C Ratio", ylabel="H:C Ratio") 

