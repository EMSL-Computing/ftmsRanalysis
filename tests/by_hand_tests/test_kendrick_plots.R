## Test Kendrick plots

library(ftmsRanalysis)

data(exampleProcessedPeakData)
peakObj <- exampleProcessedPeakData
rm(exampleProcessedPeakData)


kendrickPlot(peakObj, title="Test1", colorPal=NA, colorCName=NA, vkBoundarySet="bs1")

kendrickPlot(peakObj, title="Test2", colorPal=NA, colorCName="kdefect", vkBoundarySet="bs1", legendTitle = "Defect")

kendrickPlot(peakObj, title="Test3", colorPal=scales::col_numeric("YlGnBu", domain=c(0,1)), colorCName="kdefect", vkBoundarySet="bs1",
                legendTitle="Defect")

peakObj$e_meta$random_factor_1 <- factor(paste("Category", sample(1:5, size=nrow(peakObj$e_meta), replace=TRUE)))
kendrickPlot(peakObj, title="Test4", colorPal=NA, colorCName="random_factor_1", vkBoundarySet="bs1",
                legendTitle="random_factor_1")

peakObj$e_meta$random_factor_2 <- factor(paste("Category", sample(1:12, size=nrow(peakObj$e_meta), replace=TRUE)))
kendrickPlot(peakObj, title="Test5", colorPal=NA, colorCName="random_factor_2", vkBoundarySet="bs1",
                legendTitle="random_factor_2")

peakObj$e_meta$random_char_1 <- paste("Char category", sample(1:5, size=nrow(peakObj$e_meta), replace=TRUE))
kendrickPlot(peakObj, title="Test6", colorPal=NA, colorCName="random_char_1", vkBoundarySet="bs1",
                legendTitle="random_char_1")

peakObj$e_meta$random_char_2 <- paste("Category", sample(1:12, size=nrow(peakObj$e_meta), replace=TRUE))
cpal <- scales::col_factor("YlOrRd", paste("Category", 1:12))
kendrickPlot(peakObj, title="Test7", colorPal=cpal, colorCName="random_char_2", vkBoundarySet="bs1",
                legendTitle="random_char_2")

peakObj$e_meta$random_factor_2 <- factor(peakObj$e_meta$random_factor_2, levels=paste("Category", 1:12))
cpal <- scales::col_factor("YlOrRd", levels=levels(peakObj$e_meta$random_factor_2))
kendrickPlot(peakObj, title="Test8", colorPal=cpal, colorCName="random_factor_2", vkBoundarySet="bs1",
                legendTitle="random_factor_2")

cpal <- scales::col_factor("YlOrRd", levels=levels(peakObj$e_meta$random_factor_2))
kendrickPlot(peakObj, title="Test9", colorPal=cpal, colorCName="random_factor_2", vkBoundarySet="bs1",
                legendTitle="random_factor_2")

cpal <- scales::col_factor("Set3", levels=rownames(getVanKrevelenCategoryBounds("bs1")$VKbounds))
kendrickPlot(peakObj, title="Test10", colorPal=cpal, colorCName=NA, vkBoundarySet="bs1",
                legendTitle="")

kendrickPlot(peakObj, title="Test11", colorPal=NA, colorCName=NA, vkBoundarySet="bs1",
                legendTitle="Van Krevelen Category")

kendrickPlot(peakObj, title="Test12", colorPal=cpal, colorCName=NA, vkBoundarySet="bs1",
                legendTitle="")

# color by column in e_data
kendrickPlot(peakObj, colorCName="EM0011_sample", vkBoundarySet="bs1",
                legendTitle="Log EM0011<br>Abundance")

## This should fail: color palette does not match colorCName data type
kendrickPlot(peakObj, title="Test13", colorPal=cpal, colorCName="kdefect", vkBoundarySet="bs1",
                legendTitle="")


# Use "Intensity" keyword for colorCName, but data is already logged
peakSubset <- subset(edata_transform(peakObj, "log"), samples="EW0161_sample")
kendrickPlot(peakSubset, title="Test14", colorCName="Intensity", legendTitle="Log (already)<br>Intensity")

## this should automatically log transform the data, look for a message on the console:
peakSubset2 <- subset(peakObj, samples="EW0161_sample")
kendrickPlot(peakSubset2, title="Test15", colorCName="Intensity")


msGroup <- subset(peakObj, groups="M_S")
msGroup <- summarizeGroups(msGroup, summary_functions = list("n_present", "prop_present"))

# legend should show integers
kendrickPlot(msGroup, title="Test16", colorCName="M_S_n_present", legendTitle="Number<br>Present")

# continuous color bar:
kendrickPlot(msGroup, title="Test17", colorCName="M_S_prop_present", legendTitle="Proportion<br>Present")

# group overlay plot
peakGroupComp <- divideByGroupComparisons(peakObj, comparisons="one-factor")
peakCompSummary <- summarizeGroupComparisons(peakGroupComp[[1]], summary_functions="uniqueness_gtest", 
                                             summary_function_params=list(
                                               uniqueness_gtest=list(pres_fn="prop", pres_thresh=0.2,
                                                                     pvalue_thresh=0.05)
                                             ))
kendrickPlot(peakCompSummary, title="Test18", colorPal=NA, colorCName="uniqueness_gtest", 
                             xlabel="O:C Ratio", ylabel="H:C Ratio") 

