## Test Van Krevelen plots

library(fticRanalysis)

# setwd("~/Files/MinT/github/fticRanalysis")
# load_all()

data(peakIcrProcessed)
picr <- peakIcrProcessed
rm(peakIcrProcessed)

vanKrevelenPlot(picr, title="Test1", colorPal=NA, colorCName=NA, vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Van Krevelen Category")

vanKrevelenPlot(picr, title="Test2", colorPal=NA, colorCName="kdefect", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Defect")

vanKrevelenPlot(picr, title="Test3", colorPal=scales::col_numeric("YlGnBu", domain=c(0,1)), colorCName="kdefect", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Defect")

picr$e_meta$random_factor_1 <- factor(paste("Category", sample(1:5, size=nrow(picr$e_meta), replace=TRUE)))
vanKrevelenPlot(picr, title="Test4", colorPal=NA, colorCName="random_factor_1", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_factor_1")

picr$e_meta$random_factor_2 <- factor(paste("Category", sample(1:12, size=nrow(picr$e_meta), replace=TRUE)))
vanKrevelenPlot(picr, title="Test5", colorPal=NA, colorCName="random_factor_2", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_factor_2")

picr$e_meta$random_char_1 <- paste("Char category", sample(1:5, size=nrow(picr$e_meta), replace=TRUE))
vanKrevelenPlot(picr, title="Test6", colorPal=NA, colorCName="random_char_1", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_char_1")

picr$e_meta$random_char_2 <- paste("Category", sample(1:12, size=nrow(picr$e_meta), replace=TRUE))
cpal <- scales::col_factor("YlOrRd", paste("Category", 1:12))
vanKrevelenPlot(picr, title="Test7", colorPal=cpal, colorCName="random_char_2", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_char_2")

picr$e_meta$random_factor_2 <- factor(picr$e_meta$random_factor_2, levels=paste("Category", 1:12))
cpal <- scales::col_factor("YlOrRd", levels=levels(picr$e_meta$random_factor_2))
vanKrevelenPlot(picr, title="Test8", colorPal=cpal, colorCName="random_factor_2", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="random_factor_2")

cpal <- scales::col_factor("YlOrRd", levels=levels(picr$e_meta$random_factor_2))
vanKrevelenPlot(picr, title="Test9", colorPal=cpal, colorCName="random_factor_2", vkBoundarySet="bs1",
                showVKBounds=FALSE, legendTitle="random_factor_2")

cpal <- scales::col_factor("Set3", levels=rownames(getVanKrevelenCategoryBounds("bs1")))
vanKrevelenPlot(picr, title="Test10", colorPal=cpal, colorCName=NA, vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="")

vanKrevelenPlot(picr, title="Test11", colorPal=NA, colorCName=NA, vkBoundarySet="bs1",
                showVKBounds=FALSE, legendTitle="Van Krevelen Category")

vanKrevelenPlot(picr, title="Test12", colorPal=cpal, colorCName=NA, vkBoundarySet="bs1",
                showVKBounds=FALSE, legendTitle="")

## This should fail: color palette does not match colorCName data type
vanKrevelenPlot(picr, title="Test13", colorPal=cpal, colorCName="kdefect", vkBoundarySet="bs1",
                showVKBounds=FALSE, legendTitle="")

picrSubset <- subset(edata_transform(picr, "log"), samples="EW0161_sample")
vanKrevelenPlot(picrSubset, title="Test14", colorCName="Intensity", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Intensity")

## this should automatically log transform the data, look for a message on the console:
picrSubset2 <- subset(picr, samples="EW0161_sample")
vanKrevelenPlot(picrSubset2, title="Test15", colorCName="Intensity", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Intensity")

msGroup <- subset(picr, groups="M_S")
msGroup <- summarizeGroups(msGroup, summary_functions = list("n_present", "prop_present"))
groupVanKrevelenPlot(msGroup, title="Test16", colorCName="prop_present", vkBoundarySet="bs1",
                showVKBounds=TRUE, legendTitle="Proportion<br>Present")
