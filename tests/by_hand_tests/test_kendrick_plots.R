## Test Kendrick plots

library(fticRanalysis)

data(peakIcrProcessed)
picr <- peakIcrProcessed
rm(peakIcrProcessed)


kendrickPlot(picr, title="Test1", colorPal=NA, colorCName=NA, vkBoundarySet="bs1")

kendrickPlot(picr, title="Test2", colorPal=NA, colorCName="kdefect", vkBoundarySet="bs1", legendTitle = "Defect")

kendrickPlot(picr, title="Test3", colorPal=scales::col_numeric("YlGnBu", domain=c(0,1)), colorCName="kdefect", vkBoundarySet="bs1",
                legendTitle="Defect")

picr$e_meta$random_factor_1 <- factor(paste("Category", sample(1:5, size=nrow(picr$e_meta), replace=TRUE)))
kendrickPlot(picr, title="Test4", colorPal=NA, colorCName="random_factor_1", vkBoundarySet="bs1",
                legendTitle="random_factor_1")

picr$e_meta$random_factor_2 <- factor(paste("Category", sample(1:12, size=nrow(picr$e_meta), replace=TRUE)))
kendrickPlot(picr, title="Test5", colorPal=NA, colorCName="random_factor_2", vkBoundarySet="bs1",
                legendTitle="random_factor_2")

picr$e_meta$random_char_1 <- paste("Char category", sample(1:5, size=nrow(picr$e_meta), replace=TRUE))
kendrickPlot(picr, title="Test6", colorPal=NA, colorCName="random_char_1", vkBoundarySet="bs1",
                legendTitle="random_char_1")

picr$e_meta$random_char_2 <- paste("Category", sample(1:12, size=nrow(picr$e_meta), replace=TRUE))
cpal <- scales::col_factor("YlOrRd", paste("Category", 1:12))
kendrickPlot(picr, title="Test7", colorPal=cpal, colorCName="random_char_2", vkBoundarySet="bs1",
                legendTitle="random_char_2")

picr$e_meta$random_factor_2 <- factor(picr$e_meta$random_factor_2, levels=paste("Category", 1:12))
cpal <- scales::col_factor("YlOrRd", levels=levels(picr$e_meta$random_factor_2))
kendrickPlot(picr, title="Test8", colorPal=cpal, colorCName="random_factor_2", vkBoundarySet="bs1",
                legendTitle="random_factor_2")

cpal <- scales::col_factor("YlOrRd", levels=levels(picr$e_meta$random_factor_2))
kendrickPlot(picr, title="Test9", colorPal=cpal, colorCName="random_factor_2", vkBoundarySet="bs1",
                legendTitle="random_factor_2")

cpal <- scales::col_factor("Set3", levels=rownames(getVanKrevelenCategoryBounds("bs1")))
kendrickPlot(picr, title="Test10", colorPal=cpal, colorCName=NA, vkBoundarySet="bs1",
                legendTitle="")

kendrickPlot(picr, title="Test11", colorPal=NA, colorCName=NA, vkBoundarySet="bs1",
                legendTitle="Van Krevelen Category")

kendrickPlot(picr, title="Test12", colorPal=cpal, colorCName=NA, vkBoundarySet="bs1",
                legendTitle="")

## This should fail: color palette does not match colorCName data type
kendrickPlot(picr, title="Test13", colorPal=cpal, colorCName="kdefect", vkBoundarySet="bs1",
                legendTitle="")


picrSubset <- subset(edata_transform(picr, "log"), samples="EW0161_sample")
kendrickPlot(picrSubset, title="Test14", colorCName="Intensity", legendTitle="Intensity")

msGroup <- subset(picr, groups="M_S")
msGroup <- summarizeGroups(msGroup, summary_functions = list("n_present", "prop_present"))
groupKendrickPlot(msGroup, title="Test15", colorCName="prop_present", legendTitle="Proportion<br>Present")
