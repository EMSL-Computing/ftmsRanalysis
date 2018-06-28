## Tests of creating trelliscope displays with the convenience functions in fticRanalysis

library(fticRanalysis)
library(trelliscope)

vdbDir <- vdbConn(file.path(tempdir(), "trell_test"), autoYes = TRUE)

data('peakIcrProcessed')

##########################################################
## SAMPLE PLOTS

sampleDdo <- divideBySample(peakIcrProcessed)

## TEST S-1: VK plot, color by VK category
panelFnS1 <- panelFunctionGenerator("vanKrevelenPlot", vkBoundarySet="bs2", title="Test")

makeDisplay(sampleDdo, 
            panelFn=panelFnS1,
            name = "Trelliscope test S_1 with VK",
            group = "Sample")


## TEST S-2: Kendrick plot, color by VK category
panelFnS2 <- panelFunctionGenerator("kendrickPlot", vkBoundarySet="bs1")

makeDisplay(sampleDdo, 
            panelFn=panelFnS2,
            name = "Trelliscope test S_2 with Kendrick",
            group = "Sample")


## TEST S-3: VK plot, color by Intensity
panelFnS3 <- panelFunctionGenerator("vanKrevelenPlot", colorCName="Intensity")

makeDisplay(sampleDdo, 
            panelFn=panelFnS3,
            name = "Trelliscope test S_3 with VK",
            group = "Sample")


## TEST 4: densityPlot of NOSC
panelFn4 <- panelFunctionGenerator("densityPlot", variable="NOSC")

makeDisplay(sampleDdo, 
            panelFn=panelFn4,
            name = "Trelliscope test 4 with density",
            group = "Sample")

##########################################################
## GROUP PLOTS

groupDdo <- divideByGroup(peakIcrProcessed)
groupSummaryDdo <- summarizeGroups(groupDdo, summary_functions = c("prop_present", "n_present"))

## TEST G-1: VK group plot, color by proportion present
panelFnG1 <- panelFunctionGenerator("vanKrevelenPlot", colorCName=expr(paste0(getSplitVar(v, "Group"), "_prop_present")), 
                                   legendTitle="Proportion<br>Present")

makeDisplay(groupSummaryDdo, 
            panelFn=panelFnG1,
            name = "Trelliscope test G_1 with VK plot per group",
            group = "Group")


## TEST G-2: Kendrick group plot, color by n present
panelFnG2 <- panelFunctionGenerator("kendrickPlot", colorCName=expr(paste0(getSplitVar(v, "Group"), "_n_present")), 
                                   legendTitle="Number<br>Present")

makeDisplay(groupSummaryDdo, 
            panelFn=panelFnG2,
            name = "Trelliscope test G_2 with Kendrick plot per group",
            group = "Group")


## TEST G-3: classes plot for each group
panelFnG3 <- panelFunctionGenerator("classesPlot")

makeDisplay(groupDdo, 
            panelFn=panelFnG3,
            name = "Trelliscope test G_3 with classesPlot",
            group = "Group")


view()



