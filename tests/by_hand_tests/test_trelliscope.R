## Tests of creating trelliscope displays with the convenience functions in fticRanalysis

library(fticRanalysis)
library(trelliscope)

vdbDir <- vdbConn(file.path(tempdir(), "trell_test"), autoYes = TRUE)

data('peakIcrProcessed')

##########################################################
## SAMPLE PLOTS

sampleDdo <- divideBySample(peakIcrProcessed)

## TEST 1: VK plot, color by VK category
panelFn1 <- panelFunctionGenerator("vanKrevelenPlot", vkBoundarySet="bs2", title="Test")

makeDisplay(sampleDdo, 
            panelFn=panelFn1,
            name = "Trelliscope test 1 with VK",
            group = "Sample")
view()

## TEST 2: Kendrick plot, color by VK category
panelFn2 <- panelFunctionGenerator("kendrickPlot", vkBoundarySet="bs1")

makeDisplay(sampleDdo, 
            panelFn=panelFn2,
            name = "Trelliscope test 2 with Kendrick",
            group = "Sample")
view()

## TEST 3: VK plot, color by Intensity
panelFn3 <- panelFunctionGenerator("vanKrevelenPlot", colorCName="Intensity")

makeDisplay(sampleDdo, 
            panelFn=panelFn3,
            name = "Trelliscope test 3 with VK",
            group = "Sample")
view()


## TEST 4: compoundCalcsPlot of NOSC
panelFn4 <- panelFunctionGenerator("compoundCalcsPlot", variable="NOSC")

makeDisplay(sampleDdo, 
            panelFn=panelFn4,
            name = "Trelliscope test 4 with compoundCalcsPlot",
            group = "Sample")
view()

##########################################################
## GROUP PLOTS

groupDdo <- divideByGroup(peakIcrProcessed)
groupDdo <- summarizeGroups(groupDdo, summary_functions = c("prop_present", "n_present"))

## TEST 5: VK plot, color by proportion present
panelFn5 <- panelFunctionGenerator("groupVanKrevelenPlot", colorCName="prop_present", 
                                   legendTitle="Proportion<br>Present")

makeDisplay(groupDdo, 
            panelFn=panelFn5,
            name = "Trelliscope test 5 with VK plot per group",
            group = "Group")
view()


## TEST 6: Kendrick plot, color by n present
panelFn6 <- panelFunctionGenerator("groupKendrickPlot", colorCName="n_present", 
                                   legendTitle="Number<br>Present")

makeDisplay(groupDdo, 
            panelFn=panelFn6,
            name = "Trelliscope test 5 with Kendrick plot per group",
            group = "Group")
view()

