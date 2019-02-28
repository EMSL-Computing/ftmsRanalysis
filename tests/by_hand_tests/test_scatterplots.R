## Test scatter plot function

library(fticRanalysis)

data(exampleProcessedPeakData)

icrData <- exampleProcessedPeakData
xCName <- "NOSC"
yCName <- "DBE"
colorCName <- "HtoC_ratio"

# basic scatterplot
scatterPlot(icrData, xCName, yCName, colorCName)

# try setting the x and y ranges and hover info
icrData$e_meta$Hover <- sprintf("H:C Ratio: %.4f", icrData$e_meta$HtoC_ratio)
scatterPlot(icrData, xCName, yCName, colorCName, xrange=c(-2, 2), yrange=c(-35, 35), hoverTextCName="Hover")

# custom color palette, legend title, and plot title
colorPal <- scales::col_numeric("YlGnBu", domain = range(icrData$e_meta[, colorCName], na.rm=TRUE))
scatterPlot(icrData, xCName, yCName, colorCName, colorPal = colorPal, legendTitle="H:C Ratio", title="Test")

# look at how NAs in colorCName are handled:
scatterPlot(icrData, "kmass", "kdefect", colorCName, colorPal = colorPal, legendTitle="H:C Ratio", title="Test")

# color by an integer valued column
scatterPlot(icrData, xCName, yCName, colorCName="N")

# color by a factor
icrData <- assign_class(icrData)
icrData$e_meta$Class <- gsub(";.*", "", icrData$e_meta$bs1_class)
scatterPlot(icrData, xCName, yCName, colorCName="Class", xlabel = "My X Label", ylabel="My Y Label")

# don't specify a color column
scatterPlot(icrData, xCName, yCName)

# custom constant color for points
scatterPlot(icrData, xCName, yCName, colorPal="#FF0000")

# log transforming vs not: look at the difference in color bar range
scatterPlot(icrData, xCName, yCName, colorCName="EM0013_sample", logColorCol = TRUE, legendTitle="EM0013 Log<br>Abundance")
scatterPlot(icrData, xCName, yCName, colorCName="EM0013_sample", logColorCol = FALSE, legendTitle="EM0013<br>Abundance")

# things that should fail
scatterPlot(icrData, "not a real column", yCName, colorPal="#FF0000")
scatterPlot(icrData, xCName, "yCName", colorPal="#FF0000")
scatterPlot(icrData, xCName, "Class", colorPal="#FF0000")
scatterPlot(icrData, "Class", yCName, colorPal="#FF0000")


