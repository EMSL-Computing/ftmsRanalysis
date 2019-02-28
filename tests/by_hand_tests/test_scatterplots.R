## Test scatter plot function

library(fticRanalysis)

data(exampleProcessedPeakData)

ftmsObj <- exampleProcessedPeakData
xCName <- "NOSC"
yCName <- "DBE"
colorCName <- "HtoC_ratio"

# basic scatterplot
scatterPlot(ftmsObj, xCName, yCName, colorCName)

# try setting the x and y ranges and hover info
ftmsObj$e_meta$Hover <- sprintf("H:C Ratio: %.4f", ftmsObj$e_meta$HtoC_ratio)
scatterPlot(ftmsObj, xCName, yCName, colorCName, xrange=c(-2, 2), yrange=c(-35, 35), hoverTextCName="Hover")

# custom color palette, legend title, and plot title
colorPal <- scales::col_numeric("YlGnBu", domain = range(ftmsObj$e_meta[, colorCName], na.rm=TRUE))
scatterPlot(ftmsObj, xCName, yCName, colorCName, colorPal = colorPal, legendTitle="H:C Ratio", title="Test")

# look at how NAs in colorCName are handled:
scatterPlot(ftmsObj, "kmass", "kdefect", colorCName, colorPal = colorPal, legendTitle="H:C Ratio", title="Test")

# color by an integer valued column
scatterPlot(ftmsObj, xCName, yCName, colorCName="N")

# color by a factor
ftmsObj <- assign_class(ftmsObj)
ftmsObj$e_meta$Class <- gsub(";.*", "", ftmsObj$e_meta$bs1_class)
scatterPlot(ftmsObj, xCName, yCName, colorCName="Class", xlabel = "My X Label", ylabel="My Y Label")

# don't specify a color column
scatterPlot(ftmsObj, xCName, yCName)

# custom constant color for points
scatterPlot(ftmsObj, xCName, yCName, colorPal="#FF0000")

# log transforming vs not: look at the difference in color bar range
scatterPlot(ftmsObj, xCName, yCName, colorCName="EM0013_sample", logColorCol = TRUE, legendTitle="EM0013 Log<br>Abundance")
scatterPlot(ftmsObj, xCName, yCName, colorCName="EM0013_sample", logColorCol = FALSE, legendTitle="EM0013<br>Abundance")

# things that should fail
scatterPlot(ftmsObj, "not a real column", yCName, colorPal="#FF0000")
scatterPlot(ftmsObj, xCName, "yCName", colorPal="#FF0000")
scatterPlot(ftmsObj, xCName, "Class", colorPal="#FF0000")
scatterPlot(ftmsObj, "Class", yCName, colorPal="#FF0000")


