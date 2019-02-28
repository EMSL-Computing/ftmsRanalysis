## Test plot.peakData

library(fticRanalysis)
data(examplePeakData)
data(exampleProcessedPeakData)

data1 <- exampleProcessedPeakData
data2 <- edata_transform(data1, "log2")
data3 <- edata_transform(data1, "pres")

data4 <- exampleProcessedPeakData
data5 <- edata_transform(data4, "log")
data6 <- edata_transform(data4, "pres")

# abundance boxplots, no groups
plot(data1, title="Test1")

# log2 abundance boxplots, no groups
plot(data2, title="Test2")

# presence barplots, no groups
plot(data3, title="Test3")

# abundance boxplots, with groups
plot(data4, title="Test4", colorBy="groups")

# log2 abundance boxplots, no groups
plot(data5, title="Test5")

# presence barplots, no groups
plot(data3, title="Test6")

# presence barplots, with groups
plot(data6, title="Test7")

# presence barplots colored according to molecular form
plot(data6, title="Test8", colorBy="molform")

# presence barplots on data with group info, but colored according to molecular form
plot(data3, title="Test9", colorBy="molform")

# suppress coloring by groups
plot(data4, title="Test10", colorBy=NA)

# test custom axis labels
plot(data4, title="Test11", xlabel = "Sample Name", ylabel="You should really log transform this")
