## Test plot.peakIcrData

library(fticRanalysis)
data(peakIcrData)
data(peakIcrProcessed)

data1 <- peakIcrData
data2 <- edata_transform(data1, "log2")
data3 <- edata_transform(data1, "pres")

data4 <- peakIcrProcessed
data5 <- edata_transform(data4, "log")
data6 <- edata_transform(data4, "pres")

# abundance boxplots, no groups
plot(data1, title="Test1")

# log2 abundance boxplots, no groups
plot(data2, title="Test2")

# presence barplots, no groups
plot(data3, title="Test3")

# abundance boxplots, with groups
plot(data4, title="Test4")

# log2 abundance boxplots, no groups
plot(data5, title="Test5")

# presence barplots, no groups
plot(data6, title="Test6")

# test custom axis labels
plot(data4, title="Test7", xlabel = "Sample Name", ylabel="You should really log transform this")
