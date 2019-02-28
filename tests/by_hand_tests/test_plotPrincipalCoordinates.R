# Tests of plotPrincipalCoordinates function

library(fticRanalysis)
data("exampleProcessedPeakData")

pcoaMat <- getPrincipalCoordinates(exampleProcessedPeakData)

# default plot
plotPrincipalCoordinates(pcoaMat)

# default plot colored by groups
plotPrincipalCoordinates(pcoaMat, icrData=exampleProcessedPeakData)

# different coordinate axes
plotPrincipalCoordinates(pcoaMat, x=1, y=3)

# different coordinate axes colored by groups
plotPrincipalCoordinates(pcoaMat, x=3, y=4, icrData=exampleProcessedPeakData)

# customize axes labels and title
plotPrincipalCoordinates(pcoaMat, xlabel="Custom x-axis label", ylabel="Custom y-axis label", title="My Title")

# custom axis label without R^2   
plotPrincipalCoordinates(pcoaMat, xlabel="Custom x-axis label", includeR2OnAxes = FALSE)


#should fail: x is too high
plotPrincipalCoordinates(pcoaMat, x=7, y=3, icrData=exampleProcessedPeakData)

#should fail: y is not numeric
plotPrincipalCoordinates(pcoaMat, x=1, y="3", icrData=exampleProcessedPeakData)

#should fail: icrData is invalid
plotPrincipalCoordinates(pcoaMat, x=1, y=3, icrData="hello")


