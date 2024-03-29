## Test plotting functions for filter objects

library(ftmsRanalysis)

data(examplePeakData)

## Molecule filter

filter_obj <- molecule_filter(examplePeakData)

# Second bar should be blue:
plot(filter_obj, min_num=2)

# No blue bars
plot(filter_obj)


## Mass Filter
filter_obj <- mass_filter(examplePeakData)

# Bars between 200 and 800 should be blue:
plot(filter_obj, min_mass = 200, max_mass = 800)

# No blue bars
plot(filter_obj)


## Formula filter
filter_obj <- formula_filter(examplePeakData)

# Formula bar should be blue
plot(filter_obj, remove='NoFormula')

# NoFormula bar should be blue
plot(filter_obj, remove='Formula')

# No blue bars
plot(filter_obj)


## Emeta Filter

## TESTS
# filter peaks based on Oxygen to Carbon ratio #
data("exampleProcessedPeakData")
filter_object1 = emeta_filter(exampleProcessedPeakData, cname = "OtoC_ratio")

# Blue bars greater than 0.5
plot(filter_object1, min_val = 0.5)

# Blue bars between 0.5 and 1
plot(filter_object1, min_val = 0.5, max_val = 1)

# No blue bars
plot(filter_object1)

# filter peaks based on molecular formula #
filter_object2 = emeta_filter(exampleProcessedPeakData, cname = "MolForm")

#  Only non-NAs retained:
plot(filter_object2)

# Everthing retained
plot(filter_object2, na.rm=FALSE)

# Only 1 MolForm retained
plot(filter_object2, cats = "C12H14O12", na.rm=TRUE)

# 1 MolForm and NAs retained
plot(filter_object2, cats = "C12H14O12", na.rm=FALSE)

# Peaks matching "C10" retained
plot(filter_object2, cats=grep("C10", filter_object2$emeta_value, value = TRUE))

