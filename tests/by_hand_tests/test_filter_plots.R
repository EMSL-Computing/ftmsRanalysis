## Test plotting functions for filter objects

library(fticRanalysis)

data(peakIcrData)

## Molecule filter

filter_obj <- molecule_filter(peakIcrData)

# Second bar should be red:
plot(filter_obj, min_num=2)

# No red bars
plot(filter_obj)

## Mass Filter
filter_obj <- mass_filter(peakIcrData)

# Bars between 200 and 800 should be red:
plot(filter_obj, min_mass = 200, max_mass = 800)

# No red bars
plot(filter_obj)

## Formula filter
filter_obj <- formula_filter(peakIcrData)

# Formula bar should be red
plot(filter_obj, remove='NoFormula')

# No red bars
plot(filter_obj)


## Emeta Filter

## TESTS
# filter peaks based on Oxygen to Carbon ratio #
filter_object1 = emeta_filter(peakIcrProcessed, cname = "OtoC_ratio")

# Red bars greater than 0.5
plot(filter_object1, min_val = 0.5)

# Red bars between 0.5 and 1
plot(filter_object1, min_val = 0.5, max_val = 1)

# No red bars
plot(filter_object1)

# filter peaks based on molecular formula #
filter_object2 = emeta_filter(peakIcrProcessed, cname = "MolForm")

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
