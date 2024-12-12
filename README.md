# ftmsRanalysis
<!-- badges: start -->
[![R-CMD-check](https://github.com/EMSL-Computing/ftmsRanalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EMSL-Computing/ftmsRanalysis/actions/workflows/R-CMD-check.yaml)
[![Coverage Status](https://codecov.io/github/EMSL-Computing/ftmsRanalysis/coverage.svg?branch=master)](https://codecov.io/github/EMSL-Computing/ftmsRanalysis?branch=master)
<!-- badges: end -->

This R package provides functionality for data formatting, preprocessing, filtering, exploratory data analysis and visualization of fourier transform (FT) mass spectrometry (MS) data. 

ftmsRanalysis also serves as the backend to the FT-MS R Exploratory Data Analysis (FREDA) web application, which can be accessed at https://map.emsl.pnnl.gov/app/freda after signing up for an account.  The code repository for the FREDA web application can be found [here](https://github.com/EMSL-Computing/FREDA).

## Installation:

(**Recommended**) To install the latest *release*:
```r
devtools::install_github("EMSL-Computing/ftmsRanalysis@*release")
```

To install a specific release, say 1.0.0:

```r
devtools::install_github("EMSL-Computing/ftmsRanalysis@1.0.0")
```

**(Not recommended, since these changes are still being tested) **, You can also install the most current version of master:

```r
devtools::install_github("EMSL-Computing/ftmsRanalysis")
```

## Tutorial:

To get started, see the package documentation and function reference located at http://EMSL-Computing.github.io/ftmsRanalysis.

## Data:

Example data are available in the __ftmsRanalaysis__ package. Information about how to access this data is shown in package vignettes and on the website (link in the tutorial section above)
 
