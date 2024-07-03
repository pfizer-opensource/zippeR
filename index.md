# zippeR

[![R build status](https://github.com/pfizer-evgen/zippeR/workflows/R-CMD-check/badge.svg)](https://github.com/pfizer-evgen/zippeR/actions)
[![Coverage status](https://codecov.io/gh/pfizer-evgen/zippeR/branch/master/graph/badge.svg)](https://codecov.io/github/pfizer-evgen/zippeR?branch=main)

## Motivation
`zippeR` provides a set of functions for working with ZCTAs and building spatial and demographic data for three-digit ZCTAs. These three-digit ZCTAs have limitations (they are large regions), but they are also used within American health care to protect patient confidentiality. `zippeR` therefore offers researchers who must use three-digit ZCTAs the capability to download geometric data and also aggregate demographic data from five-digit ZCTAs to three-digit ZCTAs. In addition, `zippeR` includes functions for validating and formatting vectors of ZIP Codes or ZCTAs as well as tools for cross-walking ZIP codes with ZCTAs.

## Quick Start
The development version of `zippeR` can be accessed from GitHub with `remotes`:

```r
# install.packages("remotes")
remotes::install_github("pfizer-evgen/zippeR")
```

## Resources
The `zippeR` site includes dedicated articles on the following topics:
  * [ZIP Code and ZIP Code Tabulation Area Basics](articles/basics.html)
  * [Converting ZIP Codes to Other Geographies](articles/coverting-zips.html)
  * [Demographic and Geometric ZCTA Data](articles/demographic-geometric-zcta-data.html)
  * [Three-digit ZIP Codes and ZCTAs](articles/three-digit-zips.html)

## Contributor Code of Conduct
Please note that this project is released with a Contributor [Code of Conduct](/.github/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
