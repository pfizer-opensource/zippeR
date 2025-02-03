# zippeR

[![R build status](https://github.com/pfizer-opensource/zippeR/workflows/R-CMD-check/badge.svg)](https://github.com/pfizer-opensource/zippeR/actions)
[![CRAN_status_badge](https://www.r-pkg.org/badges/version/zippeR)](https://cran.r-project.org/package=zippeR)
[![cranchecks](https://badges.cranchecks.info/worst/zippeR.svg)](https://cran.r-project.org/web/checks/check_results_zippeR.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/zippeR?color=brightgreen)](https://www.r-pkg.org/pkg/zippeR)
[![DOI](https://img.shields.io/badge/DOI-10.32614%2FCRAN.package.zippeR-blue)](https://doi.org/10.32614/CRAN.package.zippeR)

## Motivation
`zippeR` provides a set of functions for working with ZIP Codes and ZCTAs as well as building spatial and demographic data for three-digit ZCTAs. These three-digit ZCTAs have limitations (they are large regions), but they are also used within American health care to protect patient confidentiality. `zippeR` therefore offers researchers who must use three-digit ZCTAs the capability to download geometric data and also aggregate demographic data from five-digit ZCTAs to three-digit ZCTAs. In addition, `zippeR` includes functions for validating and formatting vectors of ZIP Codes or ZCTAs as well as tools for cross-walking ZIP codes with ZCTAs.

## Quick Start
The easiest way to get started with `zippeR` is to install it from CRAN:

```r
install.packages("zippeR")
```

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

## Feedback and Code of Conduct
If you have feedback on `zippeR`, please [open an issue on GitHub](https://github.com/pfizer-opensource/zippeR/issues) after checking the [contribution guidelines](https://github.com/pfizer-opensource/zippeR/blob/main/.github/CONTRIBUTING.md). Please note that this project is released with a Contributor [Code of Conduct](https://github.com/pfizer-opensource/zippeR/blob/main/.github/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
