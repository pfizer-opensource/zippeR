
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zippeR

[![R build
status](https://github.com/pfizer-opensource/zippeR/workflows/R-CMD-check/badge.svg)](https://github.com/pfizer-opensource/zippeR/actions)
[![CRAN_status_badge](https://www.r-pkg.org/badges/version/zippeR)](https://cran.r-project.org/package=zippeR)
[![cranchecks](https://badges.cranchecks.info/worst/zippeR.svg)](https://cran.r-project.org/web/checks/check_results_zippeR.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/zippeR?color=brightgreen)](https://www.r-pkg.org/pkg/zippeR)
[![DOI](https://img.shields.io/badge/DOI-10.32614%2FCRAN.package.zippeR-blue)](https://doi.org/10.32614/CRAN.package.zippeR)

## Motivation

`zippeR` provides a set of functions for working with ZIP Codes and
ZCTAs as well as building spatial and demographic data for three-digit
ZCTAs. These three-digit ZCTAs have limitations (they are large
regions), but they are also used within American health care to protect
patient confidentiality. `zippeR` therefore offers researchers who must
use three-digit ZCTAs the capability to download geometric data and also
aggregate demographic data from five-digit ZCTAs to three-digit ZCTAs.
In addition, `zippeR` includes functions for validating and formatting
vectors of ZIP Codes or ZCTAs as well as tools for cross-walking ZIP
codes with ZCTAs.

## Installation

The easiest way to get started with `zippeR` is to install it from CRAN:

``` r
install.packages("zippeR")
```

The development version of `zippeR` can be accessed from GitHub with
`remotes`:

``` r
# install.packages("remotes")
remotes::install_github("pfizer-opensource/zippeR")
```

## Usage

`zippeR` contains functions that support the following tasks: \*
Labeling five-digit and three-digit ZIP Codes \* Converting ZIP Codes to
ZCTAs, counties, and other Census geographies \* Downloading ZCTA
geometries for both five-digit and three-digit areas \* Aggregating
demographic data from five-digit ZCTAs to three-digit ZCTAs

While a quick overview of the the core functionality is below, see the
vignettes and our package website for more information on how to use
these functions.

### Converting ZIP Codes to ZCTAs

The `zi_load_crosswalk()` function provides access to the former UDS
Mapper project’s ZIP Code to ZCTA crosswalk. This function returns a
data frame with ZIP Codes and their corresponding ZCTAs.

``` r
> zi_load_crosswalk(year = 2020)
# A tibble: 41,096 × 6                                                                                                                                                                                 
   ZIP   PO_NAME    STATE ZIP_TYPE                             ZCTA  zip_join_type       
   <chr> <chr>      <chr> <chr>                                <chr> <chr>               
 1 00501 Holtsville NY    Post Office or large volume customer 11742 Spatial join to ZCTA
 2 00544 Holtsville NY    Post Office or large volume customer 11742 Spatial join to ZCTA
 3 00601 Adjuntas   PR    ZIP Code Area                        00601 ZIP Matches ZCTA    
 4 00602 Aguada     PR    ZIP Code Area                        00602 ZIP Matches ZCTA    
 5 00603 Aguadilla  PR    ZIP Code Area                        00603 ZIP Matches ZCTA    
 6 00604 Aguadilla  PR    Post Office or large volume customer 00603 Spatial join to ZCTA
 7 00605 Aguadilla  PR    Post Office or large volume customer 00603 Spatial join to ZCTA
 8 00606 Maricao    PR    ZIP Code Area                        00606 ZIP Matches ZCTA    
 9 00610 Anasco     PR    ZIP Code Area                        00610 ZIP Matches ZCTA    
10 00611 Angeles    PR    Post Office or large volume customer 00641 Spatial join to ZCTA
# … with 41,086 more rows
```

Likewise, users can use the `zip_load_crosswalk()` function combined
with a HUD API key to access the HUD USPS ZIP Code Crosswalk. This
function returns a data frame with ZIP Codes and their corresponding
Census geographies:

``` r
zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "COUNTY",
  query = "63139", key = "<PASTE KEY>")
    ZIP GEOID RES_RATIO BUS_RATIO OTH_RATIO TOT_RATIO        CITY STATE
1 63139 29510         1         1         1         1 SAINT LOUIS    MO
```

### Downloading ZCTA Geometries

The `zi_get_geometry()` function provides access to both five and
three-digit ZCTA geometries. This function returns a `sf` object with
the geometries of the requested ZCTAs:

``` r
mo_zcta5 <- zi_get_geometry(year = 2012, state = "MO", method = "centroid", 
    includes = c("51640", "52542", "52573", "52626"))
```

### Downloading ZCTA Demographic Data

The `zi_get_demographics()` function provides access to demographic data
for five-digit ZCTAs. This function returns a data frame with
demographic data for the requested ZCTAs:

``` r
mo_gini12 <- zi_get_demographics(year = 2012, table = "B19083", 
  survey = "acs5", zcta = mo_zcta5$GEOID)
```

### Aggregating Demographic Data to Three-digit ZCTAs

These same functions can be combined with `zi_aggregate()` to download
geometric and demographic data, and then aggregate the demographic data
to the three-digit ZCTA level:

``` r
## download Missouri geometric data
mo_zcta3 <- zi_get_geometry(year = 2020, style = "zcta3", state = "MO",
  territory = NULL, method = "intersect")

## download nationwide demographic data
mo_pop20 <- zi_get_demographics(year = 2020, variables = "B01003_001", 
  survey = "acs5")

## aggregate demographic data to three-digit ZCTAs
mo_pop20 <- zi_aggregate(mo_pop20, year = 2020, extensive = "B01003_001", 
  survey = "acs5", zcta = mo_zcta3$ZCTA3)
```

## Gratitude

`zippeR` would not be possible without [Kyle
Walker’s](https://walker-data.com) packages
[`tigris`](https://CRAN.R-project.org/package=tigris) and
[`tidycensus`](https://walker-data.com/tidycensus/), which provide
access to the underlying data U.S. Census Bureau data this package
leverages.

## Feedback and Code of Conduct

If you have feedback on `zippeR`, please [open an issue on
GitHub](https://github.com/pfizer-opensource/zippeR/issues) after
checking the [contribution
guidelines](https://github.com/pfizer-opensource/zippeR/blob/main/.github/CONTRIBUTING.md).
Please note that this project is released with a Contributor [Code of
Conduct](https://github.com/pfizer-opensource/zippeR/blob/main/.github/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
