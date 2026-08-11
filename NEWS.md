# zippeR (development version)

## New features and improvements

* Added `zi_census_api_key()`, a `zi_`-prefixed wrapper around `tidycensus::census_api_key()`, so Census API key setup no longer requires calling a function from another package. Referenced from `zi_get_demographics()` and `zi_aggregate()` documentation and the README (#108)

# zippeR 0.2.0

## New features and improvements

* UDS Mapper crosswalk data (2009–2022) is now bundled with the package, eliminating the runtime network dependency on an external GitHub repository
* Full support for 2024 TIGRIS year: `zi_list_zctas()` and `zi_get_geometry()` now support 2024 for all request types, including state- and county-scoped requests (previously these aborted pending a rebuild of internal lookup data, tracked in [#104](https://github.com/pfizer-opensource/zippeR/issues/104), now resolved). 2010-2023 continue to work as before (regression-tested)
* Deprecated parameter aliases `input_zip` and `dict` in `zi_crosswalk()` with backwards-compatible support until early 2027
* `@examplesIf` guards replace `\donttest{}`/`\dontrun{}` wrappers in all network-dependent and API-key-dependent examples
* Minimum R version set to 4.1

## Bug fixes

* `zi_get_demographics()` and `zi_aggregate()` now accept `year` values through 2024 for `survey` values `"acs1"` and `"acs5"` (previously capped at 2022), bringing them in line with `zi_get_geometry()`'s full 2024 support
* Resolved several input validation gaps across `zi_aggregate()`, `zi_crosswalk()`, `zi_convert()`, `zi_get_geometry()`, `zi_get_demographics()`, `zi_load_crosswalk()`, `zi_load_labels()`, `zi_load_labels_list()`, `zi_prep_hud()`, `zi_repair()`, and `zi_validate()`
* Fixed a number of incorrect column references, variable scoping errors, and unsafe dispatch patterns identified during a code quality audit
* Replaced live Census API calls in tests with local fixtures so `R CMD check` passes on CRAN without a Census API key
* Normalized non-standard column names in the 2015 UDS crosswalk

## Dependency changes

* Dropped `purrr`, `spatstat.univar`, `stringr`, `httr`, `readr`, and `tidyr` from Imports
* Added `httr2 (>= 1.0.0)` to Imports

# zippeR 0.1.2

* Address issues with Census Bureau API being offline

# zippeR 0.1.1

* Updates to `README.md` and `NEWS.md` to reflect the fact that the package is now on CRAN
* Resolve R CMD check issue where examples in `zi_crosswalk` and `zi_load_crosswalk` fail on CI because a HUD key is not available to them
* Fail informatively if `tigris` is not working due to U.S. Census Bureau servers being unavailable. At this time TigerWeb is not used as a backup due to the limited availability of ZCTA data

# zippeR 0.1.0

* Initial CRAN submission version that contains functionality for working with UDS and HUD ZIP Code crosswalk files as well as Census Bureau ZCTA geometries and demographic data
