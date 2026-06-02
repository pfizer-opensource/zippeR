# zippeR 0.2.0

## New features and improvements

* UDS Mapper crosswalk data (2009–2022) is now bundled with the package, eliminating the runtime network dependency on an external GitHub repository
* Partial support for 2024 TIGRIS year: `zi_list_zctas()` accepts 2024 in its valid range, and `zi_get_geometry()` accepts the argument but aborts with an informative message until internal data is rebuilt in a future release
* Deprecated parameter aliases `input_zip` and `dict` in `zi_crosswalk()` with backwards-compatible support until early 2027
* `@examplesIf` guards replace `\donttest{}`/`\dontrun{}` wrappers in all network-dependent and API-key-dependent examples
* Minimum R version set to 4.1

## Bug fixes

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
