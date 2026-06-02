## Release summary
This is a minor release of `zippeR` (0.2.0) for CRAN. Key changes include:

* Bundled UDS Mapper ZIP-to-ZCTA crosswalk data (2009–2022) directly in the package, removing the runtime network dependency on an external GitHub repository
* Dropped six packages from Imports (`purrr`, `spatstat.univar`, `stringr`, `httr`, `readr`, `tidyr`), replacing them with base-R equivalents or `httr2`
* Resolved a broad set of input validation gaps, incorrect column references, and unsafe dispatch patterns identified during a code quality audit
* Replaced live Census API calls in the test suite with local fixtures so checks pass on CRAN without a Census API key
* Set minimum R version to 4.1

## Test environments
* local macOS install: R 4.4.3
* Linux ubuntu distribution (via GitHub Actions): R-devel, R-release, past four R-oldrel
* macOS (via GitHub Actions): R-release
* windows (via GitHub Actions): R-release
* winbuilder: R-release, R-oldrel, R-devel

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs with local or CI checks. 
