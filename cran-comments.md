## Release summary
This version of `zippeR` is a patch for CRAN. The package has been updated to address the following issues:

* The package was failing CRAN tests earlier this month due to the U.S. Census Bureau's servers being taken offline. If this were to happen in the future, the demographic functions now fail informatively.

## Test environments
* local macOS install: R 4.4.3
* Linux ubuntu distribution (via GitHub Actions): R-devel, R-release, past four R-oldrel
* macOS (via GitHub Actions): R-release
* windows (via GitHub Actions): R-release
* winbuilder: R-release, R-oldrel, R-devel

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs with local or CI checks. 
