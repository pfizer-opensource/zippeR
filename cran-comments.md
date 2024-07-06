## Release summary
This version of `zippeR` is a new submission to CRAN. It is a resubmission following guidance from Konstanze Lauseker. The package has been updated to address the following issues:

* Missing `roxygen2` tags have been added `zi_list_zctas()` and are reflected in `zi_list_zctas.Rd` as requested.
* The function `hud_key()` has been removed from the package - it was not used in the package and was flagged for writing by default.
* Code in `inst/extdata` was moved to a different subdirectory of `inst/` and excluded from upload via `.Rbuildignore`. These scripts were flagged for writing to the user's home directory by default and modifying the `.GlobalEnv`.

We did not address the following issues:

* Requested that we add `doi` to the `DESCRIPTION` file for references describing the methods in your package - we have a publication in progress but nothing to cite yet.

## Test environments
* local macOS install: R 4.4.0
* Linux ubuntu distribution (via GitHub Actions): R-devel, R-release, past four R-oldrel
* macOS (via GitHub Actions): R-release
* windows (via GitHub Actions): R-release
* winbuilder: R-release, R-oldrel, R-devel

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs with local or CI checks. There is one NOTE on winbuilder:

0 errors | 0 warnings | 1 note

* This is a new release.
