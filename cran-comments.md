## Release summary
This version of `zippeR` is a new submission to CRAN. It is a resubmission following guidance from Konstanze Lauseker. 

### Current Resubmission Notes
The package has been updated to address the following issues:

* Missing `roxygen2` tags have been added to `zi_load_labels()` and are reflected in `zi_load_labels.Rd` as requested.

### Prior Resubmission Notes
The prior resubmission made the following additional changes:

* Working examples were added to the following functions:
  * `zi_aggregate()`
  * `zi_crosswalk()` (partial)
  * `zi_prep_hud()`
* To facilitate working examples for the above functions, two exported data objects were added to the package:
  * `zi_mo_pop`
  * `zi_mo_zcta3`
* `\dontrun{}` was replaced with `\donttest{}` in the examples for the following functions that download data:
  * `zi_crosswalk` (partial)
  * `zi_get_demographics()`
  * `zi_get_geometry()`
* Missing `roxygen2` tags have been added `zi_list_zctas()` and are reflected in `zi_list_zctas.Rd` as requested.
* The function `hud_key()` has been removed from the package - it was not used in the package and was flagged for writing by default.
* Code in `inst/extdata` was moved to a different subdirectory of `inst/` and excluded from upload via `.Rbuildignore`. These scripts were flagged for writing to the user's home directory by default and modifying the `.GlobalEnv`.

In addition to the above changes, the following updates were made to the package:

* Global variables set within functions were moved to `zi_globals.R`
* Additional unit tests were added and modified
* New functions `zi_convert()`, `zi_load_labels_list()`, and `zi_load_labels()` were added to the package

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

```r
Maintainer: 'Christopher Prener <Christopher.Prener@pfizer.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  ZCTA (14:21)
  ZCTAs (14:71, 15:48)

Found the following (possibly) invalid URLs:
  URL: https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf
    From: inst/doc/converting-zips.html
    Status: 403
    Message: Forbidden
```

Please note that the URLs are valid and the misspelled words are not misspelled.

