# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- Updated `NEWS.md`, `README.md`/`README.Rmd`, and `cran-comments.md` with full 0.2.0 release notes, a "What's New" section, and expanded CRAN reviewer summary (#73)

### Added
- Extended TIGRIS year availability from 2023 to 2024 in `zi_get_geometry()`; `zi_list_zctas()` accepts 2024 in the valid range but aborts with an informative message until `sysdata.rda` is rebuilt (#45)
- Updated `inst/build-data/build_vectors.R` to download and process 2024 ZCTA data, enabling future `sysdata.rda` rebuild (#45)
- Bundled UDS Mapper crosswalk data (2009–2022, all 14 years) as `inst/extdata/uds_crosswalk.rds` (~400 KB, xz-compressed), eliminating the runtime network dependency on `chris-prener/uds-mapper` (#41)
- `data-raw/build_uds_crosswalk.R` script documenting provenance and reproducing the bundled crosswalk file (#41)
- Positive-path integration tests for `zi_get_geometry()`, `zi_get_demographics()`, `zi_label()`, and `zi_load_labels()` asserting on output schemas, column types, and row counts (#62)
- Deprecated parameter aliases `input_zip` and `dict` in `zi_crosswalk()` for backwards compatibility; emit deprecation warning noting removal in early 2027 (#64)
- Test coverage gap analysis report (`docs/audits/2026-05-29-test-coverage-gaps.md`) mapping all 13 exported and 18 internal functions to coverage status (#43)
- Source code quality analysis report (`docs/audits/2026-05-29-source-code-quality.md`) with 34 findings across 18 source files (#44)
- Test coverage for HUD crosswalk/loading paths: input validation for `zi_load_crosswalk()`, `zi_crosswalk()`, and internal `zi_load_hud()` helper; end-to-end custom dictionary tests (#58)

### Fixed
- Extended `zi_get_demographics()` and `zi_aggregate()` `acs1`/`acs5` year-range validation from 2010-2022 to 2010-2024, bringing them in line with `zi_get_geometry()`'s currently-working 2023 support; live-verified against the Census API for both 2023 and 2024 ACS5 vintages (#103)
- Fix N/A ZCTA rows in 2009 UDS data being incorrectly zero-padded to `"00N/A"` instead of being filtered out; normalization now removes N/A ZCTAs before zero-padding (#41)
- Replace `eval(parse(text = ...))` dispatch in `zi_utils.R` with safer `getExportedValue()` (#60)
- Replace `tigris::states()` network download in `zi_prep_hud()` with static `states_lookup` for faster, offline-capable execution (#60)
- Replace `merge()` with `dplyr::left_join()` in `zi_crosswalk()` and `zi_label()` to preserve row order (#60)
- Handle all-NA ratio groups in `zi_prep_hud()` that previously produced `-Inf` from `max()` (#60)
- Surface original error messages in network helper `tryCatch` blocks instead of generic "download failed" (#61)
- Add HTTP status validation in `zi_load_hud()` before JSON parsing (#61)
- Wrap `sf::st_read()` and `readr::read_csv()` remote calls with informative error handling (#61)
- Fail fast on invalid states in `zi_list_zctas()` instead of silently returning empty results (#61)
- Add `variables`/`table` both-NULL validation in `zi_get_demographics()` (#63)
- Add `class` parameter validation in `zi_get_geometry()` (#63)
- Add variable name existence check in `zi_aggregate()` (#63)
- Add `.data` schema validation in `zi_prep_hud()` (#63)
- Add scalar length checks for `style` and `verbose` in `zi_validate()` (#63)
- Reject non-numeric characters in `zi_validate_starts()` (e.g., "A1" no longer passes) (#63)
- Fix `zi_convert()` roxygen to correctly document NSE (bare column name) interface (#64)
- Remove misleading numeric-input auto-conversion promise from `zi_crosswalk()` and `zi_label()` docs (#64)
- Add informative message when `zi_get_geometry(year = 2011)` coerces to 2010 data (#64)
- Update `converting-zips.Rmd` and `three-digit-zips.Rmd` vignettes to use current function signatures (#64)
- Fix `zi_load_labels_list()` filter comparing `type` column to itself instead of the function argument (#52)
- Fix `zi_load_labels()` USPS vintage filter using wrong column name (`vintage` → `date`) (#53)
- Fix `zi_get_geometry()` county download failure path referencing undefined variable; now returns NULL gracefully (#54)
- Add territory FIPS code normalization so numeric codes (60, 66, 69, 72, 78) are accepted as documented (#54)
- Fix `zi_aggregate()` intensive weighting producing cartesian expansion by including GEOID in weights join (#55)
- Add `intensive_method` validation with informative error message (#55)
- Fix `zi_aggregate()` `output = "wide"` for decennial Census data which lacks `estimate`/`moe` columns (#55)
- Fix `zi_load_crosswalk()` COUNTYSUB branch using wrong internal string (`COUNTY_SUB` → `COUNTYSUB`) (#56)
- Fix `zi_repair()` stripping leading zeros via numeric coercion; now uses digit-only regex (#57)
- Fix `zi_convert()` using `substitute(input_var)` instead of `substitute(output_var)` when `output_var` is specified, which caused the input column to be overwritten instead of creating a new output column (#33)
- Replace live Census API calls in `test_zi_aggregate.R` with local fixtures so `R CMD check` passes on CRAN without a Census API key (#6)
- Normalize non-standard column names in 2015 UDS crosswalk (`zcta_use` → `zcta`, etc.) so `zi_load_crosswalk(zip_source = "UDS", year = 2015)` no longer errors (#5)
- Add `.data` and `.env` to `zi_load_uds` `globalVariables()` to resolve R CMD check NOTE on no visible binding (#97)
- Add `^data-raw$` to `.Rbuildignore` to suppress CRAN NOTE on top-level `data-raw/` directory (#96)
- Wrap HUD portal URLs in backticks in `vignettes/converting-zips.Rmd` to suppress CRAN WARNING from HTTP 202 responses (#95)
- Fix pkgdown CI build failure caused by non-empty `docs/` directory conflict; pkgdown now builds to `_site/` (#70)
- Replace `JamesIves/github-pages-deploy-action` with first-party OIDC-based `actions/upload-pages-artifact` + `actions/deploy-pages` to resolve GitHub Pages deployment permission error (#74)

### Changed
- Bump version to 0.2.0 (#75)
- Replace `\donttest{}` / `\dontrun{}` wrappers in roxygen2 examples with `@examplesIf interactive()` (network-dependent examples) and `@examplesIf nzchar(Sys.getenv("hud_key"))` (HUD API key examples) across all 9 affected source files (#77)
- Document NULL return values in `@return` tags for `zi_get_demographics()`, `zi_get_geometry()`, and `zi_aggregate()` (#12)
- Remove all manual `@usage` roxygen2 tags; usage sections now auto-generated (#19)
- Set minimum R version to 4.1 in DESCRIPTION (#21)
- Remove base packages (`datasets`, `stats`) from Imports (#10)
- Add minimum version constraints for all dependencies (#15)
- Enable R package dependency caching in lint and pkgdown CI workflows; bump `actions/checkout` v2 → v4 in pkgdown (#32)
- Cache system dependencies (GDAL, PROJ, GEOS, udunits) on Linux and macOS in CI; install steps run only on cache miss (#32)
- Remove `== TRUE` / `== FALSE` anti-patterns across all R/ sources (#11)
- Remove deprecated `context()` calls from all test files; adopt testthat 3rd edition (#8)

### Removed
- Drop `purrr` from `Imports`; replace `purrr::map_dfr()` in `zi_load_hud()` with `do.call(rbind, lapply())` (#80)
- Drop `spatstat.univar` from `Imports`; replace `weighted.median()` calls with an internal `weighted_median()` base-R helper in `R/zi_utils.R` (#81)
- Drop `stringr` from `Imports`; replace `str_pad()`, `str_trim()`, and `word()` calls with base-R equivalents (`formatC`, `trimws`, `sub`) across `R/zi_aggregate.R`, `R/zi_get_demographics.R`, `R/zi_utils.R`, and `R/zi_validate.R` (#82)
- Drop `httr` from `Imports`; replace `httr::GET()` / `httr::content()` / `httr::http_error()` with `httr2::request() |> httr2::req_perform()` / `httr2::resp_body_string()` / `httr2_http_error` condition handler in `R/zi_load_crosswalk.R`; add `httr2 (>= 1.0.0)` to `Imports` (#83)
- Drop `readr` from `Imports`; replace `readr::read_csv()` with `utils::read.csv()` (using `colClasses` for character-type enforcement) in `R/zi_load_labels.R` (#84)
- Drop `tidyr` from `Imports`; replace `tidyr::pivot_wider()` with `stats::reshape()` for both the decennial (single-value) and ACS (estimate+moe) wide-output paths in `R/zi_aggregate.R` (#85)

### Added
- Data provenance README in `inst/build-data/` documenting build scripts, execution order, prerequisites, and outputs (#20)
- Package architecture overview, domain glossary, objectives, and updated roadmap in `docs/` hub (#26)
- `BugReports` field in DESCRIPTION pointing to GitHub Issues (#14)
- Spelling check CI workflow with `inst/WORDLIST` for domain-specific terms (#17)
- `spelling` package added to Suggests (#17)
- `Language: en-US` field in DESCRIPTION (#17)
- EVGen workflow scaffolding vendored from pfizer-evgen/rwd-agent-skills

### Fixed
- Typo in `zi_mo_pop` documentation: "Communiy" → "Community" (#17)
- Typo in `zi_repair` documentation: "conveted" → "converted" (#17)
- Tests for `zi_convert`, `zi_label`, `zi_load_labels`, `zi_load_labels_list`, `zi_prep_hud` (#7)
- Complete placeholder tests in `test_zi_crosswalk.R` and `test_zi_get_demographics.R` (#18)
