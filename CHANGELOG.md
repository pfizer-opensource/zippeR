# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Test coverage gap analysis report (`docs/audits/2026-05-29-test-coverage-gaps.md`) mapping all 13 exported and 18 internal functions to coverage status (#43)
- Source code quality analysis report (`docs/audits/2026-05-29-source-code-quality.md`) with 34 findings across 18 source files (#44)

### Fixed
- Fix `zi_convert()` using `substitute(input_var)` instead of `substitute(output_var)` when `output_var` is specified, which caused the input column to be overwritten instead of creating a new output column (#33)
- Replace live Census API calls in `test_zi_aggregate.R` with local fixtures so `R CMD check` passes on CRAN without a Census API key (#6)
- Normalize non-standard column names in 2015 UDS crosswalk (`zcta_use` → `zcta`, etc.) so `zi_load_crosswalk(zip_source = "UDS", year = 2015)` no longer errors (#5)

### Changed
- Document NULL return values in `@return` tags for `zi_get_demographics()`, `zi_get_geometry()`, and `zi_aggregate()` (#12)
- Remove all manual `@usage` roxygen2 tags; usage sections now auto-generated (#19)
- Set minimum R version to 4.1 in DESCRIPTION (#21)
- Remove base packages (`datasets`, `stats`) from Imports (#10)
- Add minimum version constraints for all dependencies (#15)
- Enable R package dependency caching in lint and pkgdown CI workflows; bump `actions/checkout` v2 → v4 in pkgdown (#32)
- Cache system dependencies (GDAL, PROJ, GEOS, udunits) on Linux and macOS in CI; install steps run only on cache miss (#32)
- Remove `== TRUE` / `== FALSE` anti-patterns across all R/ sources (#11)
- Remove deprecated `context()` calls from all test files; adopt testthat 3rd edition (#8)

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
