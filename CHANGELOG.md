# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- `zi_census_api_key()`, a `zi_`-prefixed wrapper around `tidycensus::census_api_key()`, exported, documented, tested, and referenced from `zi_get_demographics()`, `zi_aggregate()`, and the README, so Census API key setup no longer requires calling a function from another package (#108)
- `.agent.config.yml` declaring the `minimal` profile (GitHub tracker, markdown-in-git docs), reflecting that `zippeR` is a public open-source CRAN package with no Jira or Confluence integration
- Vendored `tests/meta/validate_frontmatter.py`, `validate_agents.py`, and `validate_closing_issue_preconditions.py`; all three are excluded from the package tarball via `.Rbuildignore`
- Vendored the `start_work` and `confluence_docs` skills, the `product-manager` and `scrum-master` agent personas, and 13 new shared partials (provider/profile/scoring-mode, R hat, Copilot attribution, PR body-composition safety)
- Rebuilt `R/sysdata.rda` with full 2024 ZCTA geometry data (intersect and centroid vectors, reference tables); state- and county-scoped `zi_get_geometry()`/`zi_list_zctas()` requests for year 2024 now work end-to-end, matching nationwide support added previously (#91)

### Changed
- Converted all 7 `dplyr::left_join()` call sites in `zi_aggregate.R`, `zi_crosswalk.R`, `zi_label.R`, and `zi_prep_hud.R` to a new internal `left_join_base()` helper (`R/zi_utils.R`) wrapping Base R `merge()`, as part of Epic L's dplyr removal; the helper restores `merge()`'s row and column ordering to match `dplyr::left_join()`'s contract exactly (`x`'s original row order, `x` columns followed by new `y` columns), verified against NA-key matching and one-to-many duplicate-key expansion; behavior is unchanged, no `rlang` introduced (#131)
- Converted all `dplyr::filter()`, `select()`, `mutate()`, and `arrange()` call sites in `zi_aggregate.R`, `zi_crosswalk.R`, `zi_get_demographics.R`, `zi_get_geometry.R`, `zi_list_zctas.R`, `zi_load_crosswalk.R`, `zi_load_labels.R`, and `zi_prep_hud.R` to Base R equivalents, as part of Epic L's dplyr removal; behavior is unchanged, no `rlang` introduced (#130)
- Registered `[Epic K] Vendored Workflow Toolkit Adoption & Governance` (#123) on `docs/ROADMAP.md` under `Next`, with a preferred-but-not-blocking dependency edge to `[Epic J]` (#111) and a note that #120 gates #118 and #119 (#123)
- Added a `brew link --overwrite udunits gdal proj` step after the macOS `sf` dependency install in `R-CMD-check.yaml`, so a cache miss no longer silently leaves the runner-image-preinstalled formulae unlinked with a `##[warning] ... not linked` annotation (#122)
- Retired `[Epic I] CI & Development Infrastructure Health` (#110) from `docs/ROADMAP.md` to `Recently Shipped`; all five sub-issues (#102, #113, #114, #115, #122) shipped, resolving the caching-layer defects, checks-not-firing gap, Node.js 20 deprecation warnings, and macOS Homebrew warnings
- Re-pointed the vendored workflow toolkit upstream from `pfizer-evgen/rwd-agent-skills` to its renamed home `pfizer-evgen/agentic-dev`, and synced all 84 non-overridden `vendored-exact` files to byte parity with current upstream (#116)
- Migrated the monolithic `pull_request` skill to the `pr-gates` cluster (`pr_orchestrator` plus five gate skills); `pr_orchestrator/SKILL.md` is now the entry point for opening and updating PRs (#116)
- Retired the `r-developer` and `workflow-steward` agent personas upstream; R expertise now lives in `developer` via the module-gated R "hat" partial, and `workflow-steward` was renamed `scrum-master` (#116)
- Rebuilt `.github/copilot-instructions.md` from upstream template 1.4.0, preserving the repo's `R/` user-acceptance-testing requirement and expanding the Pre-PR QC checklist with `devtools::document()`, `devtools::test()`, and `devtools::check()` steps (#116)
- Marked `.github/CONTRIBUTING.md`, `.github/LABELS.md`, `.github/PULL_REQUEST_TEMPLATE.md`, and the bug-report and feature-request issue templates as `local_override` in the vendor manifest, so future syncs leave them alone; upstream's versions target internal data-pipeline repos and are unsuitable for a public open-source package (#116)
- Retired `[Epic E] Code & Test Quality Audit` (#48) from `docs/ROADMAP.md` to `Recently Shipped`, and registered `[Epic H] User-Facing API Ergonomics & Dependency Reduction` (#109), `[Epic I] CI & Development Infrastructure Health` (#110), and `[Epic J] v0.3.0 Release` (#111) with an epic dependency graph (#48)
- Raised `[Epic I]` (#110) to `priority/high` after discovering that CI checks did not execute on PRs #105/#106, allowing a `line_length_linter` violation to reach `main` (#113)
- Removed the broken "Cache Linux system dependencies" step in `R-CMD-check.yaml` and its paired `cache-hit` guard on the Linux `apt-get install` step, since `actions/cache` was extracting into root-owned `/usr/lib`/`/usr/include` paths as the unprivileged `runner` user and silently losing the cache-save race across 5 of 6 Linux legs sharing one key; `apt-get install` now always runs on Linux (#115)
- Set `cache: 'always'` with an explicit `cache-version: 1` on `r-lib/actions/setup-r-dependencies` across all five workflows (previously `cache: TRUE`, which silently no-ops on a failed cache restore); added `sudo apt-get update` before Linux system-dependency installs in `R-CMD-check.yaml` and `integration-tests.yaml`; re-keyed the `tigris` download cache in `integration-tests.yaml` with a rotating weekly segment plus a `restore-keys` fallback chain so it can grow instead of being pinned forever to one `DESCRIPTION` hash; added `concurrency` groups with `cancel-in-progress: true` to `R-CMD-check.yaml`, `lint.yaml`, and `spelling.yaml` (#102)
- Bumped `actions/checkout` from v4 to v7, `actions/cache` from v4 to v6, and `actions/upload-pages-artifact`/`actions/deploy-pages` from v3/v4 to v5 (kept in lockstep) across all five workflows to resolve Node.js 20 deprecation warnings on every CI run; `r-lib/actions/*@v2` intentionally left untouched (#114)
- Refactored `inst/build-data/build_vectors.R` to support partial/incremental rebuilds: per-year cached `.rda` files in `inst/data-raw/` are now checked and skipped if already present, so adding a new year no longer requires re-downloading and re-processing all prior years (#91)
- Removed the obsolete interactive `usethis::ui_yeah()` style prompt from `build_vectors.R`; the script now always runs its cache-aware fetch/process step (#91)
- Removed the `zi_list_zctas()` guard that aborted state-/county-scoped 2024 requests; 2024 is now fully supported across all `zippeR` geometry functions (#91)
- Updated `NEWS.md`, `README.md`/`README.Rmd`, and `cran-comments.md` with full 0.2.0 release notes, a "What's New" section, and expanded CRAN reviewer summary (#73)
- Corrected `zi_get_geometry()`/`zi_list_zctas()` documentation and `NEWS.md` wording to accurately describe 2024 geometry support: nationwide requests (no `state`/`county`) already work today, while state- or county-scoped requests abort until internal lookup data is rebuilt; refined the `zi_list_zctas()` 2024 abort message to reference #104; added live-verified regression tests confirming 2020-2023 continue to work correctly (#104)

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
