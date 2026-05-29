# Source Code Quality Analysis

**Date:** 2026-05-29
**Issue:** #44
**Epic:** #48 ([Epic E] Code & Test Quality Audit)

## Summary

- **34 findings** across 18 source files
- **11 high severity**, 23 medium severity
- Categories: Correctness (10), Input validation (10), Error handling (5), API consistency (4), Performance (3), DRY/readability (2)

## High-Severity Findings

### 1. zi_aggregate.R — Weighting cartesian expansion
**Lines:** L350-L398, L436-L488
**Category:** Correctness

Weights helper drops `GEOID` via `select(out, ZCTA3, weight)` and joins only by `ZCTA3`, creating a cartesian expansion within each 3-digit region. Weighted means/medians are silently wrong.

### 2. zi_aggregate.R — `intensive_method` never validated
**Lines:** L356-L363, L442-L451
**Category:** Input validation

Any value other than `"mean"`/`"median"` falls through and returns grouped raw rows instead of aggregated output.

### 3. zi_aggregate.R — Wide output broken for decennial
**Lines:** L301-L309
**Category:** Correctness / API consistency

`output = "wide"` always renames `estimate`/`moe`; decennial paths produce `value`, so wide output for census data is broken.

### 4. zi_get_demographics.R — Both `variables` and `table` can be NULL
**Lines:** L109-L110
**Category:** Input validation / Error handling

Function forbids using both together but allows both to be NULL, pushing invalid request to tidycensus.

### 5. zi_get_geometry.R — Territory FIPS codes rejected
**Lines:** L33-L38 vs L212-L216
**Category:** Correctness / API consistency

Docs say territory FIPS codes accepted; validation only accepts abbreviations.

### 6. zi_get_geometry.R — County failure path uses undefined variable
**Lines:** L426-L463
**Category:** Correctness

If county download fails, `zi_process_county()` reaches `return(out)` but `out` was never created.

### 7. zi_load_crosswalk.R — COUNTYSUB branch dead code
**Lines:** L98-L102 vs L269-L270
**Category:** Correctness

Public API accepts `"COUNTYSUB"` but internal mapping checks `"COUNTY_SUB"` — branch never executes.

### 8. zi_load_crosswalk.R — No HTTP/API-error validation
**Lines:** L274-L283
**Category:** Error handling

Parser assumes fixed JSON layout; no status check before accessing elements.

### 9. zi_load_labels.R — USPS vintage filter uses wrong column
**Lines:** L101-L109
**Category:** Correctness

Filters with `vintage == vintage_chr` but metadata table uses `date` column.

### 10. zi_load_labels_list.R — Self-comparison filter
**Lines:** L31-L35
**Category:** Correctness

`subset(labels_list, type == type)` compares column to itself; always returns all rows.

### 11. zi_validate.R / zi_repair() — Leading-zero stripping
**Lines:** L251-L263
**Category:** Correctness

Coerces through numeric; can strip leading zeros from valid values and allows scientific notation.

## Medium-Severity Findings

| # | File | Category | Description |
|---|---|---|---|
| 12 | zi_aggregate.R | Input validation | Extensive/intensive vars only filtered, not verified; missing names silently dropped |
| 13 | zi_convert.R | API consistency | Docs say character scalars; implementation requires NSE names |
| 14 | zi_crosswalk.R | API consistency | Docs say numeric ZIPs converted; implementation aborts |
| 15 | zi_crosswalk.R | Input validation | `source_var` validated with wrong style/message |
| 16 | zi_crosswalk.R | Performance | `base::merge()` reorders rows; should use `left_join()` |
| 17 | zi_crosswalk.R | DRY | Year/vintage/load logic duplicates zi_label/zi_load_crosswalk |
| 18 | zi_get_demographics.R | Correctness | Rebuilds GEOID from NAME instead of using API's GEOID field |
| 19 | zi_get_geometry.R | API consistency | year==2011 silently coerced to 2010 |
| 20 | zi_get_geometry.R | Input validation | `class` parameter never validated |
| 21 | zi_get_geometry.R | Input validation | `zi_validate_starts()` only checks width, not digit content |
| 22 | zi_get_geometry.R | Error handling | `sf::st_read()` for zcta3 has no try wrapper |
| 23 | zi_label.R | API consistency | Same numeric-input promise as zi_crosswalk, not honored |
| 24 | zi_label.R | Performance | Same `merge()` issue |
| 25 | zi_list_zctas.R | Error handling | Invalid states warned and dropped; silent degradation to empty |
| 26 | zi_load_crosswalk.R | Input validation | Numeric ZIP queries lose leading zeros |
| 27 | zi_load_labels.R | Error handling | Remote read_csv failures not wrapped |
| 28 | zi_prep_hud.R | Performance | Downloads tigris::states() on every call for state→FIPS |
| 29 | zi_prep_hud.R | Input validation | .data schema never validated |
| 30 | zi_prep_hud.R | Correctness | max() on all-NA groups returns -Inf, silently dropping rows |
| 31 | zi_utils.R | Readability | eval(parse()) for tigris dispatch; brittle |
| 32 | zi_utils.R | Error handling | Helpers swallow all errors; mask programmer errors |
| 33 | zi_validate.R | Input validation | Numeric check uses as.numeric(); scientific notation passes |
| 34 | zi_validate.R | Input validation | style/verbose checked for type but not scalar length |

## Documentation Drift

- `vignettes/converting-zips.Rmd:46-58` — documents obsolete `zi_crosswalk(..., input_zip=, dict=)` usage
- `vignettes/three-digit-zips.Rmd:72` — says 2011 excluded, but code silently aliases to 2010

## Follow-on Issues

**High-priority bugs:**
- #55 — zi_aggregate() weighting cartesian expansion
- #52 — zi_load_labels_list() self-comparison filter
- #56 — zi_load_crosswalk() COUNTYSUB dead code
- #53 — zi_load_labels() USPS vintage column mismatch
- #57 — zi_repair() leading-zero stripping
- #54 — zi_get_geometry() county failure undefined variable

**Medium-priority improvements:**
- #63 — Input validation gaps
- #61 — Error handling improvements
- #64 — API consistency issues
- #60 — DRY and performance improvements
