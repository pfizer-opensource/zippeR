# Test Coverage Gap Analysis

**Date:** 2026-05-29
**Issue:** #43
**Epic:** #48 ([Epic E] Code & Test Quality Audit)

## Summary

- **13 exported functions** analyzed: 2 fully covered, 11 partially covered
- **18 internal functions** analyzed: 10 uncovered, 5 partially covered, 3 N/A
- **160 tests passing**, 0 failures, 1 deprecation warning
- **Major blind spots**: HUD crosswalk paths, aggregation internals, geometry success branches

## Exported Function Coverage

| Function | Status | Key Gaps |
|---|---|---|
| `zi_aggregate` | Partial | No decennial, intensive, weights, or median tests |
| `zi_convert` | Partial | Minimal output-content assertions |
| `zi_crosswalk` | Partial | API-backed HUD/UDS paths untested |
| `zi_get_demographics` | Partial | No data assertions; no decennial/table/wide tests |
| `zi_get_geometry` | Partial | No county/zcta3/full/state/territory success tests |
| `zi_label` | Partial | No UDS/USPS success tests |
| `zi_list_zctas` | Partial | No `state=` or `method="intersect"` tests |
| `zi_load_crosswalk` | Partial | HUD loader path untested |
| `zi_load_labels` | Partial | No positive-path assertions |
| `zi_load_labels_list` | Covered | Simple function; minor network-dep risk only |
| `zi_prep_hud` | Partial | No NA-state, tie-breaking, or malformed input tests |
| `zi_repair` | Partial | Two tests call wrong function (`zi_validate` instead of `zi_repair`) |
| `zi_validate` | Covered | Minor edge cases only (NA-heavy/zero-length inputs) |

## Internal Function Coverage

| Function | File | Status | Notes |
|---|---|---|---|
| `validate_state` | zi_utils.R | Partial | Hit via exported paths; not exhaustive |
| `simpleCapSO` | zi_utils.R | Uncovered | Only used in `validate_state` full-name normalization |
| `zi_get_tigris` | zi_utils.R | Partial | One smoke test; error/alternate paths untested |
| `zi_get_decennial` | zi_get_demographics.R | Uncovered | No successful decennial retrieval tests |
| `zi_get_acs` | zi_get_demographics.R | Partial | Reached by smoke tests; null/error untested |
| `zi_census_extensive` | zi_aggregate.R | Uncovered | No decennial aggregation tests |
| `zi_census_intensive` | zi_aggregate.R | Uncovered | No intensive decennial tests |
| `zi_census_weights` | zi_aggregate.R | Uncovered | No decennial weighting tests |
| `zi_acs_extensive` | zi_aggregate.R | Covered | Exercised by ACS happy-path tests |
| `zi_acs_intensive` | zi_aggregate.R | Uncovered | No ACS intensive tests |
| `zi_acs_weights` | zi_aggregate.R | Uncovered | No ACS intensive weighting tests |
| `zi_load_uds` | zi_load_crosswalk.R | Partial | Lightly exercised |
| `zi_load_hud` | zi_load_crosswalk.R | Uncovered | No HUD loader tests |
| `zi_load_labels_uds` | zi_load_labels.R | Partial | Only lightly through wrapper |
| `zi_load_labels_usps` | zi_load_labels.R | Uncovered | No meaningful test hits |
| `zi_get_zcta5` | zi_get_geometry.R | Partial | One centroid smoke path only |
| `zi_process_county` | zi_get_geometry.R | Uncovered | No county geometry success tests |
| `zi_get_zcta3` | zi_get_geometry.R | Uncovered | No zcta3 geometry success tests |
| `zi_validate_starts` | zi_get_geometry.R | Partial | Invalid-input path only |

## Key Gaps

1. **Network-backed success paths are mostly smoke tests** — use `expect_no_error()` without verifying output content.
2. **HUD crosswalk logic is the largest blind spot** — most complex branching, zero test coverage.
3. **Aggregation internals under-tested** — only ACS extensive path is exercised.
4. **Geometry success branches under-tested** — no output assertions for most `type`/`return` combinations.
5. **`zi_repair` has mistaken tests** — `test_zi_repair.R:13,17` call `zi_validate()`.

## Follow-on Issues

- #58 — test: add coverage for HUD crosswalk/loading paths (P1)
- #62 — test: add positive-path assertions for geometry, demographics, and label functions (P2)
- #59 — test: add coverage for aggregation internals (P3)
