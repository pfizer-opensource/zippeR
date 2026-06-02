---
last_updated: "2026-05-29"
---
# Objectives — zippeR

## Active Objectives

### O1: CRAN-ready package quality

Ensure zippeR meets CRAN submission standards for documentation, testing, and
package structure.

**Key Results:**

- KR1: All exported functions have complete roxygen2 documentation (`@param`,
  `@return`, `@examples`) — _achieved (Epic C)_
- KR2: R CMD check passes with 0 errors, 0 warnings, 0 notes — _achieved (Epic G)_
- KR3: Test coverage ≥ 80% for all exported functions — _achieved (Epic A)_
- KR4: `DESCRIPTION` fields complete (URL, BugReports, Language) — _achieved
  (Epic D)_

_Last check-in: 2026-06-02_

### O2: Comprehensive geographic coverage

Support the full range of Census Bureau ZCTA vintages and crosswalk sources
relevant to health services research.

**Key Results:**

- KR1: ZCTA geometries available for all TIGER/Line vintages 2010–2024 —
  _partial_ (`zi_get_geometry()` supports 2024; `zi_list_zctas()` guarded pending
  `sysdata.rda` rebuild — #45 follow-up)
- KR2: HUD and UDS crosswalk loading supported — _achieved_ (UDS crosswalk now
  bundled in `inst/extdata/`, eliminating runtime network dependency — #41)
- KR3: 3-digit ZCTA aggregation supports both extensive and intensive variables
  — _achieved_

_Last check-in: 2026-06-01_

## Retired Objectives

_None._
