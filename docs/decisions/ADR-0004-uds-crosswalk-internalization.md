# ADR-0004: Bundle UDS crosswalk data in inst/extdata rather than downloading at runtime

**Status:** accepted
**Date:** 2026-05-31
**Decision-makers:** @chris-prener
**Related epic(s):** #49 ([Epic F] New Features & Enhancements)
**Related requirements:** N/A
**Source(s):** Issue #41 (research + recommendation comment), PR #41-internalize-uds-crosswalk

## Context

`zi_load_uds()` previously downloaded a per-year CSV from
`https://raw.githubusercontent.com/chris-prener/uds-mapper/main/data/uds_crosswalk_<year>.csv`
on every call. This created three problems:

1. **Network dependency at runtime.** Any invocation of `zi_load_crosswalk()`,
   `zi_crosswalk()`, `zi_load_labels()`, or `zi_label()` with `zip_source = "UDS"`
   required internet access. Offline use, air-gapped environments, and automated
   pipelines without network were not supported.

2. **Single point of failure.** If the upstream `chris-prener/uds-mapper` repository
   changed its URL structure, was renamed, or went offline, all UDS-related functions
   silently broke. Issue #5 (2015 schema inconsistency) was a direct symptom of
   upstream schema drift that the runtime path had no defense against.

3. **Non-reproducibility.** Because the CSV was fetched live, results could differ
   between calls if upstream data changed — violating the reproducibility contract
   expected of a CRAN package.

The UDS crosswalk covers 14 years (2009–2022) and is updated annually. Once a year is
published it is frozen upstream, making the data amenable to bundling. A size analysis
(see issue #41 research comment) showed all 14 years combine to ~400 KB when stored as
a single xz-compressed RDS — well within CRAN's 5 MB tarball soft limit.

Five approaches were evaluated: vendored compressed data in `inst/extdata/`, lazy-load
`data/` or `sysdata.rda`, companion data package (`zippeRdata`), pinned GitHub release
+ local cache, and year subsetting.

## Decision

We will ship all 14 UDS crosswalk years (2009–2022) as a single xz-compressed RDS file
at `inst/extdata/uds_crosswalk.rds` and load it at runtime via `system.file()`.

A `data-raw/build_uds_crosswalk.R` script documents provenance and allows the file to
be regenerated when a new UDS year becomes available (requiring a package release).

**Alternatives considered and rejected:**

| Option | Verdict |
|---|---|
| Per-year files in `inst/extdata/` | Viable but unnecessary — combined file compresses better |
| Lazy-load `data/` or `sysdata.rda` | Similar size but forces all years into memory on package attach |
| Companion data package (`zippeRdata`) | Overkill for 400 KB; adds install complexity for users |
| Pinned GitHub release + local cache (`tools::R_user_dir()`) | Retains a (more stable) network dependency; more complex code path |
| Year subsetting (recent years only) | Unnecessary given full dataset fits easily under CRAN limits |

## Consequences

- **Easier:** UDS functions work offline and in air-gapped environments; results are
  reproducible across machines and time; no HTTP error handling needed in `zi_load_uds()`;
  tests for UDS paths no longer require network access or `skip_if_no_integration()`.
- **Harder:** Adding a new UDS year (when UDS Mapper publishes 2023+) requires running
  `data-raw/build_uds_crosswalk.R`, committing the updated RDS, and cutting a new package
  release. Users cannot self-serve new years without upgrading `zippeR`.
- **Constraints:** The bundled RDS must be regenerated and committed whenever upstream
  UDS data is updated. The `data-raw/build_uds_crosswalk.R` script is the single
  authoritative way to regenerate it; ad-hoc edits to the RDS are prohibited.
- **Revisit when:** The UDS dataset grows substantially (e.g., many new years added
  rapidly) such that the RDS approaches 2–3 MB and threatens CRAN size compliance, or
  when a streaming/lazy API for the data becomes available that is sufficiently stable
  to serve as a reproducible source.

## References

- Issue #41 — research findings and size analysis that motivated this decision
- Issue #5 — 2015 UDS schema inconsistency (symptom of the runtime-download fragility this ADR addresses)
- [UDS Mapper](https://udsmapper.org) — original data source
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html) — package size constraints referenced in the size analysis
