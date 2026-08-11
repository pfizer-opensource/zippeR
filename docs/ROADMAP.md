---
last_updated: "2026-08-11"
---
# Roadmap — zippeR

## Now (in progress)

_No epics in progress._

## Next (planned)

- **[Epic H] User-Facing API Ergonomics & Dependency Reduction** (#109) — Self-contained `zi_`-prefixed API surface plus Phase 3 dependency decisions; 0/3 sub-issues closed
- **[Epic J] v0.3.0 Release** (#111) — Ship 2024 ZCTA support to CRAN; sub-issues to be filed at kickoff
- **[Epic K] Vendored Workflow Toolkit Adoption & Governance** (#123) — Make the vendored toolkit's labels and ADR corpus real, so lifecycle gates resolve from decisions rather than absence; 0/4 sub-issues closed

## Later (aspirational)

- **Deprecated argument removal** (#71) — Remove `input_zip` and `dict` from `zi_crosswalk()`; time-gated to early 2027

## Recently Shipped

- **[Epic I] CI & Development Infrastructure Health** (#110) — closed 2026-08-10
- **[Epic E] Code & Test Quality Audit** (#48) — closed 2026-08-06
- **[Epic G] CRAN Release Preparation** (#79) — closed 2026-06-02
- **[Epic F] New Features & Enhancements** (#49) — closed 2026-06-01
- **[Epic C] Documentation Improvements** (#24) — closed 2026-05-29
- **[Epic B] Code Style & Error Handling** (#23) — closed 2026-05-28
- **[Epic D] Package Infrastructure & CRAN Readiness** (#25) — closed 2026-05-28
- **[Epic A] Test Coverage & Quality** (#22) — closed 2026-05-28

## Epic dependencies

```mermaid
graph TD
  H["#109 Epic H — API ergonomics"] -.optional.-> J["#111 Epic J — v0.3.0 Release"]
  K["#123 Epic K — Toolkit adoption"] -.preferred.-> J
```

- [Epic I] closed 2026-08-10 — its precondition on [Epic J] (release checks running against a trustworthy pipeline) is satisfied; the dependency edge is resolved and removed from the graph.
- [Epic H] is optional for the release; #108 (`zi_census_api_key()`) is a good candidate to include if it lands in time.
- [Epic K] is preferred-but-not-blocking for [Epic J] — the release cycle runs more meaningfully once lifecycle gates resolve from recorded decisions, but no release artifact depends on it. Internally, #120 gates both #118 and #119.
