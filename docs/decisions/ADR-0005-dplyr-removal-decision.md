# ADR-0005: Remove dplyr as a direct Import, replacing ~110 call sites with Base R

**Status:** accepted
**Date:** 2026-08-11
**Decision-makers:** @chris-prener
**Related epic(s):** #109 ([Epic H] User-Facing API Ergonomics & Dependency Reduction), #48 ([Epic E], predecessor)
**Related requirements:** N/A
**Source(s):** Issue #86 (research + recommendation comment: https://github.com/pfizer-opensource/zippeR/issues/86#issuecomment-5258035897)

## Context

Issue #86 was a time-boxed research spike, filed under Epic H (#109), evaluating
whether `dplyr` should be removed as a direct dependency of `zippeR`. `dplyr` is
used across ~110 call sites in 9 files (`zi_aggregate.R`, `zi_crosswalk.R`,
`zi_get_demographics.R`, `zi_get_geometry.R`, `zi_label.R`, `zi_list_zctas.R`,
`zi_load_crosswalk.R`, `zi_load_labels.R`, `zi_prep_hud.R`), all via
fully-qualified `dplyr::` calls (no `library(dplyr)` attachment, so no
NAMESPACE `importFrom` cleanup is required either way).

The research produced three findings that together motivate this decision:

1. **Call-site inventory.** ~110 sites confirmed (within counting-method noise
   of the issue's original ~108 estimate), covering `filter()`, `select()`,
   `mutate()`, `arrange()`, `rename()`/`rename_with()`, `summarise()`,
   `left_join()`, `group_by()`, `all_of()`/`any_of()`, `everything()`, and
   `bind_rows()`. Each pattern has a well-understood, mechanical Base R
   translation — this is a large but low-ambiguity rewrite, not a redesign.

2. **Performance benchmark** (41,099-row real-world UDS crosswalk fixture
   joined against a synthetic 33,139-row table, `microbenchmark`, 20 reps):
   dplyr is faster for joins (~3.7x) and filter→select→mutate→arrange
   pipelines (~4.8x); Base R (`tapply`) is faster for grouped aggregation
   (~3.8x vs. `group_by()`/`summarise()`). At zippeR's real operating scale
   (tens of thousands of rows, not millions, and none of these functions are
   hot paths in a tight loop), every operation completes in single- or
   low-double-digit milliseconds regardless of implementation. **Performance
   is immaterial and does not favor either side.**

3. **Transitive dependency analysis.** `dplyr` has 19 transitive dependencies.
   `tidycensus` + `tigris` (already direct Imports of zippeR) together pull 61
   transitive dependencies — a strict superset of all 19 of dplyr's.
   **Removing dplyr yields zero net savings in the transitive dependency
   tree.** Install size and install time are unaffected by this decision.

Given finding #3, the usual "remove for dependency/performance savings"
rationale does **not** apply here, and a future reader should not assume this
decision was install-size or performance motivated — it was neither. The
actual driver is **direct-dependency maintainability**: `dplyr` remains a
large, actively-evolving **direct** Import with its own API surface,
deprecation cycles, and breaking-change history (e.g., past
`summarise()`/`.groups` semantics changes, NSE/`rlang` quosure evolution).
Every direct Import is a maintenance liability independent of what else is
already installed transitively — it is one more upstream API zippeR must
track, pin against, and re-test on every dplyr major/minor release. Dropping
it narrows zippeR's direct Import surface and decouples the package from
dplyr's release cadence, at no performance cost.

Per Epic H's constraints, this decision must preserve:

- **Character-string column inputs must keep working** — this rewrite does
  not touch zippeR's public API contract.
- **No `rlang` may be introduced** as a replacement dependency — the point is
  to *reduce* the direct dependency surface, not trade one large direct
  dependency for another.

This ADR's scope is the `dplyr` removal decision only. The separate
bare-name NSE question (#88) explicitly depends on this decision per Epic
H's dependency graph and is out of scope here; #88 should now proceed using
Base R `substitute()`/`deparse()`/`match.call()`, consistent with dplyr-free
internals and the no-`rlang` constraint above.

## Decision

We will remove `dplyr` as a direct Import of `zippeR`, replacing all ~110
call sites across the 9 named files with Base R equivalents. This ADR
authorizes and scopes the removal; it does **not** execute it — implementation
is deferred to a follow-up epic (see Phased plan below).

**Alternatives considered and rejected:**

| Option | Verdict |
|---|---|
| Retain `dplyr`, no action | Rejected — leaves an unnecessary large direct dependency with its own breaking-change history, for no offsetting benefit once performance is shown to be a wash |
| Remove `dplyr` for dependency-count savings | Rejected as the *stated rationale* — finding #3 shows zero transitive savings; citing this as the reason would misrepresent the actual driver to future readers |
| Remove `dplyr` for performance | Rejected as the *stated rationale* — finding #2 shows performance is immaterial at zippeR's scale, and dplyr is in fact faster for two of the three benchmarked patterns |
| Partial removal (e.g., only aggregation call sites, where Base R wins) | Rejected — leaves dplyr as a direct Import regardless, forfeiting the maintainability benefit while adding a mixed-style codebase |

## Consequences

- **Easier:** zippeR's direct Import list shrinks by one large,
  actively-evolving package; future dplyr major/minor releases (deprecations,
  `.groups` / NSE semantics changes, etc.) no longer require zippeR
  re-testing or pinning. `R CMD check` NOTE surface tied to dplyr's own
  dependency churn is eliminated.
- **Harder:** ~110 call sites must be mechanically translated to Base R
  across 9 files, each requiring test coverage to be maintained through the
  rewrite. Some patterns (multi-column `select()`/`rename_with()` with
  `all_of()`/`any_of()`/`everything()`) require more verbose Base R
  equivalents (`[`, `setNames()`, `%in%`) than their dplyr counterparts,
  slightly increasing code verbosity in the affected functions.
- **Constraints:** The replacement code must not introduce `rlang` (or any
  other NSE-heavy dependency) as a substitute — the goal is a smaller direct
  Import surface, not a lateral trade. Character-string column input handling
  must be preserved exactly across the rewrite (no public API behavior
  change). #88 (bare-name NSE) is gated on this ADR and should use Base R
  `substitute()`/`deparse()`/`match.call()`.
- **Revisit when:** A future zippeR feature has a hard performance or
  expressiveness requirement that only `dplyr`/`tidyverse` idioms satisfy at
  a scale meaningfully larger than tens of thousands of rows, or if the
  Base R rewrite is found during implementation to introduce correctness
  regressions that cannot be resolved without disproportionate complexity.

## Phased plan

This ADR authorizes but does not execute the removal. Actual implementation
is scoped to a **follow-up epic** (to be filed separately — out of scope for
both #86 and this ADR), structured as:

- **Phase 1 — Scope and file the implementation epic.** File a dedicated
  epic with sub-issues grouped by function-pattern (filters/select, joins,
  grouped aggregation, renames/pipelines) or by file, referencing this ADR
  and issue #86 as the authorizing decision.
- **Phase 2 — Implement per-pattern or per-file.** Convert call sites in
  small, independently reviewable batches; maintain and extend test coverage
  at each step so no behavior regresses (character-string column input
  contract in particular). `#88`'s bare-name NSE work can proceed in parallel
  once this ADR is accepted, since it is no longer blocked by an
  as-yet-undecided dplyr question.
- **Phase 3 — Remove the dependency.** Once all ~110 call sites are
  converted and no `dplyr::` references remain in `R/`, remove `dplyr` from
  `DESCRIPTION` Imports and regenerate `NAMESPACE`/`man` via `devtools::document()`.
- **Phase 4 — Final verification.** Run `R CMD check` and confirm 0
  errors/warnings/notes before the removal PR(s) merge; confirm no residual
  `dplyr` references in vignettes, README, or `data-raw/` scripts.

## References

- Issue #86 — research findings and GO recommendation that motivated this decision
  (https://github.com/pfizer-opensource/zippeR/issues/86#issuecomment-5258035897)
- Issue #109 (Epic H) — acceptance criteria requiring a documented decision on dplyr removal
- Issue #48 (Epic E) — predecessor epic
- Issue #88 — bare-name NSE question, gated on this decision
