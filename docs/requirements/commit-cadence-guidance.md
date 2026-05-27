---
status: draft
owner: chris-prener
created: 2026-04-27
last-updated: 2026-04-27
related-epic(s): #123 (Skills consistency & cleanup)
related-adr(s): .github/vendored-decisions/ADR-0006-institutional-gate-vs-in-flight-critique.md
supersedes: (n/a)
---
# Requirements: Commit-cadence guidance

> **Phase 1 (Discovery) artifact for #130.** This document captures the audit, the canonical-home decision, the proposed rule, and the conditional Phase 2 scope. Phase 2 is filed as a separate issue and lands as a follow-up PR. This document does **not** itself change any skill or instruction file.

## 1. Background

Recent PRs in this repo have settled into a 1-or-2-commits-per-PR shape that is the wrong granularity for non-trivial work:

- **PR #126** (`post_merge/SKILL.md`, ~10 files): **1 commit** for the whole thing.
- **PR #127** (backlog partials refactor, **14 files**, 5 new partials + 8 modified, **+453 LOC**): **2 commits** — one mega-commit (`refactor(skills): extract backlog partials and add user-story format`) plus one `fix(skills): address rubber-duck findings` follow-up.

Two commits across 14 files is too coarse:

- A reviewer cannot isolate "the partials extraction" from "the new User-story Step A.5" from "the template updates" from the workflow_audit cross-ref bumps.
- `git bisect` over the refactor commit lands on a 14-file change and forces another bisection layer.
- The shape of the diff is hidden inside one squash.

The current behavior is consistent because **none of the workflow skills, neither `.github/copilot-instructions.md` nor `../../.github/CONTRIBUTING.md`, says anything about commit granularity** (see §2 audit). The agent defaults to "one commit per coherent work session," which collapses to one mega-commit on big jobs. The 1-commit pattern is even *rewarded* by the existing rubber-duck-fixes pattern: the agent batches everything else into the first commit because it knows there will only be one more.

The user's stated trade-off is the inverse of what the repo currently optimizes for: **fewer PRs, more commits per PR.** Bigger PRs are fine because they ship coherent units of work; the commits *inside* the PR are what should be granular.

This document is the Phase 1 Discovery output: an audit + a canonical-home decision + the rule itself + a conditional Phase 2 scope. Phase 2 lands the chosen changes.

## 2. Sources

- **S1.** Audit of every workflow file referenced in #130's acceptance criteria (`.github/copilot-instructions.md`, `pull_request/SKILL.md`, `code_review/SKILL.md`, `post_merge/SKILL.md`, `backlog/SKILL.md`, `epic/SKILL.md`, `epic_retrospective/SKILL.md`, `session_start/SKILL.md`, `../../.github/CONTRIBUTING.md`), 2026-04-27. **Finding:** No granularity guidance in any of these files. The closest signals are mechanical (PR-body bullet grouping in `pull_request/SKILL.md` line 233 says "Group by commit when there are multiple") and structural (the `code_review/SKILL.md` BLOCKER → "fix-now-and-rerun" path naturally produces a second commit). The phrase "commit" appears 27 times across these files; not one occurrence sets a granularity expectation.
- **S2.** Recent PRs as evidence: PR #126 (1 commit / 10 files), PR #127 (2 commits / 14 files / 453 LOC), PR #160 (3 commits / 15 files — the docs-hygiene bundle that immediately preceded this Discovery PR; demonstrates the cadence is achievable when explicitly considered, but #160 was authored with #130 in mind, so it does not yet falsify the "agent defaults to mega-commit" claim).
- **S3.** [ADR-0006](../../.github/vendored-decisions/ADR-0006-institutional-gate-vs-in-flight-critique.md) (2026-04-27). Establishes that institutional gates judge the final shipped diff and that **opt-out markers should not bypass them**. The `_no-code-review:_` marker was deprecated as part of ADR-0006. The institutional direction is moving *away* from bypass markers and *toward* deterministic, non-bypassable behavior. This frames the soft-check decision in §3 R3.
- **S4.** Pre-PR rubber-duck pass on this Discovery's plan (2026-04-27 in-session; not externally archived). Adopted in full: surfaced the "logical separability vs. fixed count" framing, the meaningful-files heuristic, the canonical-home-vs-operational-surfacing distinction, and the stale-section-numbering catch.
- **S5.** Issue #130 strawman (the issue body itself). Adopted in part — the canonical-home framing was correct, the rule strawman needed refinement, the soft-check question was rejected (no marker, but a deterministic surfacing line is added; see R3).
- **S6.** Stale section numbering catch: the issue body refers to "`../../.github/CONTRIBUTING.md` §6 (Commit conventions)," but in the current `../../.github/CONTRIBUTING.md` §6 is "Labels" and the commits content lives in §5 ("How to open a PR") line 72 as a single bullet that defers to `.github/copilot-instructions.md`. Phase 2 must update §5, not §6.

## 3. Requirements

### R1. Canonical home: `.github/copilot-instructions.md` "Commits" block

- **Rationale:** The cadence rule is a *heuristic*, not a procedure. It belongs in always-loaded context where the agent encounters it before authoring each commit, not in a skill the agent must invoke. A new `commit_skill.md` would be procedural overkill (skill count is already 41; adding one for a one-line heuristic is inflationary), and the rule has no "operational" steps that warrant a skill (no multi-step workflow, no GitHub-state mutation, no inputs to validate). The skill template's value-add — frontmatter triggers, formal inputs/outputs, success criteria — does not apply to a single-paragraph heuristic.
- **Source(s):** S1 (audit), S4 (rubber-duck), S5 (issue strawman).
- **Acceptance criteria:** Phase 2 PR adds a `**Cadence**:` bullet (or short paragraph) to the existing `### Commits` block in `.github/copilot-instructions.md` (currently lines 49-56) that captures R2's rule. `../../.github/CONTRIBUTING.md` §5 (line 72) gains a single-line mirror that points readers at the canonical home.
- **Out of scope:** Creating `commit_skill.md`. Moving the canonical home elsewhere later (e.g., to a future `commit_skill.md`) is not foreclosed, but is not expected.
- **Notes:** This separates the **canonical policy home** (always-loaded copilot-instructions) from the **operational surfacing point** (R3, in `pull_request/SKILL.md`). They are not the same thing; R1 picks the policy home; R3 decides whether and how to surface it operationally.

### R2. The rule itself

- **Rationale:** The original strawman ("≥ 3 commits for PRs touching ≥ 5 files OR ≥ 200 LOC") risked becoming a pseudo-rule — a mechanical target rather than a heuristic. The principle is **logical separability**, with thresholds as smell detectors, not gates.
- **Source(s):** S4 (rubber-duck refinement of S5 strawman).
- **Acceptance criteria:** Phase 2 lands the following text (or a faithful equivalent) in the canonical home:

  > **Cadence.** Each commit should be a single logical change reviewable in isolation. For substantial PRs — roughly ≥ 5 *meaningful* files or ≥ 200 *non-generated* LOC — pause before committing and split along natural seams (e.g., extract partial X / extract partial Y / wire consumers / update docs / update CHANGELOG). Substantial PRs often need 3+ commits, but the rule is logical separability, not a fixed count. The `code_review/SKILL.md` "fix-now-and-rerun" commit is *additive*, not a substitute for the initial logical split.
  >
  > "Meaningful files" excludes pure renames, generated outputs, lockfiles, and CHANGELOG-only churn from the file-count signal. Treat docs mirrors (e.g., `../../.github/CONTRIBUTING.md` reflecting `.github/copilot-instructions.md`) as context, not independent evidence of multiple logical commits.

- **Out of scope:** A per-commit-message linter. A specific minimum or maximum commit count. Rewriting history on already-merged PRs.
- **Notes:** Three load-bearing pieces:
  1. **Logical separability is the rule.** "3+" is a smell, not a target.
  2. **Heuristic excludes mechanical noise** — file count should be of *meaningful* files; LOC should be *non-generated*.
  3. **Anti-pattern explicitly named:** "rubber-duck-fixes is additive" prevents the observed PR #127 failure mode where the second commit became a mock-cadence cover.

### R3. Operational surfacing in `pull_request/SKILL.md` Step 1 — informational only, no marker, no gate

- **Rationale:** The audit shows `pull_request/SKILL.md` Step 1 already runs a pre-flight (clean tree, pushed branch, scan recent commits for issue references). It is the natural last-recoverable moment where the agent can be reminded of cadence before committing to the PR shape. **However**, per S3 (ADR-0006 direction) and the rubber-duck's analysis, this must be **purely informational** — no opt-out marker, no BLOCKER gate, no audit drift check. Adding a 4th opt-out marker (`_no-commit-cadence:_`) would directly contradict the institutional direction set days ago in ADR-0006.
- **Source(s):** S1 (Step 1 already does pre-flight), S3 (ADR-0006 direction), S4 (rubber-duck).
- **Acceptance criteria:** Phase 2 adds a single deterministic line in `pull_request/SKILL.md` Step 1's pre-flight output of the form:

  > Branch shape: `<N>` files changed, `+<A> -<D>` LOC, `<K>` commits on branch. If the branch is substantial and `K` is low, the agent should consider splitting via `git rebase -i` before continuing — see `.github/copilot-instructions.md` Commits block for the cadence rule.

  - The line is **always emitted** (no opt-out, no skip).
  - It does **not** halt the flow; Step 1 continues unconditionally.
  - It does **not** trigger an `--apply-fixes` or any state mutation.
  - The skill itself takes a MINOR version bump (behavior extension: Step 1 pre-flight now emits the diff-shape advisory line).
- **Out of scope:**
  - A `_no-commit-cadence:_` opt-out marker. **Explicitly rejected.**
  - A `workflow_audit/SKILL.md` Cat 2 drift check for cadence. (No marker → no drift to detect. The cadence rule is a soft preference; auditing it post-hoc would re-create exactly the bypass-marker dynamic ADR-0006 deprecates.)
  - Defining what "substantial" means as a hard threshold inside the skill — the skill emits the diff shape; the agent applies the R2 heuristic.
- **Notes:** This is the operational hook that distinguishes "rule-in-context-only" (which we have evidence is insufficient on its own — see PR #126 / #127) from "rule-in-context plus a checkpoint at the last recoverable moment." The checkpoint is informational, not enforcement. If future evidence shows agents still ship 1-commit mega-PRs after this lands, the next escalation step is a deterministic check (e.g., `pull_request/SKILL.md` *prompts* the operator when the diff shape exceeds the heuristic) — but **not** a marker. We have learned the cost of markers.

### R4. Phase 2 scope — conditional on R3

- **Rationale:** Phase 2 is the implementation PR. Its scope depends on R1+R2+R3.
- **Source(s):** S4 (Phase-2 scoping critique).
- **Acceptance criteria:** Phase 2 issue (filed at the same time this Discovery PR opens) carries the following acceptance criteria:

  **Always:**
  - [ ] Update `.github/copilot-instructions.md` `### Commits` block per R1 + R2.
  - [ ] Update `../../.github/CONTRIBUTING.md` §5 (line 72) mirror per R1 (single-line cross-reference; canonical home is copilot-instructions).
  - [ ] Add `[Unreleased] / Changed` entry in `CHANGELOG.md`.

  **If R3 is implemented (default = yes):**
  - [ ] Update `.github/skills/pull_request/SKILL.md` Step 1 to emit the diff-shape line per R3.
  - [ ] Bump `pull_request/SKILL.md` MINOR version (behavior extension is the rationale: Step 1 pre-flight now emits an informational diff-shape line).
  - [ ] Update the skill's `## Versioning` history.
  - [ ] Update `pull_request/SKILL.md` frontmatter `description` field to document the Step 1 diff-shape surfacing (the description field summarizes the skill's evolution; every MINOR bump from v2.0.0 forward is reflected there).
  - [ ] Update `.github/copilot-instructions.md` Persistent-memory layer's `pull_request/SKILL.md` reference if the new behavior warrants a callout.

  **Explicitly NOT in Phase 2:**
  - [ ] No `commit_skill.md`.
  - [ ] No `_no-commit-cadence:_` opt-out marker.
  - [ ] No `workflow_audit/SKILL.md` Cat 2 drift check.
  - [ ] No ../../.github/CONTRIBUTING.md §6 edit (§6 is "Labels"; the commits content lives in §5 — this issue's strawman was based on stale section numbering, see S6).

- **Out of scope:** Anything beyond the bullet list above. Phase 2 is a small, focused PR.
- **Notes:** Phase 2 is roughly 4 file edits + a version bump + a CHANGELOG entry. It is itself a good candidate for the cadence rule it introduces — likely 2 commits (instructions + CONTRIBUTING in one; pull_request/SKILL.md + version bump + CHANGELOG in the other) given its small surface, demonstrating the rule is also reasonable for small PRs.

## 4. Decisions log

| # | Decision | Rationale | Source |
|---|---|---|---|
| D1 | Canonical home is `.github/copilot-instructions.md`, not a new `commit_skill.md`. | Heuristic, not procedure; always-loaded > invoked-on-demand for a one-paragraph rule. | R1 |
| D2 | Rule is "logical separability"; "3+ commits" is a smell, not a target. | Avoid pseudo-rule lock-in; preserves the issue's "no specific commit count" out-of-scope. | R2 |
| D3 | Heuristic explicitly excludes mechanical noise (renames, generated, CHANGELOG). | Reduces false positives; aligns the rule with the user's actual intent. | R2 |
| D4 | Add deterministic diff-shape surfacing in `pull_request/SKILL.md` Step 1. **No opt-out marker.** | Last-recoverable checkpoint without re-creating the bypass-marker anti-pattern ADR-0006 just deprecated. | R3 |
| D5 | Anti-pattern explicitly named: "rubber-duck-fixes is additive, not a substitute." | Addresses the observed PR #127 failure mode. | R2 |
| D6 | No `workflow_audit/SKILL.md` Cat 2 drift check. | No marker means no drift to detect; cadence is a soft preference, post-hoc audit would re-create the bypass dynamic. | R3 |
| D7 | Phase 2 updates `../../.github/CONTRIBUTING.md` §5, not §6 — the issue's strawman section numbering is stale. | §6 is "Labels"; the commits guidance currently lives in §5 line 72. | S6 |

## 5. Open questions

None blocking Phase 2. If the diff-shape surfacing in R3 turns out to be too noisy in practice (e.g., emitted on every trivial PR), the follow-up should be to add a `--minimal` whitelist (parallel to `code_review/SKILL.md`'s `--minimal`), not an opt-out marker.

## 6. Phase 2 issue

Filed alongside this Discovery PR. Title: `[Tech Debt] Implement commit-cadence guidance per docs/requirements/commit-cadence-guidance.md (#130 Phase 2)`. Carries `tech-debt` + `priority/medium` + `wsjf/medium` (parallel to #130's own scoring; the implementation is small but compounds across every future PR).

---

## Where this fits

- **Quadrant:** intent (this doc captures the *why* and *what* of a rule that lives in `.github/copilot-instructions.md`).
- **See also:** [`../../.github/copilot-instructions.md`](../../.github/copilot-instructions.md) Commits block (canonical home of the rule itself, post-Phase-2), [`../../.github/skills/pull_request/SKILL.md`](../../.github/skills/pull_request/SKILL.md) Step 1 (operational surfacing, post-Phase-2), [`ADR-0006`](../../.github/vendored-decisions/ADR-0006-institutional-gate-vs-in-flight-critique.md) (frames the no-marker decision in R3).
- **Related skills:** `pull_request/SKILL.md`, `code_review/SKILL.md` (interaction with the "fix-now-and-rerun" commit), `post_merge/SKILL.md` (no interaction; cadence is a pre-merge concern).
