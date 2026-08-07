# Label Vocabulary

This file is the source of truth for GitHub Issue / PR labels in this
repository. The `backlog/SKILL.md` and `backlog_retrospective/SKILL.md`
skills read this file when present; they fall back to the embedded
baseline only when this file is absent.

The vocabulary below is the **cross-repo canonical baseline for this toolkit**
plus any **repo-specific extensions** that follow it.

## Baseline (cross-repo canonical, 35 labels)

The full canonical inventory: 7 Type + 6 Status + 3 Priority + 8 Score
(4 `tier/*` default + 4 `wsjf/*` opt-in) + 3 Gate profile + 4 Meta
+ 4 Close-reason = 35 labels.

These labels are expected to exist with the same name, color, and
purpose in every consumer repo using this toolkit (`example-consumer-repo`,
`<org>/<analytics-repo>`, `<org>/<app-repo>`, `<org>/<data-repo>`,
`<org>/<service-repo>`).

### Type — what kind of work is this

| Label            | Color     | Purpose                                                                  |
|------------------|-----------|--------------------------------------------------------------------------|
| `bug`            | `#d73a4a` | Something isn't working                                                  |
| `enhancement`    | `#a2eeef` | New feature or request                                                   |
| `documentation`  | `#0075ca` | Improvements or additions to documentation                               |
| `tech-debt`      | `#fbca04` | Refactoring, cleanup, robustness, scaling, or other internal-quality work |
| `epic`           | `#5319E7` | Parent issue for a thematic body of work; sub-issues linked              |
| `audit-finding`  | `#B60205` | Issue identified during an annual audit                                  |
| `qc`             | `#D93F0B` | QC finding from automated or independent audit                           |

### Status — where is this in the workflow

| Label       | Color     | Purpose                                                                            |
|-------------|-----------|------------------------------------------------------------------------------------|
| `qc-fixed`        | `#0E8A16` | QC finding auto-fixed by tooling                                                   |
| `blocked`         | `#E11D48` | Cannot proceed — waiting on data, a decision, or another issue                     |
| `UAT`             | `#FBCA04` | Awaiting user acceptance testing                                                   |
| `needs-grooming`  | `#cfd3d7` | Quick-captured by `quick_capture/SKILL.md`; missing AC / WSJF / parent epic. Cleared by `backlog_grooming/SKILL.md`. |
| `needs-triage`    | `#cfd3d7` | Auto-filed or freshly groomed; awaiting routing decision by `triage/SKILL.md`. Cleared by triage. |
| `in-progress`     | `#0E8A16` | Issue has an active implementation plan in flight. Set by `implementation_plan/SKILL.md` on `Status: in-progress` transition; cleared on `ready-for-pr` / `shipped` / `blocked` (the latter swaps in `blocked`). Read by `resume/SKILL.md`, `next_action/SKILL.md`, and `workflow_audit/SKILL.md` Cat 3 to identify resumption candidates and detect stale in-flight work. |

> **Note on description-length divergence.** GitHub's label-description field is capped at 100 characters, so the on-repo `description` for `in-progress` (and any other label whose Purpose prose here exceeds the cap) is necessarily a terser one-liner than the Purpose column above. The Purpose column in this file is the canonical doc-of-record; the repo description is a UI hint. Treat the on-repo description as a pointer back here, not as a competing source of truth.

### Priority — how urgent

| Label              | Color     | Purpose (default)                                          |
|--------------------|-----------|------------------------------------------------------------|
| `priority/blocker` | `#B60205` | Severe impact, immediate attention required                |
| `priority/high`    | `#D93F0B` | Significant impact, address soon                           |
| `priority/medium`  | `#FBCA04` | Moderate impact, address as capacity allows                |

Repos may specialize the *purpose* text for their domain in the
"Repo customizations" section below; the *names and colors* are
fixed.

### Gate profile — how much ceremony this task earns (per [ADR-0014](../docs/decisions/ADR-0014-task-weight-gate-profiles.md))

Assigned at triage (`triage/SKILL.md`) by the Product Owner. `gate-profile/standard`
is the fail-safe default when no label is present — an unclassified task never
silently gets the light treatment.

| Label                  | Color     | Purpose                                                                                                     |
|------------------------|-----------|---------------------------------------------------------------------------------------------------------------|
| `gate-profile/light`   | `#0E8A16` | Plan-handoff + retro only; code-review and changelog gates are skippable via the `_no-code-review:` / `_no-prep-gate:` markers. Doc-only edits, single-file fixes under a small LOC threshold, mechanical renames. |
| `gate-profile/standard`| `#FBCA04` | All five gates (today's default). Ordinary feature/bugfix work. Fail-safe default when unlabeled.             |
| `gate-profile/full`    | `#B60205` | All five gates, plus a mandatory Tech Lead review pass before merge. Architecturally significant changes.     |

### Score — backlog-item prioritization signal (per [ADR-0014](vendored-decisions/ADR-0014-wsjf-opt-in-lightweight-default.md))

Two mechanisms, resolved by `_partials/scoring-mode.md`'s single-sourced read of `.agent.config.yml`'s `wsjf.enabled` key. **Conflict rule (unchanged in both modes):** when an issue carries both a `priority/*` label and a `tier/*` or `wsjf/*` label, `priority/*` wins (executive priority overrides quantitative/qualitative score).

**`tier/*` — default, direct impact/effort classification.** Applied when `wsjf.enabled` is absent or `false` (every repo that has not explicitly opted in). A direct 2×2 read — no arithmetic, no computed-boundary risk. Documented in `_partials/scoring-mode.md`.

| Label               | Color     | Meaning (impact × effort)       |
|---------------------|-----------|----------------------------------|
| `tier/quick-win`    | `#0E8A16` | Do first — high impact, low effort |
| `tier/big-bet`      | `#5319E7` | Schedule deliberately — high impact, high effort |
| `tier/fill-in`      | `#cfd3d7` | Opportunistic — low impact, low effort |
| `tier/reconsider`   | `#B60205` | Justify before committing — low impact, high effort |

**`wsjf/*` — opt-in, quantitative WSJF bucket.** Applied only when a repo sets `wsjf.enabled: true` in `.agent.config.yml`. WSJF (Weighted Shortest Job First) is documented in [`CONTRIBUTING.md`](CONTRIBUTING.md) §"WSJF prioritization" and computed by `backlog/SKILL.md` per `_partials/wsjf-scoring.md` (v2.0.0+). Each issue carrying a WSJF body block also gets exactly one bucket label. The override (when `priority/*` disagrees) is recorded as a row inside the WSJF body block.

| Label             | Color     | Score range                                                              |
|-------------------|-----------|--------------------------------------------------------------------------|
| `wsjf/critical`   | `#B60205` | ≥ 5.0                                                                    |
| `wsjf/high`       | `#D93F0B` | 2.0 – 5.0                                                                |
| `wsjf/medium`     | `#FBCA04` | 0.5 – 2.0                                                                |
| `wsjf/low`        | `#cfd3d7` | < 0.5                                                                    |

### Meta

| Label      | Color     | Purpose                                  |
|------------|-----------|------------------------------------------|
| `question`     | `#d876e3` | Further information is requested         |
| `no-changelog` | `#cfd3d7` | PR is internal/refactor; opts out of the `pr_gate_changelog/SKILL.md` changelog gate |
| `qc-review-cycling` | `#B60205` | `code_review/SKILL.md`'s QC-round loop guard escalated: 3+ gate rounds without convergence on a must-fix (BLOCKER or HIGH) finding (or an oscillating BLOCKER/HIGH finding); the dev agent stops auto-resolving and human adjudication is needed. Set on the PR if one exists, otherwise on the linked closing issue(s) (Create mode, before `gh pr create` runs). Set by `code_review/SKILL.md` itself (its QC-round loop guard step 5), invoked in gate mode by `pr_gate_code_review/SKILL.md`; cleared manually once the target converges or is closed. |
| `qc-review-round-<N>` | `#D93F0B` | Dynamic, numbered label (e.g. `qc-review-round-1`, `qc-review-round-2`, …) tracking the current QC-round loop guard round count on its target — the PR if one exists, otherwise the linked closing issue(s). Swapped (new added and confirmed before the old is removed) by `code_review/SKILL.md` on every gate-mode invocation; replaces the prior local-report-file-count approach so state survives fresh clones/sessions. A genuinely new PR/issue starts at round 1 with no round label to inherit, but this label is **not** automatically cleaned up if a target is closed and later reopened — clearing it remains a manual operator step, same as `qc-review-cycling`. |

### Close-reason — set BEFORE closing an issue without a full retro

The `backlog_retrospective/SKILL.md` carve-out (no full retro
required) only fires when one of these labels is present on the
issue at close time. `--reason` alone is not sufficient.

| Label         | Color     | Purpose                                       |
|---------------|-----------|-----------------------------------------------|
| `duplicate`   | `#cfd3d7` | This issue or pull request already exists     |
| `wontfix`     | `#ffffff` | This will not be worked on                    |
| `invalid`     | `#e4e669` | This doesn't seem right                       |
| `not-planned` | `#cfd3d7` | Closed without action; out of scope           |

## Removed (do NOT add to any consumer repo using this toolkit)

These two GitHub defaults are public-OSS conventions that don't fit
private internal repos. If they exist in a repo, they should be
deleted as part of label migration.

- `good first issue`
- `help wanted`

## Usage rules

- **Every new issue must carry at least one Type label.** The
  `backlog/SKILL.md` enforces this in its issue-creation step.
- **Every open issue must carry exactly one `gate-profile/*` label**,
  assigned at triage (`triage/SKILL.md`), before `start_work/SKILL.md`
  permits implementation to begin (per [ADR-0014](../docs/decisions/ADR-0014-task-weight-gate-profiles.md)).
  `gate-profile/standard` is the fail-safe default *at classification
  time* when the correct tier is ambiguous — it does not exempt an issue
  from needing a label; `start_work/SKILL.md` treats an absent label as
  a genuine blocking finding, not an advisory.
- **Most new issues should also carry a Priority label.** Skip only
  for low-stakes or speculative items where priority is undecided.
- **Status labels are added/removed during the issue's lifetime**,
  not at creation. `blocked` and `UAT` are particularly important
  to keep current.
- **Close-reason labels are required for non-completed closes.**
  Add the appropriate label *before* `gh issue close --reason ...`.
  The retro skill checks for the label, not the reason.
- **Color collisions are intentional in the priority axis.**
  `priority/blocker` shares `#B60205` with `audit-finding`, and
  `priority/high` shares `#D93F0B` with `qc` — both signal "serious"
  consistently.

## Repo customizations

> Add or override the baseline below. Specifying a label here that
> appears in the baseline overrides the baseline entry's *purpose*;
> name and color stay fixed.

### `example-consumer-repo` (example customization)

An example dataset-oriented consumer repo might specialize the priority axis
to dataset-scale impact like this:

| Label              | Purpose (specialized)                          |
|--------------------|------------------------------------------------|
| `priority/blocker` | Will fail at less than 10 datasets             |
| `priority/high`    | Will degrade at 10–30 datasets                 |
| `priority/medium`  | Pain mounts at 30+ datasets                    |

Repo-only labels (no cross-repo equivalent):

| Label     | Color     | Class | Status     | Notes                                                                                          |
|-----------|-----------|-------|------------|------------------------------------------------------------------------------------------------|
| `spike`   | `#A371F7` | Type  | active     | Time-boxed discovery / exploration issue managed by [`discovery/SKILL.md`](skills/discovery/SKILL.md). Counts as a Type label for DoR purposes — a `spike`-only issue is fully Type-labeled. Closure must select an artifact (requirements doc / scoped issues / ADR / not-planned decision summary); enforced by `discovery_skill close-spike`, audited post-hoc by `workflow_audit/SKILL.md` Cat 3. Repo-only for now; cross-repo promotion deferred until sibling repos request it. |

## Migration TODOs (per-repo follow-ups, tracked separately)

When this file is rolled out to the four sibling repos, file one
follow-up issue per repo to:

- Delete `good first issue` and `help wanted`.
- Delete deprecated overlap labels: `cleanup`, `refactor`, `robustness`, `future` (re-label issues to `tech-debt` first).
- Normalize colors: `qc` → `#D93F0B`, `qc-fixed` → `#0E8A16`, `blocked` → `#E11D48`.
- Add missing baseline labels (`tech-debt`, `audit-finding`, the `priority/*` set, `not-planned`).
- Add a `.github/LABELS.md` mirroring the structure here, with any repo-specific specializations.
