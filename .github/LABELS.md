# Label Vocabulary

This file is the source of truth for GitHub Issue / PR labels in this
repository. The `backlog/SKILL.md` and `backlog_retrospective/SKILL.md`
skills read this file when present; they fall back to the embedded
baseline only when this file is absent.

The vocabulary below is the **EVGen cross-repo canonical baseline**
plus any **repo-specific extensions** that follow it.

## Baseline (cross-repo canonical, 26 labels)

The full canonical inventory: 7 Type + 6 Status + 3 Priority + 4 Score
+ 2 Meta + 4 Close-reason = 26 labels.

These labels are expected to exist with the same name, color, and
purpose in every EVGen repository (`evgen-open-data-pipelines`,
`evgen-lymeDataHub-data`, `tbd-tbeDataHub-data`, `tbd-tbeDataHub`,
`tbd-lymeDataHub`).

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

### Score — quantitative WSJF prioritization bucket

WSJF (Weighted Shortest Job First) is documented in [`CONTRIBUTING.md`](CONTRIBUTING.md) §"WSJF prioritization" and computed by `backlog/SKILL.md` (v1.5.0+). Each issue carrying a WSJF body block also gets exactly one bucket label. **Conflict rule:** when an issue carries both a `priority/*` label and a `wsjf/*` label, `priority/*` wins (executive priority overrides quantitative score). The override is recorded as a row inside the WSJF body block.

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
| `no-changelog` | `#cfd3d7` | PR is internal/refactor; opts out of the `pull_request/SKILL.md` changelog gate |

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

## Removed (do NOT add to any EVGen repo)

These two GitHub defaults are public-OSS conventions that don't fit
private internal repos. If they exist in a repo, they should be
deleted as part of label migration.

- `good first issue`
- `help wanted`

## Usage rules

- **Every new issue must carry at least one Type label.** The
  `backlog/SKILL.md` enforces this in its issue-creation step.
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

### `evgen-open-data-pipelines` (this repo)

The priority axis is specialized to dataset-scale impact (this repo
ships reference data packages; the failure modes are scale-dependent):

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
