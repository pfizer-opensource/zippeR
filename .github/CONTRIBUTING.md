# Contributing to this repo

This file is the operational contract for working in this repo. It is deliberately lean — the **skills** in [`.github/skills/`](.github/skills/) are the source of truth for *how* each step is done; this file tells you *what* is expected and points you at the right skill.

## 1. Overview

This vendored-exact file is the operational contract for working in a repo that uses this toolkit. In consumer repos, replace this Overview paragraph with a repo-specific description of the deliverable, domain, and key structural invariants that contributors need to know. In this upstream copy, keep the summary generic: day-to-day conventions live in [`.github/copilot-instructions.md`](.github/copilot-instructions.md), and the skills directory remains the canonical source of truth for workflow behavior.

If you are an AI agent: read [`.github/copilot-instructions.md`](.github/copilot-instructions.md) first, then this file. The skills directory ([`.github/skills/`](.github/skills/)) is canonical for any workflow step.

## 2. How to file an issue

Always use [`.github/skills/backlog/SKILL.md`](.github/skills/backlog/SKILL.md) — it picks the right template, enriches the body with code references, and applies labels per [`.github/LABELS.md`](.github/LABELS.md).

**Unscoped problem?** When the answer to "what should we file?" is itself unknown (new dataset family, unfamiliar source data, unclear approach among alternatives), reach for [`.github/skills/discovery/SKILL.md`](.github/skills/discovery/SKILL.md) instead. It opens a time-boxed `spike` issue that produces an artifact (requirements doc / scoped issues / ADR / not-planned decision summary) at the end — avoiding both premature scope-baking and indefinite exploration.

Templates available in [`.github/ISSUE_TEMPLATE/`](.github/ISSUE_TEMPLATE/):

| Template | Use when… |
|---|---|
| `bug_report.md` | Broken functionality, incorrect behavior, regressions |
| `feature_request.md` | New capability or enhancement |
| `tech_debt.md` | Refactors, scaling cracks, paydown work, internal-quality improvements |
| `qc_finding.md` | QC finding (typically auto-filed by `run_dataset_qc/SKILL.md`) |

## 3. Definition of Ready (DoR)

An issue is **Ready** when every box below is checked. Until then, the issue should not be picked up for work — `backlog/SKILL.md`, `triage/SKILL.md`, and `backlog_grooming/SKILL.md` together enforce this. Issues carrying the `needs-grooming` Status label (filed via [`quick_capture/SKILL.md`](.github/skills/quick_capture/SKILL.md)) are explicit deferrals against DoR; pick-up should wait until grooming clears the label.

- [ ] **Problem statement** — concrete, testable. Names the failure mode or the missing capability.
- [ ] **User story** — a `## User story` block at the top of the body in the form *"As a `<role>`, I want `<capability>`, so that `<outcome>`"*. Pick `<role>` from the Standard roles below or supply free text. Minor mechanical chores may use the `**Mechanical change** — <one-line description>` escape hatch instead. See [`.github/skills/_partials/user-story.md`](.github/skills/_partials/user-story.md).
- [ ] **Proposal** — the approach to solving it (not just "investigate").
- [ ] **Acceptance criteria** — measurable, checklist form. "Done means…" must be falsifiable.
- [ ] **Out of scope** — explicit boundaries. What this issue is *not* doing.
- [ ] **Codebase context** — file paths and (where helpful) line numbers; references to related symbols.
- [ ] **Labels** — at minimum one **Type** label and one **Priority** label, picked per [`.github/LABELS.md`](.github/LABELS.md). **Status** labels (`blocked`, `UAT`) are added during the lifecycle.
- [ ] **Epic linkage** — every non-trivial issue links to a parent epic (label `epic`) via GitHub native sub-issue or, exceptionally, carries a `standalone — <reason>` line in the body. Enforced by `backlog/SKILL.md` (v1.4.0+).
- [ ] **Score label** *(recommended; required for non-trivial issues)* — `tier/*` (default) or `wsjf/*` (if the repo opts in via `.agent.config.yml`); see §"Backlog scoring" below. Tiny chores may skip it.
- [ ] **Dependencies** — other issues or external work this issue waits on, listed explicitly.

### Standard roles (for User story)

The canonical role list (from [`.github/skills/_partials/user-story.md`](.github/skills/_partials/user-story.md)) — pick one, or supply free text if none fit:

- `data consumer` — downstream system or analyst consuming the repo's published outputs.
- `dataset owner` — engineer responsible for a specific `datasets/<slug>/` package.
- `repo maintainer` — operator of cross-cutting helpers, skills, governance, and release flow.
- `AI agent` — Copilot CLI / autonomous helper that needs structure to operate predictably.
- `pipeline operator` — person running scheduled refreshes / dataset updates.
- `new contributor` — first-time author who needs the workflow to be self-explanatory.

## 4. Definition of Done (DoD)

An issue is **Done** when every box below is checked. The closing skills enforce this.

- [ ] **Acceptance criteria** all checked on the issue body.
- [ ] **Tests pass** — relevant suites under `tests/`, `datasets/<slug>/tests/`, and `aggregations/<slug>/tests/` are green. **No new `skip_if_not(file.exists(...))` smells** in dataset tests (per #31).
- [ ] **Retro posted** via [`.github/skills/backlog_retrospective/SKILL.md`](.github/skills/backlog_retrospective/SKILL.md) — required for any "completed" close. Carve-out: closes labeled `duplicate` / `wontfix` / `not-planned` / `invalid` need only a 1–3 sentence rationale comment, but the **label is required before close** (`--reason` alone is not sufficient).
- [ ] **AI code review pass produced verdict CLEAN or ADVISORY** — no unaddressed BLOCKER findings from `code_review/SKILL.md` (or `_no-code-review:_` body marker present with justification).
- [ ] **Version bumps** applied wherever the change touched a versioned artifact (skill frontmatter `version`, dataset spec versions). New entry added to the changed skill's `VERSIONING.md`.
- [ ] **Related docs updated** — ROADMAP entry status (per `roadmap/SKILL.md`), this `CONTRIBUTING.md` if a process changed, [`.github/copilot-instructions.md`](.github/copilot-instructions.md) if conventions changed, [`CHANGELOG.md`](CHANGELOG.md) `[Unreleased]` block (or `no-changelog` opt-out — see §5), [`docs/decisions/`](docs/decisions/) if a new architectural decision was made.
- [ ] **Follow-on issues filed** — any work descoped or surfaced during implementation has a dedicated issue, listed in the retro's *Follow-ups* section.

## 5. How to open a PR

Always use [`.github/skills/pr_orchestrator/SKILL.md`](.github/skills/pr_orchestrator/SKILL.md). It enforces the five-section body template (Summary / Implementation / Testing / Closes / Notes) and runs **six discipline checks** before `gh pr create`:

1. **Plan-handoff gate** — [`pr_gate_plan_handoff/SKILL.md`](.github/skills/pr_gate_plan_handoff/SKILL.md) ensures every closing issue has a plan comment (or an explicit `_no-plan:_` skip) via `session_handoff/SKILL.md` `Targeted handoff`. Runs *first* among the gates, before code review, so review sees the final promoted plan state. Opt-out via `_no-handoff: <one-sentence justification>_` in the PR body's `## Notes` section.
2. **Code review gate** — [`pr_gate_code_review/SKILL.md`](.github/skills/pr_gate_code_review/SKILL.md) invokes [`code_review/SKILL.md`](.github/skills/code_review/SKILL.md) v1.0.0 with `--mode=gate`. The skill assembles a templated rubber-duck prompt (diff + issue AC + recent ADRs + repo invariants), invokes the rubber-duck `task` agent, parses findings by severity (BLOCKER / HIGH / MEDIUM / NIT), files non-trivial findings via `backlog/SKILL.md` auto-file mode, and writes a report to `.github/audit-reports/code-review_<ts>.md`. BLOCKER findings present three explicit operator choices (fix-now / file-and-stop / proceed-with-marker). No opt-out marker exists post-ADR-0006 — this gate is mandatory. Runs after plan-handoff so retros (which close issues) only commit after review passes. Merges happen on github.com — branch-protection there is the right enforcement layer for post-PR gates.
3. **Retro gate** — [`pr_gate_retro/SKILL.md`](.github/skills/pr_gate_retro/SKILL.md) invokes `backlog_retrospective/SKILL.md` once per `Closes #N` / `Fixes #N` reference *before* PR creation, so each closed issue has its retro at PR-creation time (not at GitHub auto-close time).
4. **Changelog gate** — [`pr_gate_changelog/SKILL.md`](.github/skills/pr_gate_changelog/SKILL.md) requires every PR to add at least one `[Unreleased]` entry to [`CHANGELOG.md`](CHANGELOG.md), or carry the `no-changelog` label, or include `_no-changelog: <one-sentence justification>_` in the PR body's `## Notes` section.
5. **Pre-PR QC gate** — [`pr_gate_qc/SKILL.md`](.github/skills/pr_gate_qc/SKILL.md) invokes [`run_repo_qc/SKILL.md`](.github/skills/run_repo_qc/SKILL.md) + [`documentation_audit_changes/SKILL.md`](.github/skills/documentation_audit_changes/SKILL.md) + repo-specific extensions per [`.github/copilot-instructions.md`](.github/copilot-instructions.md) "Pre-PR QC checklist". BLOCKER findings present three explicit choices to the operator (fix-now / file-and-stop / proceed-with-marker); WARNING/INFO surface as auto-filed issues but don't block. Opt-out via `_no-prep-gate: <one-sentence justification>_` in the PR body's `## Notes` section. Findings flow through `backlog/SKILL.md` v1.7.0 auto-file mode (per ADR-0004).
6. **Title convention** — conventional-commit prefixes documented in [`.github/copilot-instructions.md`](.github/copilot-instructions.md): `feat(...)`, `fix(<slug>):`, `release(<slug>):`, `docs:`, `refactor:`, `chore:`. Commits must be GPG-signed and carry the `Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>` trailer. **Commit cadence** — each commit should be a single logical change reviewable in isolation; substantial PRs (≥ 5 *meaningful* files or ≥ 200 *non-generated* LOC) should be split along natural seams. See [`.github/copilot-instructions.md`](.github/copilot-instructions.md) "Commits" block for the canonical statement and [`docs/requirements/commit-cadence-guidance.md`](docs/requirements/commit-cadence-guidance.md) for the rationale (#130). **Plan-revision traceability** — when a commit is written against an `## Implementation plan` comment (ADR-0005) that has been revised since implementation began, also add `Implements: #<issue-number>` and `Plan-Version: <plan's current _Last updated:_ timestamp>` trailers (#221, Epic O); optional when the plan hasn't changed since work started.

### Pre-PR QC gate (now wired into `pr_gate_qc/SKILL.md`)

The Pre-PR QC gate (item 5 above) is now a hard gate enforced automatically by `pr_gate_qc/SKILL.md`. See [`docs/WORKFLOW.md`](docs/WORKFLOW.md) §7a for the full lifecycle position. The skills it invokes:

1. [`run_repo_qc/SKILL.md`](.github/skills/run_repo_qc/SKILL.md) — generic repo-level QC (tests, structural invariants, lint). Reads `docs/qc_skill_modifications.md` for repo-specific checks.
2. [`documentation_audit_changes/SKILL.md`](.github/skills/documentation_audit_changes/SKILL.md) — PR-scoped doc-staleness audit.
3. **Repo-specific extensions** per [`.github/copilot-instructions.md`](.github/copilot-instructions.md) "Pre-PR QC checklist" subsection — touched-dataset QC with a `source/`-changes escalation rule.

Findings flow through `backlog/SKILL.md` v1.7.0 auto-file mode (per ADR-0004). BLOCKER findings present three explicit choices: **fix now and stop**, **file-and-stop**, or **proceed with `_no-prep-gate: <justification>_` body marker**. WARNING/INFO surface as auto-filed issues but don't block. Sub-skill output lands at `.github/audit-reports/` (gitignored), keeping the working tree clean for `gh pr create`.

### GitHub enforcement layer

In addition to the local pre-PR gates above, `main` is protected by a **github.com-side enforcement layer** that cannot be bypassed by direct pushes or external tooling:

- **Branch protection** — signed commits required, force-pushes and deletions blocked.
- **CI required check** — `.github/workflows/ci.yml` runs a skill-frontmatter validator on every PR targeting `main` (validates required fields, `layout_version == v2`, `filename:` consistency). Wired as a required status check — PRs cannot merge until it passes.
- **Closing-issue-preconditions check (ADR-0012)** — `.github/workflows/ci.yml` runs `tests/meta/validate_closing_issue_preconditions.py` on every PR: for each issue the PR declares it closes (`Closes` / `Fixes` / `Resolves` `#N`), fails unless a non-`drafting` `<!-- implementation-plan-v1 -->` plan comment exists and neither `needs-triage` nor `needs-grooming` is present, honoring the same `_no-plan:_` / `_no-handoff:_` opt-out markers as `pr_gate_plan_handoff/SKILL.md`. This is the deterministic, model-proof backstop for the agentic `start_work/SKILL.md` entry gate (§6b in `docs/WORKFLOW.md`). Not yet wired as a required status check — per ADR-0012 it must run green on `main` at least once first.
- **Bypass-actor policy** — `enforce_admins` is set to `false`, meaning repository admins can bypass branch-protection rules. This is deliberate in a solo+AI context where the single maintainer needs an escape hatch for infrastructure operations (e.g., initial CI setup, protection-rule bootstrapping). Downstream repos with multiple human contributors should set `enforce_admins: true`.
- **Repo identification** — `.pfizer.yml` provides org-standard repo ownership metadata.
- **Ruleset-as-code** — `.github/rulesets/main.json` captures the branch-protection configuration for reproducibility across downstream repos.

## 6. Labels

Single source of truth: [`.github/LABELS.md`](.github/LABELS.md). It defines the cross-repo canonical baseline (25 labels: Type / Status / Priority / Score / Meta + close-reason) and any repo-specific specializations (see the example customization section). The `backlog/SKILL.md` reads this file at session start.

Two label classes deserve special mention:

- **`epic`** — parent issues for thematic bodies of work. See §7 below.
- **Close-reason labels** (`duplicate`, `wontfix`, `not-planned`, `invalid`) — required *on the issue* before closing without a full retro. The label is the durable signal.

`good first issue` and `help wanted` are **not** used in repos that adopt this toolkit. Delete on sight.

## 6a. Backlog scoring: tier tiering (default) and WSJF (opt-in)

Per [ADR-0014](.github/vendored-decisions/ADR-0014-wsjf-opt-in-lightweight-default.md), the active scoring mechanism is resolved by [`_partials/scoring-mode.md`](.github/skills/_partials/scoring-mode.md) from `.agent.config.yml`'s `wsjf.enabled` key. Both mechanisms coexist with the `priority/*` labels the same way: `priority/*` is always the executive override.

```yaml
# .agent.config.yml (repo root)
wsjf:
  enabled: false   # optional; default false — tier/* tiering applies when absent or false
```

### Tier tiering (default)

When `wsjf.enabled` is absent or `false` — the default for every repo — issues get a direct impact/effort classification instead of a computed score:

| Label               | Meaning (impact × effort)                          |
|---------------------|-----------------------------------------------------|
| `tier/quick-win`    | Do first — high impact, low effort                   |
| `tier/big-bet`      | Schedule deliberately — high impact, high effort     |
| `tier/fill-in`      | Opportunistic — low impact, low effort               |
| `tier/reconsider`   | Justify before committing — low impact, high effort  |

No arithmetic, no boundary math to get wrong. The agent proposes one tier with a one-line rationale; the user confirms or picks a different tier. In-body block:

```markdown
<!-- TIER-START -->
## Tier: `tier/<value>` — <one-line rationale>
<!-- TIER-END -->
```

Full mechanism in [`_partials/scoring-mode.md`](.github/skills/_partials/scoring-mode.md).

### WSJF (opt-in)

Set `wsjf.enabled: true` in `.agent.config.yml` to use **Weighted Shortest Job First (WSJF)** instead — a quantitative, comparative score, unchanged in mechanism from its original design.

#### Formula

```
WSJF = (User-Business Value + Time Criticality + Risk Reduction / Opportunity Enablement) / Job Size
```

Each input on a modified Fibonacci scale: **1, 2, 3, 5, 8, 13**.

#### Bucket labels

The computed score maps to exactly one `wsjf/*` label (defined in [`.github/LABELS.md`](.github/LABELS.md) §Score):

| Label             | Score range  |
|-------------------|--------------|
| `wsjf/critical`   | ≥ 5.0        |
| `wsjf/high`       | 2.0 – 5.0    |
| `wsjf/medium`     | 0.5 – 2.0    |
| `wsjf/low`        | < 0.5        |

#### In-body block (machine-owned)

The WSJF block lives inside HTML fence markers so `backlog/SKILL.md` can parse it deterministically. Place the block near the top of the issue body, after the title's first prose section and before "Acceptance criteria":

```markdown
<!-- WSJF-START -->
## WSJF Score: <total> (`wsjf/<bucket>`)

| Component                         | Score | Rationale |
|-----------------------------------|-------|-----------|
| User-Business Value               |       |           |
| Time Criticality                  |       |           |
| Risk Reduction / Opp. Enablement  |       |           |
| **Cost of Delay (sum)**           |       |           |
| Job Size                          |       |           |
| **WSJF (CoD / Job Size)**         |       |           |
| Override                          |       |           |
<!-- WSJF-END -->
```

The `Override` row is empty by default. When a `priority/*` label is set that disagrees with the computed `wsjf/*` bucket, fill in `priority/<level> overrides wsjf/<bucket> — <reason>`. The label `priority/*` is the durable signal; the row is the auditable rationale.

#### When WSJF (opt-in mode) is required vs. optional

- **Required** for: any `enhancement`, `tech-debt`, or `epic` issue that's expected to take more than ~1 PR — same rule as tier tiering, applied to whichever mechanism is active.
- **Optional** for: tiny chores (single-line fixes, typos, label tweaks), `qc` findings (which already carry severity from the QC skill), and `audit-finding` items (auditor-supplied priority).
- **Re-scoring**: agents may re-score an issue at any time; `backlog/SKILL.md` overwrites the existing block and updates the bucket label. No history table is kept in-body — git history and GitHub's edit log are sufficient.

## 7. Roadmap, epics, and objectives

The curated, outcome-based menu of in-flight and upcoming work lives at the top of [`docs/ROADMAP.md`](docs/ROADMAP.md), under four horizons: **Now / Next / Later / Recently Shipped** (per #65). The horizon IS the status; an entry's section header tells you everything you need.

- **Objectives** (strategic intent) live in [`docs/OBJECTIVES.md`](docs/OBJECTIVES.md) — see [`objectives/SKILL.md`](.github/skills/objectives/SKILL.md). One Objective has 2–4 Key Results; epics link to a supporting Objective when one exists. The OKR rhythm is lightweight; the skill explicitly allows degrading to a freeform "Outcomes" section if it feels heavy.
- **Epics** are parent issues labeled `epic` with native GitHub sub-issues underneath. New epics enter at **Later** by default (or **Next** if explicitly committed); they move to **Now** when work begins, and to **Recently Shipped** on close. The [`epic/SKILL.md`](.github/skills/epic/SKILL.md) v2.0.0 enforces three lifecycle gates at filing time: requirements doc (soft), ADR (hard), supporting objective (prompt only).
- **Sub-issue → parent close gate.** [`epic_retrospective/SKILL.md`](.github/skills/epic_retrospective/SKILL.md) blocks closing a parent epic while any sub-issue is still open.
- **Milestones** are orthogonal to epics — used for release / timeframe grouping. Naming convention: `vX.Y.Z` (semver), tied to release artifacts in `aggregations/` and to `CHANGELOG.md` cut-release blocks.
- **Epic lifecycle skills.** [`roadmap/SKILL.md`](.github/skills/roadmap/SKILL.md), [`epic/SKILL.md`](.github/skills/epic/SKILL.md), and [`epic_retrospective/SKILL.md`](.github/skills/epic_retrospective/SKILL.md) own the lifecycle of the Epics section.

For repo-level work scope and dependency ordering on the legacy scaling backlog, see the rest of [`docs/ROADMAP.md`](docs/ROADMAP.md) (Sequencing section, below the Epics section).

## 7a. Lifecycle map (the persistent-memory layer)

> The table below is the **elevator pitch**. The full lifecycle map — with gates, cross-references, and a flow diagram — lives at [`docs/WORKFLOW.md`](docs/WORKFLOW.md), which is the canonical source of truth.

A new body of work moves through these layers, each owned by a skill:

| Layer | Question | Doc | Skill |
|---|---|---|---|
| **Strategy** | Why does this matter? | `docs/OBJECTIVES.md` | [`objectives/SKILL.md`](.github/skills/objectives/SKILL.md) |
| **Requirements** | What are we building? | `docs/requirements/<slug>.md` | [`requirements/SKILL.md`](.github/skills/requirements/SKILL.md) |
| **Decisions** | How did we decide to build it? | `.github/vendored-decisions/ADR-NNNN-*.md` (workflow) + `docs/decisions/ADR-NNNN-*.md` (local) | [`adr/SKILL.md`](.github/skills/adr/SKILL.md) |
| **Roadmap** | When are we doing it? | `docs/ROADMAP.md` (Now / Next / Later) | [`roadmap/SKILL.md`](.github/skills/roadmap/SKILL.md) |
| **Epic** | Who's working it? | GitHub issue `[Epic <id>] <theme>` | [`epic/SKILL.md`](.github/skills/epic/SKILL.md) |
| **Sub-issues** | What's the actual work? | GitHub sub-issues | [`backlog/SKILL.md`](.github/skills/backlog/SKILL.md) (heavy intake) or [`quick_capture/SKILL.md`](.github/skills/quick_capture/SKILL.md) (half-formed) |
| **Triage** | Where does this freshly-filed issue go? | `## Triage` comment + label changes | [`triage/SKILL.md`](.github/skills/triage/SKILL.md) |
| **PR** | How is it shipping? | GitHub PR (5-section body) | [`pr_orchestrator/SKILL.md`](.github/skills/pr_orchestrator/SKILL.md) |
| **Per-issue close** | What did we ship and how? | `## Retrospective` comment | [`backlog_retrospective/SKILL.md`](.github/skills/backlog_retrospective/SKILL.md) |
| **Epic close** | What theme of work landed? | `## Epic Retrospective` comment | [`epic_retrospective/SKILL.md`](.github/skills/epic_retrospective/SKILL.md) |
| **Release narrative** | What changed for users? | `CHANGELOG.md` | [`changelog/SKILL.md`](.github/skills/changelog/SKILL.md) |
| **Session resumption** | Where did we leave off? | `plan.md` + checkpoints + the layers above | [`session_start/SKILL.md`](.github/skills/session_start/SKILL.md) |
| **Daily flow** | What should I do right now? | (no doc — printed recommendation; optional `next_action_lock.json`) | [`next_action/SKILL.md`](.github/skills/next_action/SKILL.md) |
| **Post-merge pivot** | What's next, now that the PR landed? | (no doc — local-only ceremony) | [`post_merge/SKILL.md`](.github/skills/post_merge/SKILL.md) |
| **Periodic audit** | What's drifted? | `.github/audit-reports/<date>.md` (gitignored) | [`workflow_audit/SKILL.md`](.github/skills/workflow_audit/SKILL.md) |
| **Periodic grooming** | Which captured ideas are bake-able? | `## Grooming` comments + optional `.github/grooming-reports/<date>.md` | [`backlog_grooming/SKILL.md`](.github/skills/backlog_grooming/SKILL.md) |

Together these form the persistent-memory layer: any session can pick up by reading the relevant doc, without verbal re-orientation.

## 7b. Skill length budget

Cross-cutting skills under [`.github/skills/`](.github/skills/) are bounded in length to keep them readable, vendorable, and easy to amend. The budget is enforced informally (no CI gate) but tracked by the periodic audit ([`.github/audit/skills_xref.py`](.github/audit/skills_xref.py); most recent at [`docs/audits/2026-04-27-skills-consistency.md`](docs/audits/2026-04-27-skills-consistency.md)):

| Range | Status | Required action |
|---|---|---|
| ≤ 300 lines | Preferred | none |
| 301–500 lines | Warning | factor shared text into [`_partials/`](.github/skills/_partials/) where practical; otherwise accept with rationale |
| > 500 lines | **Hard violation** | open a dedicated split / refactor issue (`tech-debt`, `priority/medium` minimum); do not let the skill grow further |

Adopted in #108. See the audit report for current offender list and disposition.

## 8. Where to ask questions

Open an issue using the `feature_request.md` template (or `bug_report.md` if you've found a defect) and tag with the `question` label. **New to this repo?** Start by invoking [`onboarding/SKILL.md`](.github/skills/onboarding/SKILL.md) for a one-shot orientation through standards / lifecycle / glossary / current state — `docs/playbooks/` is named there as the recommended second read. For repo-conventions questions, check [`.github/copilot-instructions.md`](.github/copilot-instructions.md) first. To orient a fresh session, invoke [`session_start/SKILL.md`](.github/skills/session_start/SKILL.md). For the daily "what should I do right now?" answer, invoke [`next_action/SKILL.md`](.github/skills/next_action/SKILL.md) (Z4). For navigating the documentation tree itself (where does X live? where should new doc Y go?), start at [`docs/README.md`](docs/README.md) — the Diátaxis-aligned hub indexes every living doc by quadrant and cross-references the [`docs_organization/SKILL.md`](.github/skills/docs_organization/SKILL.md) that governs placement, frontmatter, and archival. The cold-load reading order for a new contributor or AI agent is [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) → [`docs/GLOSSARY.md`](docs/GLOSSARY.md) → [`docs/README.md`](docs/README.md) → [`docs/WORKFLOW.md`](docs/WORKFLOW.md), with [`docs/playbooks/`](docs/playbooks/) as worked examples once you need them.

## 9. Repo standards and vendoring

All consumer repos using this toolkit share a common workflow standard managed via [`REPO_STANDARDS.md`](REPO_STANDARDS.md). The canonical source for cross-cutting skills lives in `pfizer-evgen/agentic-dev`; vendored copies in each project repo carry version pointers back to central.

- **Provenance model:** Files are classified as `vendored-exact` (synced from central), `seed-local` (bootstrapped from template, locally owned), or `local-only` (repo-specific).
- **Vendored skills are read-only locally.** Edits go upstream to the central repo via PR, then sync downstream.
- **Local overrides** are supported: set `local_override: true` + `override_reason` in the skill's frontmatter and in `.github/VENDOR_MANIFEST.json`.
- **Operations:** Use [`bootstrap_repo/SKILL.md`](skills/bootstrap_repo/SKILL.md) to bootstrap / audit / backfill / sync.
- **Machine-readable source of truth:** `.github/VENDOR_MANIFEST.json` tracks every file's provenance, upstream version, and override status.

### Adding a new skill to this repo

When you add a new skill to `.github/skills/`, two vendoring artifacts must be updated in the same PR:

1. **Vendoring frontmatter** — the new skill's YAML frontmatter must include all 6 vendoring fields:
   ```yaml
   upstream_repo: pfizer-evgen/agentic-dev
   upstream_version: 1.0.0        # matches the skill's version field
   vendored_at: "YYYY-MM-DD"      # date of creation
   local_override: false
   override_reason: null
   provenance_mode: vendored-exact
   ```
2. **`VENDOR_MANIFEST.json`** — add an entry for the new skill with its path, provenance, and version.

Run `python3 tests/meta/validate_frontmatter.py` before opening the PR — it will catch missing frontmatter fields, missing manifest entries, and version mismatches.
