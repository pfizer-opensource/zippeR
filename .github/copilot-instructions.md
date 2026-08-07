<!-- template-version: 1.4.0 -->
<!-- Changelog: 1.1.0 adds Model routing section (2026-05-26); 1.2.0 strengthens
     plan/retro gate enforcement language (MUST framing) and adds a Pre-flight
     self-check checklist (2026-07-31, Epic O Phase O2); 1.3.0 documents the
     Copilot attribution footer convention in Conventions (#215, Epic O Phase O4);
     1.4.0 replaces the version-pinned model-ID ceiling/alias mapping with the
     family-level rule from ADR-0016 (#355, Epic O) -->
# Copilot Instructions — zippeR

You are working in **`pfizer-opensource/zippeR`**. Tools for Working with ZIP Codes, ZCTAs, and 3-digit ZCTAs (R package)

## What this repo is

`zippeR` is an open-source R package published on CRAN that provides tools for working with ZIP codes, ZIP Code Tabulation Areas (ZCTAs), and 3-digit ZCTAs. It offers geometry retrieval, crosswalk construction, demographic data joins, and ZIP-to-ZCTA validation utilities for researchers working with US spatial health data.

This repo is a **consumer repo using this toolkit** that vendors cross-cutting workflow skills from `pfizer-evgen/agentic-dev`. It follows the toolkit's standard for issue management, PR ceremonies, and documentation.

## Repo structure

<!-- Adapt this section to match the actual directory layout of this repo.
     The entries below are common starting points. -->

- `.github/skills/*/SKILL.md` — workflow skills (vendored from upstream + any repo-specific skills)
- `.github/skills/_partials/*.md` — composable fragments shared across skills
- `.github/vendored-decisions/` — workflow-intrinsic ADRs (vendored from upstream)
- `docs/decisions/` — repo-specific local ADRs
- `.github/CONTRIBUTING.md` — operational contract (DoR, DoD, lifecycle)
- `.github/LABELS.md` — canonical label vocabulary
- `docs/` — documentation hub (architecture, roadmap, objectives, glossary, workflow)
- `CHANGELOG.md` — user-facing release narrative

## Skills

Skills are the atomic unit of work in this repo. Each skill is a markdown file with YAML frontmatter (`filename`, `name`, `version`, `layout_version`, `triggers`, `description`) and a structured body following the v2 layout template (`## Activation`, `## Inputs`, `## Steps`, `## Outputs`, `## Success criteria`, `## Out of scope`, `## Cross-references`, `## Versioning`).

### Skill versioning

- Skills use semver: MAJOR.MINOR.PATCH.
- MAJOR: breaking changes to the skill's contract (renamed operations, removed triggers, changed output format).
- MINOR: new operations, new triggers, behavioral additions.
- PATCH: bug fixes, wording clarifications, cross-reference updates.
- Every version bump gets a CHANGELOG entry and a row in the skill's `## Versioning` table.

### Partials

Partials (`.github/skills/_partials/*.md`) are composable fragments included by reference in multiple skills. They carry a `<!-- partial-version: X.Y.Z -->` comment instead of full YAML frontmatter. Changes to a partial affect every consuming skill.

### Model routing

Model routing lives **only** at the persona-delegation layer (per upstream ADR-0011). Agent personas document their default tier in a `## Model routing` body section (not frontmatter — only `name`, `description`, and `tools` are valid agent frontmatter fields); CI checks that every `.agent.md` has exactly one such section naming a valid tier. Skills MUST NOT carry a `model:` frontmatter key — the validator rejects it. See the upstream `docs/model-assignment-matrix.md` for the persona tier table and design principles.

**Tier guidelines:**
- **Opus**: High-judgment work — architectural decisions, strategic prioritization, code review, discovery spikes
- **Sonnet**: Implementation and documentation — most lifecycle skills, pattern-following work
- **Haiku**: Mechanical bookends — quick capture, session start/handoff, post-merge cleanup

**Family-level constraint (per upstream ADR-0016, superseding ADR-0011 §4):** persona delegation is restricted to the three governed families — `opus`, `sonnet`, `haiku` — never to a model outside them. There is no standing per-point-release version ceiling; do not pin or request a specific `claude-<family>-<version>` ID in any routing directive.

**When delegating via task tool**, pass the `model` parameter by resolving the target persona's declared family to the highest currently-available concrete model ID in that family, as offered by the invoking harness at that moment:
- Tech Lead / Product Manager / Product Owner → `opus` family
- Developer / Tech Writer / others → `sonnet` family
- Mechanical sub-agents → `haiku` family

If the current model doesn't match the active persona's documented tier, consider switching before high-judgment work.

## Workflow

The lifecycle is documented in `docs/WORKFLOW.md`. The skills directory is the source of truth for *how* each step is done. Key lifecycle skills:

| Step | Skill |
|---|---|
| 0. Session start | `session_start/SKILL.md` |
| 1. Discovery | `discovery/SKILL.md` |
| 6. File issue | `backlog/SKILL.md`, `quick_capture/SKILL.md` |
| 6a. Triage | `triage/SKILL.md` |
| 7. Plan | `implementation_plan/SKILL.md` |
| 8. Implement | (developer / agent) |
| 9. Pre-PR | `pr_orchestrator/SKILL.md`, `code_review/SKILL.md` |
| 10. Post-merge | `post_merge/SKILL.md` |

## Conventions

- **Filenames**: `<name>/SKILL.md` for skills (one-subdir-per-skill per ADR-0008); `kebab-case.md` for docs under `docs/`.
- **ADRs**: `ADR-NNNN-kebab-case-title.md`. Monotonic, never reused, immutable once accepted.
- **Issues**: every issue needs at least one Type label + one Priority label per `.github/LABELS.md`.
- **Epics**: title format `[Epic <letter>] <theme>`. Epic letters are repo-scoped (A, B, C, ...).
- **Commits**: conventional commits (`feat:`, `fix:`, `docs:`, `chore:`). GPG-signed.
- **PRs**: five-section body per `.github/PULL_REQUEST_TEMPLATE.md`.
- **Attribution**: agent-created issues, PR bodies, and issue comments get a trailing Copilot co-authorship footer per `_partials/copilot-attribution.md`, complementing the git commit trailer — this is handled automatically by the shared `create_item`/`comment_item` provider operations and `pr_orchestrator/SKILL.md`; skill authors do not need to add it manually.

## What NOT to do

- Do NOT edit vendored-exact files to add repo-specific content. If a vendored file needs local customization, set `local_override: true` in this repo's `VENDOR_MANIFEST.json`.
- Do NOT skip the changelog gate on PRs that bump skill versions.
- Do NOT file ADRs for trivial decisions. ADRs are for structural choices that future sessions might re-litigate.

## Workflow enforcement

These rules are mandatory. Bypassing them defeats the lifecycle gates that maintain cross-session traceability.

1. **Always use `pr_orchestrator/SKILL.md` to create or update PRs.** Never call `gh pr create` or `gh pr edit` directly. The orchestrator enforces the plan-handoff hooks (`pr_gate_plan_handoff/SKILL.md`, `pr_gate_retro/SKILL.md`), code-review gate (`pr_gate_code_review/SKILL.md`), retrospective (`pr_gate_retro/SKILL.md`), changelog gate (`pr_gate_changelog/SKILL.md`), and Pre-PR QC gate (`pr_gate_qc/SKILL.md`). Bypassing it silently skips all of these.

2. **Always create a plan comment before implementing an issue.** Invoke `implementation_plan/SKILL.md` `Create` to post a `<!-- implementation-plan-v1 -->` comment on the issue before writing any implementation code. **Never post a plan via raw `gh issue comment` or `gh api`** — even a well-structured plan posted that way lacks the ADR-0005 locator and will be treated as "no plan found." The plan comment is the cross-session state contract per [ADR-0005](vendored-decisions/ADR-0005-issue-comment-as-plan-contract.md). Without it, `pr_gate_plan_handoff/SKILL.md` has nothing to hand off and `pr_gate_retro/SKILL.md` has nothing to transition.

3. **Always use `gh` CLI for GitHub interactions — never MCP tools.** Do not use `github-mcp-server-*` tools for any GitHub API access (issues, PRs, labels, comments, commits, search). The `gh` CLI authenticates via the user's pre-authorized token and works reliably in organizations with OAuth App access restrictions. MCP tool attempts fail silently or with opaque errors in these environments, wasting time and producing noisy logs.

4. **Always run `start_work/SKILL.md` before writing implementation code on an issue.** It verifies Type + Priority labels, absence of `needs-triage` / `needs-grooming`, and a plan comment in one pass, routing any gap to its owning skill (`triage/SKILL.md`, `backlog_grooming/SKILL.md`, `implementation_plan/SKILL.md`) instead of silently proceeding. This is the agentic (soft) half of [ADR-0012](vendored-decisions/ADR-0012-hard-vs-soft-lifecycle-gate-enforcement.md)'s layered model; the `closing-issue-preconditions` CI job is the hard backstop for the same checks at PR time.

<!-- ============================================================
     Pre-PR QC checklist (read by pr_gate_qc/SKILL.md)
     Add repo-specific QC steps below. Each line should be a
     concrete, verifiable check the agent runs before opening a PR.
     ============================================================ -->

## Pre-flight self-check

Run this short checklist mentally at the two points it names — it is the lightweight heuristic that catches the two most common lifecycle-gate misses before a human has to notice them:

- **Before starting implementation**: has `start_work/SKILL.md` run clean for this issue (Type/Priority labels, no needs-triage/needs-grooming, plan comment present)? If not, close the gap it names before writing code.
- **Before opening a PR**: have I posted a `<!-- implementation-plan-v1 -->` plan comment (via `implementation_plan/SKILL.md`, not raw `gh issue comment`) on every issue this PR will close? If not, stop and post one now.
- **Before closing an issue**: will a `## Retrospective` comment exist on it (via `backlog_retrospective/SKILL.md`, invoked through `pr_gate_retro/SKILL.md`) before the close happens? If not, the close is premature.

## Human-in-the-loop: R/ source changes

Any changes to scripts in the `R/` folder require **User Acceptance Testing (UAT)** before merging. When a PR touches files in `R/`:

1. Flag the PR explicitly for UAT in the PR body and in chat.
2. Do NOT merge or mark the task as complete until the user has confirmed acceptance.
3. Present a summary of functional changes to `R/` scripts and ask the user to verify correctness.

This applies to all modifications — new files, refactors, bug fixes, and style changes within `R/`.

## Pre-PR QC checklist

- Flag any `R/` file changes for UAT and confirm user sign-off before PR creation.
- Run `devtools::document()` if any roxygen2 blocks changed, and confirm `NAMESPACE`/`man/` are in sync.
- Run `devtools::test()` and confirm all testthat tests pass.
- Run `devtools::check()` for changes to `R/`, `DESCRIPTION`, or `NAMESPACE`; confirm 0 errors and 0 warnings.
