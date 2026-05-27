<!-- template-version: 1.1.0 -->
<!-- Changelog: 1.1.0 adds Model routing section (2026-05-26) -->
# Copilot Instructions — zippeR

You are working in **`pfizer-opensource/zippeR`**. Tools for Working with ZIP Codes, ZCTAs, and 3-digit ZCTAs (R package)

## What this repo is

<!-- Replace this section with a 2–4 sentence description of the repo's purpose,
     primary deliverables, and relationship to the EVGen ecosystem. -->

This repo is an **EVGen consumer repo** that vendors cross-cutting workflow skills from `pfizer-evgen/rwd-agent-skills`. It follows the EVGen agent-driven development standard for issue management, PR ceremonies, and documentation.

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

Each skill declares a recommended LLM tier in its `model:` frontmatter field (`opus`, `sonnet`, `haiku`). Agent personas declare `default_model:` similarly. See the upstream `docs/model-assignment-matrix.md` for the full mapping and rationale.

**Tier guidelines:**
- **Opus**: High-judgment work — architectural decisions, strategic prioritization, code review, discovery spikes
- **Sonnet**: Implementation and documentation — most lifecycle skills, pattern-following work
- **Haiku**: Mechanical bookends — quick capture, session start/handoff, post-merge cleanup

**Hard ceiling: Opus 4.6.** Never use Opus 4.7 or higher due to billing constraints.

**When delegating via task tool**, pass the `model` parameter based on the target persona's `default_model`:
- Tech Lead / Product Owner → `claude-opus-4.5` (or `claude-opus-4.6` max)
- Developer / Tech Writer / others → `claude-sonnet-4.5`
- Mechanical sub-agents → `claude-haiku-4.5`

Each skill displays its recommended tier at the top of its body. If the current model doesn't match, consider switching before high-judgment work.

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
| 9. Pre-PR | `pull_request/SKILL.md`, `code_review/SKILL.md` |
| 10. Post-merge | `post_merge/SKILL.md` |

## Conventions

- **Filenames**: `<name>/SKILL.md` for skills (one-subdir-per-skill per ADR-0008); `kebab-case.md` for docs under `docs/`.
- **ADRs**: `ADR-NNNN-kebab-case-title.md`. Monotonic, never reused, immutable once accepted.
- **Issues**: every issue needs at least one Type label + one Priority label per `.github/LABELS.md`.
- **Epics**: title format `[Epic <letter>] <theme>`. Epic letters are repo-scoped (A, B, C, ...).
- **Commits**: conventional commits (`feat:`, `fix:`, `docs:`, `chore:`). GPG-signed.
- **PRs**: five-section body per `.github/PULL_REQUEST_TEMPLATE.md`.

## What NOT to do

- Do NOT edit vendored-exact files to add repo-specific content. If a vendored file needs local customization, set `local_override: true` in this repo's `VENDOR_MANIFEST.json`.
- Do NOT skip the changelog gate on PRs that bump skill versions.
- Do NOT file ADRs for trivial decisions. ADRs are for structural choices that future sessions might re-litigate.

## Workflow enforcement

These rules are mandatory. Bypassing them defeats the lifecycle gates that maintain cross-session traceability.

1. **Always use `pull_request/SKILL.md` to create or update PRs.** Never call `gh pr create` or `gh pr edit` directly. The PR skill enforces the plan-handoff hooks (Steps 1.5, 3.5), code-review gate (Step 2.5), retrospective (Step 3), changelog gate (Step 4), and Pre-PR QC gate (Step 4.5). Bypassing it silently skips all of these.

2. **Always create a plan comment before implementing an issue.** Invoke `implementation_plan/SKILL.md` `Create` to post a `<!-- implementation-plan-v1 -->` comment on the issue before writing any implementation code. The plan comment is the cross-session state contract per [ADR-0005](vendored-decisions/ADR-0005-issue-comment-as-plan-contract.md). Without it, `pull_request/SKILL.md` Step 1.5 has nothing to hand off and Step 3.5 has nothing to transition.

3. **Always use `gh` CLI for GitHub interactions — never MCP tools.** Do not use `github-mcp-server-*` tools for any GitHub API access (issues, PRs, labels, comments, commits, search). The `gh` CLI authenticates via the user's pre-authorized token and works reliably in organizations with OAuth App access restrictions. MCP tool attempts fail silently or with opaque errors in these environments, wasting time and producing noisy logs.

<!-- ============================================================
     Pre-PR QC checklist (read by pull_request/SKILL.md Step 4.5c)
     Add repo-specific QC steps below. Each line should be a
     concrete, verifiable check the agent runs before opening a PR.
     ============================================================ -->

## Human-in-the-loop: R/ source changes

Any changes to scripts in the `R/` folder require **User Acceptance Testing (UAT)** before merging. When a PR touches files in `R/`:

1. Flag the PR explicitly for UAT in the PR body and in chat.
2. Do NOT merge or mark the task as complete until the user has confirmed acceptance.
3. Present a summary of functional changes to `R/` scripts and ask the user to verify correctness.

This applies to all modifications — new files, refactors, bug fixes, and style changes within `R/`.

## Pre-PR QC checklist

- Flag any `R/` file changes for UAT and confirm user sign-off before PR creation.
<!-- - Run `tests/meta/validate_frontmatter.py` and confirm 0 errors. -->
<!-- - Run dataset-level QC for each touched dataset under `datasets/`. -->
<!-- - Confirm `.gitignore` patterns exclude all generated data files. -->
