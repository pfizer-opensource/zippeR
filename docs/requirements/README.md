---
last_updated: 2026-04-27
last_updated_by: github-copilot-cli
owner_skill: requirements/SKILL.md
quadrant: intent
---

# `docs/requirements/` — body-of-work requirements docs

Scoping documents for thematic bodies of work, typically one per epic or large feature. Each doc captures background, sources, and numbered testable requirements with acceptance criteria.

> **Distinct from per-dataset specs.** The per-dataset spec template lives at [`./_dataset-template.md`](./_dataset-template.md) and is copied by `source/create_dataset_skeleton.R` into each new `datasets/<slug>/`. This subfolder is for *repo-level* / *epic-level* scoping, not per-dataset specs.

Requirements docs are governed by [`requirements/SKILL.md`](../../.github/skills/requirements/SKILL.md). The skill's `file` operation creates a new doc from `_template.md`; the `audit` operation surfaces drift.

## Conventions

- **Filenames**: `kebab-case.md` matching the body of work (e.g. `repo-scaling.md`, `docs-foundation.md`).
- **Template**: [`_template.md`](./_template.md). The leading `_` keeps it sorting at the top.
- **Frontmatter**: each requirements doc carries the standard `last_updated` frontmatter (see the hub [`../README.md`](../README.md) for the convention). Stale-threshold is per-doc, tied to the lifecycle of the epic that owns it.
- **Supersession**: when a requirements doc is replaced by a newer scope or absorbed into an in-tree implementation, archive it under [`../archive/<year>/requirements/<slug>.md`](../archive/) via [`docs_organization/SKILL.md`](../../.github/skills/docs_organization/SKILL.md).

## Active requirements

| Doc | Body of work | Owner epic |
|---|---|---|
| [`repo-scaling.md`](./repo-scaling.md) | v2 repo layout & data-package conventions | (pre-Epic-A umbrella spec; long-lived) |
| [`commit-cadence-guidance.md`](./commit-cadence-guidance.md) | Commit-granularity guidance — Phase 1 Discovery output (#130) | #123 (Skills consistency & cleanup) |

## Template

- [`_template.md`](./_template.md) — body-of-work / epic-level requirements template.

---

## Where this fits

- **Quadrant**: intent
- **See also**: [`../README.md`](../README.md), [`../decisions/README.md`](../decisions/README.md), [`../archive/README.md`](../archive/README.md)
- **Reference skills**: [`requirements/SKILL.md`](../../.github/skills/requirements/SKILL.md), [`docs_organization/SKILL.md`](../../.github/skills/docs_organization/SKILL.md)
