---
last_updated: 2026-04-27
last_updated_by: github-copilot-cli
owner_skill: playbooks/SKILL.md
quadrant: tutorials
---

# `docs/playbooks/` — worked-example traces

Step-by-step traces of common end-to-end operations. Playbooks are the **Diátaxis "tutorial / worked-example" axis** of the docs taxonomy: concrete, end-to-end, learning-oriented. Pattern-match against a real trace instead of inferring composition from skill catalogs and reference docs.

Playbooks complement reference docs ([`docs/WORKFLOW.md`](../WORKFLOW.md), the skill catalog under [`.github/skills/`](../../.github/skills/), [`docs/ARCHITECTURE.md`](../ARCHITECTURE.md)) — they never replace them. When a playbook references a skill or helper, the audit op of [`playbooks/SKILL.md`](../../.github/skills/playbooks/SKILL.md) verifies the reference resolves.

## When to use a playbook

- You're doing a common operation for the **first time** and want a worked example to follow.
- You're an AI agent picking up the repo and need the **composition** of skills (which goes first; what's the decision tree) — not just the per-skill capability surface.
- A reviewer wants to confirm an operation followed the canonical sequence.

For one-off or rare operations, a session checkpoint is the right artifact, not a playbook.

## Catalog

| Slug | Title | Last updated |
|---|---|---|
| [`add-new-dataset.md`](./add-new-dataset.md) | Add a new dataset from slug to first release artifact | 2026-04-27 |
| [`pick-up-stale-issue.md`](./pick-up-stale-issue.md) | Resume a stalled issue and ship it through PR | 2026-04-27 |
| [`start-new-epic.md`](./start-new-epic.md) | Promote an objective into an epic with sub-issues on a roadmap horizon | 2026-04-27 |

Additional playbooks (`release-dataset-update`, `bootstrap-new-repo`, `file-an-ADR`) named in [#88](https://github.com/pfizer-evgen/evgen-open-data-pipelines/issues/88) ship as needed via [`playbooks/SKILL.md`](../../.github/skills/playbooks/SKILL.md)'s `author` operation.

## Authoring

The authoring conventions (voice, step format, decision-point notation, length target, naming) are documented in [`playbooks/SKILL.md`](../../.github/skills/playbooks/SKILL.md) under "Authoring conventions" — single source of truth. To add a new playbook, invoke that skill's `author` operation; it walks through skeleton population and validates the result via `audit` before declaring success.

## Maintenance

Playbooks are **living traces** — they reference real skills, real commands, real file paths. When a referenced skill renames or restructures, [`playbooks/SKILL.md`](../../.github/skills/playbooks/SKILL.md) `audit` surfaces the impact (BLOCKER on broken `<name>_skill.md` references; BLOCKER on broken relative paths). The Tutorials quadrant stale threshold is 180 days per [`docs/README.md`](../README.md).

## See also

- [`onboarding/SKILL.md`](../../.github/skills/onboarding/SKILL.md) names playbooks as a "second read" after orientation.
- [`docs/WORKFLOW.md`](../WORKFLOW.md) — the canonical 12-step lifecycle reference. Playbooks instantiate slices of that lifecycle on concrete topics.
- [`docs/ARCHITECTURE.md`](../ARCHITECTURE.md) — what each major component does. Playbooks show how the components compose for specific outcomes.
- [`docs/GLOSSARY.md`](../GLOSSARY.md) — terminology used throughout the playbooks.

---

**Where this fits**: Tutorials quadrant of the docs hub ([`docs/README.md`](../README.md)). Catalog is curated by [`playbooks/SKILL.md`](../../.github/skills/playbooks/SKILL.md). 180-day stale threshold per the Tutorials quadrant default.
