---
last_updated: 2026-05-31
last_updated_by: github-copilot-cli
owner_skill: adr/SKILL.md
quadrant: history
---

# Architecture Decision Records — Repo-Specific

This directory holds **repo-specific local ADRs** — architectural decisions that apply only to this repo. Numbering is independent and starts from `ADR-0001`.

For **workflow-intrinsic ADRs** (vendored from `pfizer-evgen/agentic-dev`), see [`.github/vendored-decisions/README.md`](../../.github/vendored-decisions/README.md). The directory separation is documented in [ADR-0007](../../.github/vendored-decisions/ADR-0007-directory-separated-adr-vendoring.md).

## Conventions

- Filenames: `ADR-NNNN-kebab-case-title.md`. Numbering is monotonic within this directory and never reused.
- ADRs are **immutable once accepted**. Changes happen via a new ADR that supersedes the old one (status updated; cross-links).
- Template: [`_template.md`](./_template.md).
- Filing / superseding / auditing / re-indexing: [`adr/SKILL.md`](../../.github/skills/adr/SKILL.md).

## Index

| ADR | Title | Status | Date |
|---|---|---|---|
| [0001](./ADR-0001-agent-persona-model.md) | Five-persona agent model using native `.agent.md` format | accepted | 2026-04-30 |
| [0002](./ADR-0002-persona-model-growth-criteria.md) | Persona-model growth criteria (6th+ persona justification) | accepted | 2026-04-30 |
| [0003](./ADR-0003-pr-orchestrator-gate-contract.md) | PR skill orchestrator↔gate contract | accepted | 2026-05-20 |
| [0004](./ADR-0004-uds-crosswalk-internalization.md) | Bundle UDS crosswalk data in inst/extdata rather than downloading at runtime | accepted | 2026-05-31 |

---

## Where this fits

- **Quadrant**: history
- **See also**: [`.github/vendored-decisions/README.md`](../../.github/vendored-decisions/README.md) (workflow-intrinsic ADRs), [`../README.md`](../README.md), [`../requirements/README.md`](../requirements/README.md), [`../archive/README.md`](../archive/README.md)
- **Reference skills**: [`adr/SKILL.md`](../../.github/skills/adr/SKILL.md)
