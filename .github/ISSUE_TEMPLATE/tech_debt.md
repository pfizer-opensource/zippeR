---
name: Tech Debt
about: Refactors, scaling cracks, paydown work, internal-quality improvements
title: "[Tech Debt] <short description>"
labels: tech-debt
---

<!--
Title format: [Tech Debt] <verb-phrase> — keep under ~80 chars.
File via .github/skills/backlog/SKILL.md when possible — it auto-fills WSJF and Parent-epic and links the sub-issue. This template is the human fallback.

Suggested labels (set via the GitHub UI sidebar, NOT in this body):
- tech-debt (auto-applied)
- exactly one of priority/blocker | priority/high | priority/medium
- exactly one of wsjf/critical | wsjf/high | wsjf/medium | wsjf/low (compute below)
See .github/LABELS.md for the full vocabulary.
-->

**Parent epic:** #<epic-number>  *(or:* `standalone — <reason>`*)*

## User story

<!-- See .github/skills/_partials/user-story.md for format and the canonical role list. -->
**As a** <role>, **I want** <capability>, **so that** <outcome>.

*(or, for minor mechanical chores:* **Mechanical change** — <one-line description>. No primary user story; this is a chore enabling [linked issue / epic / standard].*)*

## Motivation

Why this debt matters now. The audience is a future maintainer — explain the cost of leaving it.

## Current pain

The concrete symptom: brittle helper, repeated workarounds, slow tests, unclear contract. Reference file paths and (where helpful) the git commits or PRs that introduced the smell.

## Proposal

The refactor, not just "clean it up." Describe the target shape: helper signatures, file layout, contract changes. Note any breaking impact on callers.

<!-- WSJF-START -->
## WSJF Score: <total> (`wsjf/<bucket>`)

| Component                         | Score | Rationale |
|-----------------------------------|-------|-----------|
| User-Business Value               |       |           |
| Time Criticality                  |       |           |
| Risk Reduction / Opp. Enablement  |       |           |
| **Cost of Delay (sum)**           |       | — |
| Job Size                          |       |           |
| **WSJF (CoD / Job Size)**         |       | — |
| Override                          |       |           |
<!-- WSJF-END -->

## Acceptance criteria

- [ ] Target shape implemented as described.
- [ ] All callers migrated; no transitional shims left behind unless explicitly scoped.
- [ ] Tests still pass; new tests added if the refactor exposes a previously-untested path.
- [ ] No new test skips (`skip_if_not(file.exists(...))` and similar smells per #31).
- [ ] Docs updated where the change touches a contract.

## Out of scope

- Adjacent debt that shares the same code path but isn't part of this paydown.
- Scope creep into "while we're here" rewrites.

## Codebase context

- Affected files / functions: `path/to/file.R`, `helper_name()`.
- Related issues / PRs: #NNN, #MMM.
- Originating spec / discussion (if any): [`docs/requirements/<doc>.md`](../../docs/requirements/).

## Definition of Ready (mini-checklist)

- [ ] Motivation + current pain are concrete
- [ ] Acceptance criteria measurable
- [ ] Out of scope explicit
- [ ] Labels applied (Type + Priority + WSJF bucket)
- [ ] Parent epic linked above (or `standalone — <reason>`)

See [`../CONTRIBUTING.md`](../CONTRIBUTING.md) §3 for the full Definition of Ready.
