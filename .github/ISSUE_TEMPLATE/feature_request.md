---
name: Feature Request
about: New capability, enhancement, or process change
title: "[Feature] <short description>"
labels: enhancement
---

<!--
Title format: [Feature] <verb-phrase> — keep under ~80 chars.
For questions, use this template and add the `question` label in addition to `enhancement`.
File via .github/skills/backlog/SKILL.md when possible — it auto-fills a Score (tier by default, WSJF if opted in) and Parent-epic and links the sub-issue. This template is the human fallback.

Suggested labels (set via the GitHub UI sidebar, NOT in this body):
- enhancement (auto-applied)
- exactly one of priority/blocker | priority/high | priority/medium
- exactly one of tier/quick-win | tier/big-bet | tier/fill-in | tier/reconsider (default; compute below) — or, if this repo has opted into WSJF via .agent.config.yml, exactly one of wsjf/critical | wsjf/high | wsjf/medium | wsjf/low instead
- optionally: question
See .github/LABELS.md for the full vocabulary.
-->

**Parent epic:** #<epic-number>  *(or:* `standalone — <reason>`*)*

## User story

<!-- See .github/skills/_partials/user-story.md for format and the canonical role list. -->
**As a** <role>, **I want** <capability>, **so that** <outcome>.

*(or, for minor mechanical chores:* **Mechanical change** — <one-line description>. No primary user story; this is a chore enabling [linked issue / epic / standard].*)*

## Problem

What gap, friction, or missing capability does this address? Be concrete — "we don't have X, so users can't Y" is better than "we should have X."

## Proposal

The approach, not just the idea. Sketch the design at the level of: which file/module changes, what API or schema looks like, how the user invokes it.

<!-- TIER-START -->
## Tier: `tier/<value>`

<one-line rationale — impact × effort, per _partials/scoring-mode.md's 2×2>
<!-- TIER-END -->

<!--
If this repo has opted into WSJF via .agent.config.yml (wsjf.enabled: true),
replace the Tier block above with this WSJF block instead:

WSJF-START
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
WSJF-END
-->

## Acceptance criteria

- [ ] Measurable, falsifiable bullet 1.
- [ ] Bullet 2…
- [ ] Tests added or extended for new behavior (if executable).
- [ ] Docs updated where the change touches a contract: ROADMAP / CONTRIBUTING / `copilot-instructions.md` / skill versioning.

## Out of scope

- Explicit boundaries — what this issue is *not* doing, even if related.
- Adjacent improvements that should be filed separately.

## Codebase context

- Affected files / modules: `path/to/file.R`, `path/to/skill.md`.
- Related issues / PRs / docs: #NNN, [`docs/requirements/<doc>.md`](../../docs/requirements/).

## Alternatives considered

(Optional.) Approaches you weighed and rejected, with one-line rationale each. Useful for ADR-adjacent decisions.

## Definition of Ready (mini-checklist)

- [ ] Problem stated with concrete gap or friction
- [ ] Acceptance criteria measurable
- [ ] Out of scope explicit
- [ ] Labels applied (Type + Priority + Score label — `tier/*` default or `wsjf/*` if opted in)
- [ ] Parent epic linked above (or `standalone — <reason>`)

See [`../CONTRIBUTING.md`](../CONTRIBUTING.md) §3 for the full Definition of Ready.
