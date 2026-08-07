---
name: Bug Report
about: Broken functionality, incorrect behavior, or regressions in the pipeline
title: "[Bug] <short description>"
labels: bug
---

<!--
Title format: [Bug] <verb-phrase> — keep under ~80 chars.
File via .github/skills/backlog/SKILL.md when possible — it auto-fills a Score (tier by default, WSJF if opted in) and Parent-epic and links the sub-issue. This template is the human fallback.

Suggested labels (set via the GitHub UI sidebar, NOT in this body):
- bug (auto-applied)
- exactly one of priority/blocker | priority/high | priority/medium
- exactly one of tier/quick-win | tier/big-bet | tier/fill-in | tier/reconsider (default; compute below) — or, if this repo has opted into WSJF via .agent.config.yml, exactly one of wsjf/critical | wsjf/high | wsjf/medium | wsjf/low instead
See .github/LABELS.md for the full vocabulary.
-->

**Parent epic:** #<epic-number>  *(or:* `standalone — <reason>`*)*

## User story

<!-- See .github/skills/_partials/user-story.md for format and the canonical role list. -->
**As a** <role>, **I want** <capability>, **so that** <outcome>.

*(or, for minor mechanical chores:* **Mechanical change** — <one-line description>. No primary user story; this is a chore enabling [linked issue / epic / standard].*)*

## Problem

A concrete, falsifiable description of what is broken. Past tense. Reference symbols / file paths in `backticks`.

## Expected vs. actual

- **Expected:** what should happen.
- **Actual:** what does happen.

## Reprex / repro steps

The minimal sequence (commands, data, R session) that surfaces the bug. Prefer a `reprex::reprex()` block when the bug is in R code; for pipeline / shell-level bugs, a literal command sequence is fine.

```r
# inset reprex here
```

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

- [ ] Bug reproduces on `main` at the named commit / version.
- [ ] Root cause identified and described in the retro.
- [ ] Fix lands with a regression test that fails on `main` and passes on the fix branch.
- [ ] Other criteria specific to this bug…

## Out of scope

- Adjacent issues that share a code path but aren't part of this fix.
- Refactors not strictly needed to land the fix.

## Codebase context

- Affected files / functions: `path/to/file.R`, `helper_name()`.
- Related issues / PRs: #NNN, #MMM.

## Definition of Ready (mini-checklist)

- [ ] Problem stated with concrete failure mode
- [ ] Acceptance criteria measurable
- [ ] Out of scope explicit
- [ ] Labels applied (Type + Priority + Score label — `tier/*` default or `wsjf/*` if opted in)
- [ ] Parent epic linked above (or `standalone — <reason>`)

See [`../CONTRIBUTING.md`](../CONTRIBUTING.md) §3 for the full Definition of Ready.
