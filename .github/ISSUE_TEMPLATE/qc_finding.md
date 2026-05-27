---
name: QC Finding
about: A QC finding — typically auto-filed by `run_dataset_qc/SKILL.md`
title: "[QC] <dataset>: <short description>"
labels: qc
---

<!--
Title format: [QC] <slug>: <short description>.
This template is primarily auto-populated by .github/skills/run_dataset_qc/SKILL.md
when a QC pass surfaces a finding. Manual filing is appropriate when a human
reviewer spots something the automation missed.

Suggested labels (set via the GitHub UI sidebar):
- qc (auto-applied)
- exactly one of priority/blocker | priority/high | priority/medium
  (severity below maps to priority: CRITICAL -> priority/blocker, HIGH ->
   priority/high, MEDIUM/LOW -> priority/medium)
- WSJF is OPTIONAL on QC findings — severity is the dominant signal.
See .github/LABELS.md for the full vocabulary.
-->

**Parent epic:** #<epic-number>  *(or:* `standalone — <reason>`*)*

## User story

<!-- See .github/skills/_partials/user-story.md for format and the canonical role list. -->
**As a** <role>, **I want** <capability>, **so that** <outcome>.

*(or, for minor mechanical chores:* **Mechanical change** — <one-line description>. No primary user story; this is a chore enabling [linked issue / epic / standard].*)*

**Severity:** CRITICAL | HIGH | MEDIUM | LOW

**Dataset:** `<slug>`
**File:** `path/to/file`
**Line(s):** <line numbers>

## Description

A clear description of the finding — what is broken, incorrect, or at risk. State whether the finding affects published artifacts (in `aggregations/<slug>/data/`) or only build-time outputs (in `datasets/<slug>/data/`).

## Root cause

Why the issue exists (if determined). If unknown at file time, write "unknown — to be investigated" and let the implementer fill it in via the retro.

## Test that exposed it

Test name and file, if applicable (e.g., `tests/test-helper.R::test_that('helper handles X')`). Indicate whether the test is in the repo today or needs to be added.

## Suggested fix

Description of the recommended code change, if applicable. Skip if the fix path is unclear at file time.

## QC report

Link to the QC report in [`docs/qc-reports/`](../../docs/qc-reports/) that documents this finding (auto-populated by `run_dataset_qc/SKILL.md`; for manually-filed findings, attach the QC log if relevant).

## Acceptance criteria

- [ ] Root cause confirmed.
- [ ] Fix lands and the QC test that exposed the finding now passes.
- [ ] Regression test added so the same failure mode cannot reappear silently.
- [ ] If the finding affected published artifacts, a re-release issue is filed (or the next release is gated on this fix).

## Definition of Ready (mini-checklist)

- [ ] Severity, dataset slug, and file/line populated
- [ ] Acceptance criteria measurable
- [ ] Out of scope (implicit: only this finding) is acceptable for QC; explicit scope-list optional
- [ ] Labels applied (`qc` + Priority)
- [ ] Parent epic linked above (or `standalone — <reason>` for cross-cutting findings)

See [`../CONTRIBUTING.md`](../CONTRIBUTING.md) §3 for the full Definition of Ready.
