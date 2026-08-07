<!--
Open this PR via .github/skills/pr_orchestrator/SKILL.md, which:
- enforces this five-section body template
- invokes backlog_retrospective/SKILL.md once per `Closes #N` / `Fixes #N`
  reference *before* `gh pr create`, so each closed issue carries its retro
  at PR-creation time
- requires GPG-signed commits and the `Co-authored-by: Copilot` trailer

Title format (conventional commits):
- feat(...)        new behavior / scope
- fix(<slug>):     bug fix in a dataset or helper
- release(<slug>): dataset release
- docs:            docs-only changes
- refactor:        no behavior change
- chore:           tooling / housekeeping
-->

## Summary

A 2–4 sentence prose summary of what this PR does and why. Past tense, declarative.

## Implementation

Bullet list of the substantive changes — files touched, contracts altered, helpers added, version bumps applied. One bullet per logically-distinct change. Mention any non-obvious design choices.

## Testing

- Existing suites that ran (`Rscript -e 'testthat::test_dir("tests")'`, dataset-specific suites under `datasets/<slug>/tests/`, aggregation suites). Note the result.
- New tests added or extended; one-line note per file.
- Docs-only PRs may write `_None — docs-only._`.

## Closes

- Closes #NNN
- Closes #MMM
- Refs #PPP  *(use `Refs` for issues this PR touches but does not close)*

## Notes

Anything else worth flagging: rollouts, follow-on issues filed, epic rollups (table form is fine), runtime side-effects (`gh label create`, milestone creation), or known-deferred work.

---

### Definition of Done

Mirrors [`CONTRIBUTING.md`](CONTRIBUTING.md) §4 — keep these in sync; surface any divergence in **Notes** above.

- [ ] **Acceptance criteria** all checked on every closed issue.
- [ ] **Tests pass** locally; no new `skip_if_not(file.exists(...))` smells.
- [ ] **AI code review pass** — `code_review/SKILL.md` returned CLEAN or ADVISORY (no unaddressed BLOCKER findings); or `_no-code-review: <justification>_` body marker present.
- [ ] **Retro posted** via `backlog_retrospective/SKILL.md` for every `Closes #N` (or close-reason label set for carve-outs).
- [ ] **Version bumps** applied wherever the change touched a versioned artifact (skill frontmatter `version`, dataset spec versions). New `Versioning` entry added to each changed skill.
- [ ] **Related docs updated** — ROADMAP entry status (per `roadmap/SKILL.md`), `CONTRIBUTING.md` if a process changed, `.github/copilot-instructions.md` if conventions changed, `CHANGELOG.md` once #68 ships.
- [ ] **Follow-on issues filed** for any descoped work surfaced during implementation, listed in each closed issue's retro under *Follow-ups*.
