---
last_updated: 2026-04-27
last_updated_by: github-copilot-cli
owner_skill: playbooks/SKILL.md
quadrant: tutorials
---

# Start a new epic

Worked example: a thematic body of work has emerged that's larger than a single issue. Walk it from "this should be an epic" all the way to a populated parent issue with sub-issues on a roadmap horizon.

## Goal / starting state

**Goal**: a parent issue labeled `epic` exists with native GitHub sub-issues underneath, the parent is referenced from [`docs/ROADMAP.md`](../ROADMAP.md) under the appropriate horizon (Now / Next / Later), and any architectural decisions made during scoping are captured as ADRs.

**Starting state**:

- A theme has been identified (e.g. "docs foundation", "release automation", "cross-repo onboarding").
- An objective in [`docs/OBJECTIVES.md`](../OBJECTIVES.md) either already exists or needs to be filed alongside the epic.
- No premature scoping — sub-issue inventory may still be loose at this point.

## Steps

### 1. Confirm or file the objective

Check [`docs/OBJECTIVES.md`](../OBJECTIVES.md) for an existing objective the epic would advance.

**Decision point**: **If a matching objective exists** with key results the new epic would directly contribute to → note its identifier; you'll cite it in the epic body. **Otherwise** → invoke [`objectives/SKILL.md`](../../.github/skills/objectives/SKILL.md) to file a new objective + key results before continuing. The epic skill will gate on this in step 3.

### 2. File any architectural ADRs surfaced by scoping

If scoping the epic surfaced an architectural decision (a constraint that will bind future work; a "we won't do X" stance; a contract between components), capture it now via [`adr/SKILL.md`](../../.github/skills/adr/SKILL.md).

ADRs land in `docs/decisions/ADR-NNNN-<slug>.md` per the convention documented in [`docs/decisions/README.md`](../decisions/README.md). The epic skill enforces this as a hard gate at step 3 (it asks "any architectural decisions surfaced? — file them now or assert none").

### 3. Create the epic

Invoke [`epic/SKILL.md`](../../.github/skills/epic/SKILL.md) (currently v2.1.0) with the proposed title.

The skill enforces three lifecycle gates before composing the epic body:

1. **Requirements (soft gate)**: prompts whether a [`docs/requirements/<slug>.md`](../requirements/) doc exists or should be filed via [`requirements/SKILL.md`](../../.github/skills/requirements/SKILL.md). Soft — can be deferred for small epics.
2. **ADR (hard gate)**: refuses to proceed unless the operator either points at the relevant ADR(s) or explicitly asserts "no architectural decision surfaced." This is the gate that step 2 above feeds.
3. **Objective (prompt)**: prompts to either cite an existing objective + KR or invoke `objectives/SKILL.md` inline. This is the gate that step 1 above feeds.

After gates clear, the skill composes the epic body (Goal / Why now / Acceptance criteria / Sub-issues / Decisions and constraints / Linked objective / Priority) and files it labeled `epic`.

### 4. File sub-issues and link them natively

For each sub-issue identified during scoping:

- Invoke [`backlog/SKILL.md`](../../.github/skills/backlog/SKILL.md) (currently v2.3.0+) to file with appropriate Type + Priority labels and a User story.
- Link as a native GitHub sub-issue of the epic via the GitHub UI or API (`gh api -X POST /repos/.../issues/<epic>/sub_issues`). Native sub-issue linkage is what `epic_retrospective/SKILL.md` checks at close time — body-text references are insufficient.

**Decision point**: **If a sub-issue is half-formed** at this point, file it via [`quick_capture/SKILL.md`](../../.github/skills/quick_capture/SKILL.md) with `needs-grooming` instead — [`backlog_grooming/SKILL.md`](../../.github/skills/backlog_grooming/SKILL.md) will promote it to `needs-triage` later, and [`triage/SKILL.md`](../../.github/skills/triage/SKILL.md) will route it from there.

### 5. Place the epic on a horizon

Invoke [`roadmap/SKILL.md`](../../.github/skills/roadmap/SKILL.md) to add the epic under the appropriate horizon section in [`docs/ROADMAP.md`](../ROADMAP.md):

- **Now** — work begins immediately (or is already in flight).
- **Next** — explicitly committed; not started yet.
- **Later** — backlog (default for new epics unless explicitly committed).

Per [ADR-0003](../decisions/ADR-0003-now-next-later-roadmap-horizons.md), the horizon IS the status; do NOT add a `(status: ...)` suffix.

**Verify**: the epic appears in `docs/ROADMAP.md` under the chosen horizon section; the line links to the parent issue and (optionally) lists sub-issues inline.

### 6. Optional: cite the epic in CHANGELOG

If the epic is being promoted to Now (active work begins), file a `## Strategic context` line in `CHANGELOG.md` `[Unreleased]` per the precedent set by Epic F PR-1 — captures the user-facing narrative of "we're starting on X."

## Verification

- [ ] Parent issue exists labeled `epic` with the §5.3 / `epic/SKILL.md` body template populated.
- [ ] All three epic-skill gates passed (requirements / ADR / objective).
- [ ] Each sub-issue is a native GitHub sub-issue of the parent (verifiable via `gh api /repos/.../issues/<epic>/sub_issues`).
- [ ] [`docs/ROADMAP.md`](../ROADMAP.md) lists the epic under exactly one horizon section.
- [ ] Any architectural decisions surfaced during scoping have an ADR filed.
- [ ] If the epic advances a new objective, that objective is filed in [`docs/OBJECTIVES.md`](../OBJECTIVES.md) with KRs.

## Pointers

- **Roadmap horizons reference**: [ADR-0003](../decisions/ADR-0003-now-next-later-roadmap-horizons.md).
- **Epic lifecycle close**: [`epic_retrospective/SKILL.md`](../../.github/skills/epic_retrospective/SKILL.md) — refuses to close while any open sub-issue remains.
- **Persistent-memory layer overview**: see the "Roadmap, epics, and the persistent-memory layer" section in [`.github/copilot-instructions.md`](../../.github/copilot-instructions.md).
- **Lifecycle map**: [`docs/WORKFLOW.md`](../WORKFLOW.md).

---

**Where this fits**: Tutorials quadrant of the docs hub. Curated by [`playbooks/SKILL.md`](../../.github/skills/playbooks/SKILL.md). The composition shown here exercises the persistent-memory trio (`requirements/SKILL.md` / `adr/SKILL.md` / `objectives/SKILL.md`) gated by `epic/SKILL.md` v2.1.0, then `roadmap/SKILL.md` for horizon placement.
