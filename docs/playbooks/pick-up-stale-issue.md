---
last_updated: 2026-04-27
last_updated_by: github-copilot-cli
owner_skill: playbooks/SKILL.md
quadrant: tutorials
---

# Pick up a stale issue

Worked example: an issue is open with an `in-progress` label from a prior session you don't remember. Walk it from cold pickup all the way through to PR open.

## Goal / starting state

**Goal**: reconstruct full implementation context, finish the work, ship a PR that closes the issue.

**Starting state**:

- An issue (`#<N>`) is labeled `in-progress`.
- The implementation-plan-as-comment exists on the issue per [ADR-0005](../decisions/ADR-0005-issue-comment-as-plan-contract.md).
- You may or may not have a working branch locally; the durable state is on GitHub.

## Steps

### 1. Resume cold

Invoke [`resume/SKILL.md`](../../.github/skills/resume/SKILL.md) (Z3 of the four-skill Z-series) with the issue number.

The skill reconstructs full implementation context from GitHub state: reads the plan-as-comment per ADR-0005, locates the working branch (if it exists locally or on `origin`), inspects any open draft PR, surfaces recent commits, and pulls in any referenced ADRs.

**Verify**: you can answer "what was being built? what's done? what's left?" without re-reading the issue body.

### 2. Confirm freshness

Compare the local plan mirror at `~/.copilot/session-state/<id>/plan.md` against the live remote plan comment via the `Sync token` line — per ADR-0005 §5, the mirror's `Sync token: <comment-id>:<timestamp>` is the CAS authority.

**Decision point**: **If the mirror's `Sync token` matches the live `comment.updatedAt`** → no drift; proceed. **If they diverge** → [`implementation_plan/SKILL.md`](../../.github/skills/implementation_plan/SKILL.md) (Z1) presents the three-choice prompt (use-mirror / use-remote / merge); resolve before continuing.

### 3. Verify branch state

```bash
git switch <feature-branch>
git pull --ff-only origin <feature-branch>
git status
```

**Verify**: clean worktree; up-to-date with remote; branch base is current `main` (rebase if not).

### 4. Implement what remains

Walk the plan's `### Where I left off` section. For each remaining commit:

- Make the change.
- Run targeted tests (`Rscript -e 'testthat::test_file("...")'`).
- Commit with the standard trailer (signing handled by system GPG config; `Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>`).

**Decision point**: **If the work scope expands materially** (you discover a coupled change that wasn't in the plan), update the plan via `implementation_plan/SKILL.md` (Z1) before committing — keeps the plan-as-comment authoritative.

### 5. Pre-PR gate ladder

Per the lifecycle reference in [`docs/WORKFLOW.md`](../WORKFLOW.md) step 7a:

1. **Promote the local plan mirror** — invoke [`session_handoff/SKILL.md`](../../.github/skills/session_handoff/SKILL.md) (Z2; auto-invoked by `pull_request/SKILL.md` v2.3.0+ Step 1.5 at PR-open time, but you can run it earlier explicitly). Mirror token rewrites to live `updated_at` from the API response per ADR-0005 §5 amendment.
2. **Code-review the diff** — invoke [`code_review/SKILL.md`](../../.github/skills/code_review/SKILL.md) `self-review`. Auto-files non-trivial findings via `backlog/SKILL.md` per ADR-0004.
3. **Post a retro on each closed issue** — `pull_request/SKILL.md` v2.3.0+ Step 1.5 invokes [`backlog_retrospective/SKILL.md`](../../.github/skills/backlog_retrospective/SKILL.md) once per `Closes #N` reference *before* `gh pr create`.
4. **Run repo QC** — invoke [`run_repo_qc/SKILL.md`](../../.github/skills/run_repo_qc/SKILL.md). Test baseline must match `main`; new findings BLOCK.
5. **Doc audit on touched paths** — invoke [`documentation_audit_changes/SKILL.md`](../../.github/skills/documentation_audit_changes/SKILL.md).

### 6. Open the PR

Invoke [`pull_request/SKILL.md`](../../.github/skills/pull_request/SKILL.md) (currently v2.4.0). The skill enforces the five-section body template (Summary / Implementation / Testing / Closes / Notes), runs the gate ladder above, and dispatches between `gh pr create` (new) and `gh pr edit --body-file` (update).

**Decision point**: **If an open PR already exists for this branch**, the skill auto-detects it (Step 0 dispatch) and routes to Update mode. Confirm before proceeding. **Otherwise**: Create mode runs.

### 7. Park or merge

After the PR opens, the implementation plan stays at `Status: in-progress` until the PR merges on github.com. Once merged, run [`post_merge/SKILL.md`](../../.github/skills/post_merge/SKILL.md) for the branch-reset + three-way pivot ceremony, and the `Status` advances to `shipped` (typically via the post-merge update or the next session's `resume`).

## Verification

- [ ] Plan reconstruction yielded full context without re-reading the issue body.
- [ ] Mirror-vs-remote drift was confirmed clean (or resolved via the three-choice prompt).
- [ ] Worktree was clean and branch was current at every commit boundary.
- [ ] All commits have GPG signing + the `Co-authored-by: Copilot` trailer.
- [ ] Pre-PR gate ladder ran end-to-end with no skipped steps (or skips have explicit `_no-*:_` body markers).
- [ ] PR body uses the five-section template with at least one `Closes #N` reference.

## Pointers

- **Cross-session continuity reference**: [ADR-0005](../decisions/ADR-0005-issue-comment-as-plan-contract.md) — the plan-as-comment contract and the mirror-vs-body sync-token semantics (§5 amendment).
- **Z-series overview**: see the "Cross-session continuity" callout in [`.github/copilot-instructions.md`](../../.github/copilot-instructions.md) for the four-skill loop (Z1 plan, Z2 handoff, Z3 resume, Z4 next-action).
- **Lifecycle map**: [`docs/WORKFLOW.md`](../WORKFLOW.md).

---

**Where this fits**: Tutorials quadrant of the docs hub. Curated by [`playbooks/SKILL.md`](../../.github/skills/playbooks/SKILL.md). The composition shown here exercises the full Z-series + the pre-PR gate ladder for a working-state pickup.
