#!/usr/bin/env python3
"""Meta-test: CI hard gate for closing-issue lifecycle preconditions.

Per ADR-0012 (`.github/vendored-decisions/ADR-0012-hard-vs-soft-lifecycle-gate-enforcement.md`),
lifecycle-gate enforcement needs a hard, model-proof backstop layer in
addition to the agentic entry gate (`start_work/SKILL.md`). This is that
backstop: for every issue a PR declares it closes (via a GitHub closing
keyword), verify the issue carries a non-`drafting` ADR-0005 plan-comment
locator and does not carry `needs-triage` / `needs-grooming`.

This predicate is the canonical source for
`.github/skills/_partials/plan-readiness-predicate.md`, which
`start_work/SKILL.md` and `pr_gate_plan_handoff/SKILL.md` both consume so
the agentic (soft) gates and this hard gate can't drift apart again. A
change to `check_issue()`'s rule below MUST be reflected in that partial.

Closing-issue detection mirrors `pr_gate_plan_handoff/SKILL.md` /
`pr_orchestrator/SKILL.md`'s existing "Closes #N" handling so the hard gate
and the soft gate agree on "which issues does this PR close." The two
sanctioned escapes from `pr_gate_plan_handoff/SKILL.md`'s marker vocabulary
(`_no-plan: #<N> — ...`_` and `_no-handoff: ...`_`) are honored here too.

Invocation:
    validate_closing_issue_preconditions.py --self-test
    validate_closing_issue_preconditions.py --pr <N>
    validate_closing_issue_preconditions.py --pr-body-file <path> [--issue-json <path>]

Requires the `gh` CLI for live `--pr <N>` invocation; `--pr-body-file` /
`--issue-json` allow fully offline / self-test invocation.

Stdlib-only for parsing logic — `gh`/`json` are only touched by the live
`--pr` path.
"""

import argparse
import json
import re
import subprocess
import sys

# GitHub's recognized closing keywords (case-insensitive), per GitHub docs.
CLOSING_KEYWORDS = (
    r"close[sd]?|fix(?:e[sd])?|resolve[sd]?"
)

# Matches `Closes #12`, `fixes #12, #13`, `Resolved owner/repo#12`, comma lists.
# Captures the keyword's issue-reference tail; individual #N / owner/repo#N
# tokens are extracted from that tail by ISSUE_REF_RE.
CLOSING_LINE_RE = re.compile(
    rf"\b(?:{CLOSING_KEYWORDS})\b[:\s]+((?:(?:[\w.-]+/[\w.-]+)?#\d+[,\s]*(?:and\s+)?)+)",
    re.IGNORECASE,
)
ISSUE_REF_RE = re.compile(r"(?:([\w.-]+/[\w.-]+)#(\d+))|(?:#(\d+))")

PLAN_LOCATOR = "<!-- implementation-plan-v1 -->"
NO_PLAN_MARKER_RE = re.compile(r"_no-plan:\s*#(\d+)\s*[—-]", re.IGNORECASE)
NO_HANDOFF_MARKER_RE = re.compile(r"_no-handoff:", re.IGNORECASE)
STATUS_RE = re.compile(r"_Status:_?\s*([\w-]+?)_", re.IGNORECASE)
BLOCKING_LABELS = {"needs-triage", "needs-grooming"}


def strip_noise(body):
    """Remove fenced code blocks and HTML comments so keywords inside them
    are never mistaken for a real closing reference."""
    body = re.sub(r"```.*?```", "", body, flags=re.DOTALL)
    body = re.sub(r"<!--.*?-->", "", body, flags=re.DOTALL)
    return body


def parse_closing_issues(pr_body):
    """Return a sorted list of unique same-repo issue numbers (ints) that
    `pr_body` declares it closes via a GitHub closing keyword. Cross-repo
    references (`owner/repo#N`) are recognized but excluded from the
    returned set — this gate only checks issues in the current repo."""
    body = strip_noise(pr_body)
    issues = set()
    for line_match in CLOSING_LINE_RE.finditer(body):
        tail = line_match.group(1)
        for ref_match in ISSUE_REF_RE.finditer(tail):
            cross_repo, cross_num, same_num = ref_match.groups()
            if cross_repo:
                continue  # cross-repo ref; not this repo's gate to enforce
            issues.add(int(same_num))
    return sorted(issues)


def parse_opt_out_markers(pr_body):
    """Return (no_handoff: bool, no_plan_issues: set[int])."""
    no_handoff = bool(NO_HANDOFF_MARKER_RE.search(pr_body))
    no_plan_issues = {int(m.group(1)) for m in NO_PLAN_MARKER_RE.finditer(pr_body)}
    return no_handoff, no_plan_issues


def find_plan_comment(comments):
    """Return the first comment dict whose body starts (after leading
    blank lines) with the ADR-0005 locator immediately followed by the
    `## Implementation plan` heading, or None. A locator not immediately
    followed by the heading is not a schema-conforming plan comment —
    `implementation_plan/SKILL.md`'s own schema parsing would reject it
    the same way, so this gate must not treat it as a candidate either."""
    for c in comments:
        lines = c.get("body", "").splitlines()
        non_blank = [line for line in lines if line.strip() != ""]
        if len(non_blank) >= 2 and non_blank[0].strip() == PLAN_LOCATOR and non_blank[1].strip() == "## Implementation plan":
            return c
    return None


def check_issue(issue_number, labels, comments):
    """Return a list of failure-reason strings for one issue (empty = pass)."""
    failures = []

    label_names = {l.lower() for l in labels}
    blocking = sorted(label_names & BLOCKING_LABELS)
    if blocking:
        failures.append(f"carries blocking label(s): {', '.join(blocking)}")

    plan_comment = find_plan_comment(comments)
    if plan_comment is None:
        failures.append("no comment found with the ADR-0005 locator (<!-- implementation-plan-v1 -->)")
    else:
        status_match = STATUS_RE.search(plan_comment.get("body", ""))
        if status_match is None:
            failures.append("plan comment found but no parseable _Status:_ line (malformed per ADR-0005 schema)")
        else:
            status = status_match.group(1).lower()
            if status == "drafting":
                failures.append("plan comment found but _Status:_ is 'drafting' (not yet ready to implement)")

    return failures


def run_validation(pr_body, issue_fetch):
    """Core validation entry point, decoupled from `gh` I/O so it can be
    unit-tested with synthetic fixtures.

    `issue_fetch(issue_number) -> (labels: list[str], comments: list[dict])`
    """
    closing_issues = parse_closing_issues(pr_body)
    no_handoff, no_plan_issues = parse_opt_out_markers(pr_body)

    if not closing_issues:
        return [], ["No closing-issue references found in PR body — nothing to check."]

    if no_handoff:
        return [], [f"_no-handoff:_ marker present — skipping all {len(closing_issues)} closing issue(s)."]

    errors = []
    notes = []
    for n in closing_issues:
        if n in no_plan_issues:
            notes.append(f"#{n}: skipped (_no-plan:_ marker present).")
            continue
        labels, comments = issue_fetch(n)
        failures = check_issue(n, labels, comments)
        if failures:
            errors.append(f"#{n}: {'; '.join(failures)}")
        else:
            notes.append(f"#{n}: OK.")

    return errors, notes


def _gh_issue_fetch(issue_number):
    out = subprocess.run(
        ["gh", "issue", "view", str(issue_number), "--json", "labels,comments"],
        capture_output=True, text=True, check=True,
    )
    data = json.loads(out.stdout)
    labels = [l["name"] for l in data.get("labels", [])]
    comments = data.get("comments", [])
    return labels, comments


def _run_self_tests():
    """Exercise parse_closing_issues / parse_opt_out_markers / check_issue /
    run_validation against synthetic fixtures, stdlib-only (no `gh` calls)."""
    failures = []

    def expect(label, got, want):
        if got != want:
            failures.append(f"{label}: expected {want!r}, got {got!r}")

    # --- parse_closing_issues ---
    expect("simple Closes", parse_closing_issues("Closes #12"), [12])
    expect("case-insensitive Fixes", parse_closing_issues("fixes #7"), [7])
    expect("Resolves", parse_closing_issues("Resolves #3"), [3])
    expect(
        "comma-separated list",
        parse_closing_issues("Closes #1, #2, and #3"),
        [1, 2, 3],
    )
    expect(
        "cross-repo excluded",
        parse_closing_issues("Closes owner/repo#5 and #9"),
        [9],
    )
    expect(
        "keyword inside fenced code ignored",
        parse_closing_issues("```\nCloses #99\n```\nCloses #1"),
        [1],
    )
    expect(
        "keyword inside HTML comment ignored",
        parse_closing_issues("<!-- Closes #99 -->\nCloses #1"),
        [1],
    )
    expect("no closing issues", parse_closing_issues("Just a summary, no refs."), [])
    expect("dedup", parse_closing_issues("Closes #4\nFixes #4"), [4])

    # --- parse_opt_out_markers ---
    no_handoff, no_plan = parse_opt_out_markers("_no-handoff: batch content authoring_")
    expect("no-handoff detected", no_handoff, True)
    expect("no-handoff no plan issues", no_plan, set())

    no_handoff2, no_plan2 = parse_opt_out_markers("_no-plan: #7 — spike, no code_")
    expect("no-handoff absent", no_handoff2, False)
    expect("no-plan issue parsed", no_plan2, {7})

    # --- check_issue ---
    plan_ok = [{"body": "<!-- implementation-plan-v1 -->\n## Implementation plan\n\n_Status: in-progress_"}]
    expect("clean issue passes", check_issue(1, ["enhancement", "priority/high"], plan_ok), [])

    plan_drafting = [{"body": "<!-- implementation-plan-v1 -->\n## Implementation plan\n\n_Status: drafting_"}]
    expect(
        "drafting status fails",
        len(check_issue(1, ["enhancement"], plan_drafting)) == 1,
        True,
    )

    expect(
        "missing plan fails",
        len(check_issue(1, ["enhancement"], [])) == 1,
        True,
    )

    expect(
        "needs-triage fails",
        len(check_issue(1, ["needs-triage"], plan_ok)) == 1,
        True,
    )

    locatorless = [{"body": "## Implementation plan\n\n_Status: in-progress_"}]
    expect(
        "locator-less plan-shaped comment treated as missing",
        len(check_issue(1, ["enhancement"], locatorless)) == 1,
        True,
    )

    # Regression fixture (#360): a locator with no `## Implementation plan`
    # heading immediately after it is not a schema-conforming plan comment —
    # `implementation_plan/SKILL.md`'s own schema parsing would reject it,
    # so this gate must treat it the same as "no plan found" rather than
    # matching on the locator alone.
    locator_no_heading = [{"body": "<!-- implementation-plan-v1 -->\nJust some unrelated comment text.\n\n_Status: in-progress_"}]
    expect(
        "locator without adjacent heading treated as missing",
        len(check_issue(1, ["enhancement"], locator_no_heading)) == 1,
        True,
    )

    # Regression fixture (#360): heading present but no parseable `_Status:_`
    # line — a plan comment that is missing required schema metadata must
    # fail, not silently pass, so the hard gate can't be satisfied by a
    # comment `implementation_plan/SKILL.md`'s own parsing would reject.
    heading_no_status = [{"body": "<!-- implementation-plan-v1 -->\n## Implementation plan\n\n### Approach\n\nSome approach text."}]
    expect(
        "heading present but no _Status:_ line fails",
        len(check_issue(1, ["enhancement"], heading_no_status)) == 1,
        True,
    )

    # Regression fixture: STATUS_RE must parse the real
    # implementation_plan/TEMPLATE.md status-line shape (single italic span,
    # `_Status: <value>_`, no colon-adjacent underscore). An earlier version
    # of STATUS_RE greedily captured the closing `_` into the value (yielding
    # "drafting_"/"in"), silently defeating the drafting check against real
    # plan comments while still passing self-test fixtures that didn't match
    # production formatting.
    template_shaped_drafting = [
        {"body": "<!-- implementation-plan-v1 -->\n## Implementation plan\n\n_Status: drafting_\n"}
    ]
    expect(
        "template-shaped drafting status fails",
        len(check_issue(1, ["enhancement"], template_shaped_drafting)) == 1,
        True,
    )
    template_shaped_in_progress = [
        {"body": "<!-- implementation-plan-v1 -->\n## Implementation plan\n\n_Status: in-progress_\n"}
    ]
    expect(
        "template-shaped in-progress status passes",
        check_issue(1, ["enhancement"], template_shaped_in_progress),
        [],
    )

    # --- run_validation (positive / negative / false-positive fixtures) ---
    fixtures = {
        1: (["enhancement", "priority/high"], plan_ok),
        2: (["needs-triage"], plan_ok),
        3: (["enhancement"], []),
    }

    def fetch(n):
        return fixtures[n]

    errors, notes = run_validation("Closes #1", fetch)
    expect("positive fixture: no errors", errors, [])

    errors, notes = run_validation("Closes #2", fetch)
    expect("negative fixture: needs-triage errors", len(errors), 1)

    errors, notes = run_validation("Closes #3", fetch)
    expect("negative fixture: no plan errors", len(errors), 1)

    errors, notes = run_validation("No closing issues here.", fetch)
    expect("empty closing-issue set: no errors (skip, not fail)", errors, [])

    errors, notes = run_validation("Closes #2\n\n_no-plan: #2 — exempted_", fetch)
    expect("exempted via _no-plan:_ marker", errors, [])

    errors, notes = run_validation("Closes #2\n\n_no-handoff: batch_", fetch)
    expect("exempted via _no-handoff:_ marker", errors, [])

    if failures:
        print("FAILED self-test:")
        for f in failures:
            print(f"  ✗ {f}")
        sys.exit(1)
    print(f"PASSED: {len(failures)} failure(s) — self-test fixtures covering closing-keyword parsing "
          "(Closes/Fixes/Resolves, case-insensitivity, cross-repo form, comma lists, fenced-code/HTML-comment "
          "exclusion, dedup), opt-out markers, plan-status/triage checks, and end-to-end run_validation.")
    sys.exit(0)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--pr", type=int, help="PR number to validate via `gh`")
    parser.add_argument("--pr-body-file", help="Path to a file containing the PR body (offline mode)")
    args = parser.parse_args()

    if args.self_test:
        print("═══ Closing-issue preconditions validator self-test ═══")
        _run_self_tests()
        return

    if args.pr is not None:
        out = subprocess.run(
            ["gh", "pr", "view", str(args.pr), "--json", "body"],
            capture_output=True, text=True, check=True,
        )
        pr_body = json.loads(out.stdout)["body"]
        issue_fetch = _gh_issue_fetch
    elif args.pr_body_file:
        with open(args.pr_body_file) as f:
            pr_body = f.read()
        issue_fetch = _gh_issue_fetch
    else:
        parser.error("one of --self-test, --pr, or --pr-body-file is required")
        return

    print("═══ Closing-issue lifecycle preconditions ═══")
    errors, notes = run_validation(pr_body, issue_fetch)

    for n in notes:
        print(f"  {n}")
    print("")

    if errors:
        print(f"FAILED: {len(errors)} closing issue(s) fail lifecycle preconditions:")
        for e in errors:
            print(f"  ✗ {e}")
        print("")
        print("Fix: post a non-drafting <!-- implementation-plan-v1 --> plan comment via "
              "implementation_plan/SKILL.md, remove needs-triage/needs-grooming via triage/SKILL.md "
              "or backlog_grooming/SKILL.md, or add a _no-plan: #<N> — <justification>_ / "
              "_no-handoff: <justification>_ marker to the PR body.")
        sys.exit(1)
    else:
        print("PASSED: all closing issues meet lifecycle preconditions.")
        sys.exit(0)


if __name__ == "__main__":
    main()
