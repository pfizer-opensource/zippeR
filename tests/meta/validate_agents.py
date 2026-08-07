#!/usr/bin/env python3
"""Meta-test for agent persona Model routing sections.

Per ADR-0011 §2, persona-delegation tiers are the sole model-routing
surface, and the section that names the tier is CI-checked so it cannot
silently go missing, duplicate, or drift to an invalid tier.

Validates that every `.github/agents/*.agent.md` file has exactly one
`## Model routing` heading, and that the section names exactly one valid
tier (`opus`, `sonnet`, or `haiku`), matched via the `**<tier>** tier`
convention used across all agent files.

Stdlib-only — no external dependencies.
"""

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
AGENTS_DIR = REPO_ROOT / ".github" / "agents"

VALID_TIERS = {"opus", "sonnet", "haiku"}

HEADING_RE = re.compile(r"^## Model routing\s*$", re.MULTILINE)
# Matches the "**<tier>** tier" convention, e.g. "the **sonnet** tier"
TIER_RE = re.compile(r"\*\*(\w+)\*\*\s+tier")

errors = []


def error(msg):
    errors.append(msg)
    print(f"  FAIL: {msg}")


def section_body(content, start):
    """Return the text of the section starting at `start` up to the next `## ` heading."""
    next_heading = re.search(r"^## ", content[start:], re.MULTILINE)
    end = start + next_heading.start() if next_heading else len(content)
    return content[start:end]


def validate_agent(path):
    try:
        rel_name = str(path.relative_to(REPO_ROOT))
    except ValueError:
        # Fixture path outside the repo (used by --self-test); fall back to the bare filename.
        rel_name = path.name
    content = path.read_text()

    headings = list(HEADING_RE.finditer(content))
    if not headings:
        error(f"{rel_name}: missing '## Model routing' section")
        return
    if len(headings) > 1:
        error(f"{rel_name}: found {len(headings)} '## Model routing' sections, expected exactly 1")
        return

    body = section_body(content, headings[0].end())
    tiers_found = TIER_RE.findall(body)

    if not tiers_found:
        error(f"{rel_name}: '## Model routing' section does not name a tier (expected '**<tier>** tier')")
        return
    if len(tiers_found) > 1:
        error(f"{rel_name}: '## Model routing' section names multiple tiers: {tiers_found}")
        return

    tier = tiers_found[0]
    if tier not in VALID_TIERS:
        error(f"{rel_name}: tier '{tier}' is not valid, expected one of: {sorted(VALID_TIERS)}")


def _run_self_tests():
    """Exercise validate_agent() against synthetic fixtures covering each
    failure mode, so the regex/section-boundary logic has regression
    coverage independent of the live .github/agents/ files. Stdlib-only
    (tempfile), no external test framework."""
    import tempfile

    cases = [
        ("missing_section", "# Agent\n\nNo routing section here.\n", 1),
        (
            "duplicate_section",
            "## Model routing\nThis persona operates at the **sonnet** tier.\n\n"
            "## Model routing\nThis persona operates at the **haiku** tier.\n",
            1,
        ),
        ("invalid_tier", "## Model routing\nThis persona operates at the **turbo** tier.\n", 1),
        ("no_tier_named", "## Model routing\nNo tier mentioned in this section.\n", 1),
        (
            "valid_single_section",
            "## Model routing\nThis persona operates at the **opus** tier.\n\n## Next section\nUnrelated.\n",
            0,
        ),
        (
            "tier_not_leaked_from_next_section",
            "## Model routing\nThis persona operates at the **sonnet** tier.\n\n"
            "## Next section\nSome other persona uses the **haiku** tier.\n",
            0,
        ),
    ]

    failures = []
    with tempfile.TemporaryDirectory() as tmp:
        for name, content, expected_errors in cases:
            path = Path(tmp) / f"{name}.agent.md"
            path.write_text(content)

            errors.clear()
            validate_agent(path)
            got_errors = len(errors)

            if got_errors != expected_errors:
                failures.append(
                    f"{name}: expected {expected_errors} error(s), got {got_errors} ({errors})"
                )

    errors.clear()

    if failures:
        print("FAILED self-test:")
        for f in failures:
            print(f"  ✗ {f}")
        sys.exit(1)
    else:
        print(f"PASSED: {len(cases)} self-test fixture(s) covering missing/duplicate section, "
              "invalid/absent tier, and section-boundary containment")
        sys.exit(0)


def main():
    if "--self-test" in sys.argv:
        print("═══ Agent Model routing validator self-test ═══")
        _run_self_tests()
        return

    print("═══ Agent Model routing validation ═══")
    agent_files = sorted(AGENTS_DIR.glob("*.agent.md"))

    if not agent_files:
        error("No .agent.md files found in .github/agents/")
    else:
        print(f"  Found {len(agent_files)} agent files")
        for path in agent_files:
            validate_agent(path)

    print("")
    if errors:
        print(f"FAILED: {len(errors)} error(s) found")
        for e in errors:
            print(f"  ✗ {e}")
        sys.exit(1)
    else:
        print("PASSED: all agent Model routing sections valid")
        sys.exit(0)


if __name__ == "__main__":
    main()
