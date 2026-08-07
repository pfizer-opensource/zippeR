#!/usr/bin/env python3
"""Meta-tests for skill frontmatter and VENDOR_MANIFEST.json.

Validates that:
  A. Every skill has correct YAML frontmatter (base + vendoring fields)
  B. Every partial has a partial-version comment
  C. VENDOR_MANIFEST.json is valid and internally consistent
  D. Canonical clean-path: this repo would self-audit clean

Stdlib-only — no external dependencies.
"""

import json
import os
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
SKILLS_DIR = REPO_ROOT / ".github" / "skills"
PARTIALS_DIR = SKILLS_DIR / "_partials"
MANIFEST_PATH = REPO_ROOT / ".github" / "VENDOR_MANIFEST.json"

REQUIRED_BASE_FIELDS = {"filename", "name", "version", "layout_version", "triggers", "description"}
REQUIRED_VENDORING_FIELDS = {"upstream_repo", "upstream_version", "vendored_at", "provenance_mode"}
VALID_PROVENANCE_MODES = {"vendored-exact", "seed-local", "repo-local", "local-only", "upstream-only"}
FORBIDDEN_BASE_FIELDS = {"model"}  # ADR-0011: model-tier routing lives in the persona layer only
SEMVER_RE = re.compile(r"^\d+\.\d+\.\d+$")

errors = []
warnings = []


def error(msg):
    errors.append(msg)
    print(f"  FAIL: {msg}")


def warn(msg):
    warnings.append(msg)
    print(f"  WARN: {msg}")


def parse_frontmatter(path):
    """Extract YAML frontmatter as a dict from a markdown file."""
    with open(path) as f:
        content = f.read()
    if not content.startswith("---"):
        return None, content
    parts = content.split("---", 2)
    if len(parts) < 3:
        return None, content
    fm_text = parts[1]
    fm = {}
    current_key = None
    current_value_lines = []

    for line in fm_text.strip().split("\n"):
        # Simple YAML key: value parsing (handles multiline with >)
        kv = re.match(r"^(\w[\w_-]*)\s*:\s*(.*)", line)
        if kv:
            if current_key:
                fm[current_key] = "\n".join(current_value_lines).strip()
            current_key = kv.group(1)
            current_value_lines = [kv.group(2)]
        elif current_key:
            current_value_lines.append(line)

    if current_key:
        fm[current_key] = "\n".join(current_value_lines).strip()

    return fm, parts[2]


# ── A. Skill frontmatter validation ──────────────────────────────────────

def test_skills():
    print("\n═══ A. Skill frontmatter validation ═══")
    skill_files = sorted(
        path for path in SKILLS_DIR.glob("*/SKILL.md")
        if str(path.relative_to(REPO_ROOT)) != ".github/skills/_partials/SKILL.md"
    )
    if not skill_files:
        error("No skill files found in .github/skills/")
        return {}, {}

    print(f"  Found {len(skill_files)} skill files")
    skill_versions = {}
    skill_vendoring = {}

    for path in skill_files:
        rel_name = str(path.relative_to(SKILLS_DIR))  # e.g. "backlog/SKILL.md"
        fm, _ = parse_frontmatter(path)

        if fm is None:
            error(f"{rel_name}: no YAML frontmatter found")
            continue

        # Required base fields
        for field in REQUIRED_BASE_FIELDS:
            if field not in fm:
                error(f"{rel_name}: missing required field '{field}'")

        # Forbidden base fields (ADR-0011: no per-skill model: frontmatter)
        for field in FORBIDDEN_BASE_FIELDS:
            if field in fm:
                error(f"{rel_name}: forbidden field '{field}' (model routing is persona-level only, per ADR-0011)")

        # Required vendoring fields (only for vendored skills; repo-local uses minimal frontmatter)
        pm = fm.get("provenance_mode", "")
        if pm == "repo-local":
            # Repo-local skills use minimal frontmatter per ADR-0009
            pass
        else:
            for field in REQUIRED_VENDORING_FIELDS:
                if field not in fm:
                    error(f"{rel_name}: missing vendoring field '{field}'")

        # filename matches actual file
        if fm.get("filename") and fm["filename"] != rel_name:
            error(f"{rel_name}: filename field '{fm['filename']}' != actual '{rel_name}'")

        # version is semver-like
        version = fm.get("version", "")
        if version and not SEMVER_RE.match(version):
            error(f"{rel_name}: version '{version}' is not valid semver (N.N.N)")

        # upstream_version matches version (canonical repo invariant; skip for repo-local)
        uv = fm.get("upstream_version", "")
        if version and uv and uv != version and pm != "repo-local":
            error(f"{rel_name}: upstream_version '{uv}' != version '{version}'")

        # provenance_mode is valid
        if pm and pm not in VALID_PROVENANCE_MODES:
            error(f"{rel_name}: invalid provenance_mode '{pm}'")

        if version:
            skill_versions[str(path.relative_to(REPO_ROOT))] = version

        if pm != "repo-local":
            skill_vendoring[str(path.relative_to(REPO_ROOT))] = {
                "vendored_at": fm.get("vendored_at", "").strip('"\''),
                "provenance_mode": pm,
                "upstream_repo": fm.get("upstream_repo", "").strip('"\''),
            }

    return skill_versions, skill_vendoring


# ── B. Partial validation ────────────────────────────────────────────────

def test_partials():
    print("\n═══ B. Partial validation ═══")
    partial_files = sorted(
        path for path in PARTIALS_DIR.glob("*.md")
        if path.name != "SKILL.md" and not path.name.endswith("-VERSIONING.md")
    )
    if not partial_files:
        error("No partial files found in .github/skills/_partials/")
        return

    print(f"  Found {len(partial_files)} partial files")

    for path in partial_files:
        basename = path.name
        with open(path) as f:
            content = f.read()

        m = re.search(r"partial-version:\s*([\d.]+)", content)
        if m:
            version = m.group(1)
            if not SEMVER_RE.match(version):
                error(f"{basename}: partial-version '{version}' is not valid semver")
        else:
            warn(f"{basename}: no partial-version comment found (defaulting to 1.0.0)")


# ── C. Manifest validation ──────────────────────────────────────────────

def test_manifest(skill_versions, skill_vendoring):
    print("\n═══ C. Manifest validation ═══")

    if not MANIFEST_PATH.exists():
        error("VENDOR_MANIFEST.json not found")
        return None

    try:
        with open(MANIFEST_PATH) as f:
            manifest = json.load(f)
    except json.JSONDecodeError as e:
        error(f"VENDOR_MANIFEST.json is not valid JSON: {e}")
        return None

    # Required top-level fields
    for field in ("upstream_repo", "manifest_version", "files"):
        if field not in manifest:
            error(f"VENDOR_MANIFEST.json: missing top-level field '{field}'")

    manifest_upstream_repo = manifest.get("upstream_repo", "")
    files = manifest.get("files", [])
    print(f"  Manifest contains {len(files)} file entries")

    manifest_paths = set()
    for entry in files:
        path = entry.get("path", "")
        provenance = entry.get("provenance", "")
        manifest_paths.add(path)

        # Provenance is valid enum
        if provenance not in VALID_PROVENANCE_MODES:
            error(f"VENDOR_MANIFEST.json: invalid provenance '{provenance}' for {path}")

        # vendored-exact files must exist on disk
        if provenance == "vendored-exact":
            full_path = REPO_ROOT / path
            if not full_path.exists():
                error(f"VENDOR_MANIFEST.json: vendored-exact file missing on disk: {path}")

        # seed-local files should exist on disk (warning only)
        if provenance == "seed-local":
            full_path = REPO_ROOT / path
            if not full_path.exists():
                warn(f"VENDOR_MANIFEST.json: seed-local file missing on disk: {path}")

        # Version consistency: vendored-exact skills' manifest version matches frontmatter
        if provenance == "vendored-exact" and path in skill_versions:
            manifest_version = entry.get("upstream_version")
            fm_version = skill_versions[path]
            if manifest_version and manifest_version != fm_version:
                error(f"VENDOR_MANIFEST.json: version mismatch for {path}: "
                      f"manifest={manifest_version}, frontmatter={fm_version}")

        # Full vendoring-field consistency: vendored-exact skills' frontmatter must
        # match the manifest on vendored_at, provenance_mode, and upstream_repo too —
        # not just upstream_version. Closes the validator blind spot from #338.
        if provenance == "vendored-exact" and path in skill_vendoring:
            fm_vendoring = skill_vendoring[path]

            manifest_vendored_at = entry.get("vendored_at")
            fm_vendored_at = fm_vendoring.get("vendored_at")
            if manifest_vendored_at and fm_vendored_at and manifest_vendored_at != fm_vendored_at:
                error(f"VENDOR_MANIFEST.json: vendored_at mismatch for {path}: "
                      f"manifest={manifest_vendored_at}, frontmatter={fm_vendored_at}")

            fm_provenance_mode = fm_vendoring.get("provenance_mode")
            if fm_provenance_mode and provenance != fm_provenance_mode:
                error(f"VENDOR_MANIFEST.json: provenance_mode mismatch for {path}: "
                      f"manifest={provenance}, frontmatter={fm_provenance_mode}")

            fm_upstream_repo = fm_vendoring.get("upstream_repo")
            if manifest_upstream_repo and fm_upstream_repo and manifest_upstream_repo != fm_upstream_repo:
                error(f"VENDOR_MANIFEST.json: upstream_repo mismatch for {path}: "
                      f"manifest={manifest_upstream_repo}, frontmatter={fm_upstream_repo}")

    # Every skill on disk should be in manifest
    for path in sorted(
        p for p in SKILLS_DIR.glob("*/SKILL.md")
        if str(p.relative_to(REPO_ROOT)) != ".github/skills/_partials/SKILL.md"
    ):
        rel = str(path.relative_to(REPO_ROOT))
        if rel not in manifest_paths:
            error(f"Skill on disk not in manifest: {rel}")

    # Every partial on disk should be in manifest
    for path in sorted(
        p for p in PARTIALS_DIR.glob("*.md")
        if p.name != "SKILL.md" and not p.name.endswith("-VERSIONING.md")
    ):
        rel = str(path.relative_to(REPO_ROOT))
        if rel not in manifest_paths:
            error(f"Partial on disk not in manifest: {rel}")

    return manifest


# ── D. Canonical clean-path proof ────────────────────────────────────────

def test_canonical_clean_path(manifest):
    print("\n═══ D. Canonical clean-path proof (self-audit) ═══")

    if manifest is None:
        error("Cannot run clean-path proof without valid manifest")
        return

    files = manifest.get("files", [])
    blockers = 0
    drift_warnings = 0

    for entry in files:
        path = entry.get("path", "")
        provenance = entry.get("provenance", "")
        full_path = REPO_ROOT / path

        # Simulate audit logic
        if not full_path.exists():
            if provenance == "vendored-exact":
                error(f"AUDIT BLOCKER: vendored-exact file missing: {path}")
                blockers += 1
            elif provenance == "seed-local":
                warn(f"AUDIT WARNING: seed-local file missing: {path}")

    if blockers == 0:
        print("  ✅ Canonical repo self-audit: CLEAN (no blockers)")
    else:
        print(f"  ❌ Canonical repo self-audit: {blockers} BLOCKER(s)")


# ── Main ─────────────────────────────────────────────────────────────────

def main():
    print("=" * 60)
    print("Meta-test suite: skill frontmatter + VENDOR_MANIFEST.json")
    print("=" * 60)

    skill_versions, skill_vendoring = test_skills()
    test_partials()
    manifest = test_manifest(skill_versions, skill_vendoring)
    test_canonical_clean_path(manifest)

    print("\n" + "=" * 60)
    print(f"Results: {len(errors)} error(s), {len(warnings)} warning(s)")
    print("=" * 60)

    if errors:
        print("\nErrors:")
        for e in errors:
            print(f"  ✗ {e}")

    if warnings:
        print("\nWarnings:")
        for w in warnings:
            print(f"  ⚠ {w}")

    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
