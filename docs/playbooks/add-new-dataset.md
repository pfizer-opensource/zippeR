---
last_updated: 2026-04-27
last_updated_by: github-copilot-cli
owner_skill: playbooks/SKILL.md
quadrant: tutorials
---

# Add a new dataset

Worked example: take a new public open-data source from "we have a slug" all the way to "first release artifact tracked under `aggregations/`."

## Goal / starting state

**Goal**: ship a new self-contained dataset package at `datasets/<slug>/` whose first build produces gitignored per-table parquets, passes QC, registers in `data/dataset-registry.json`, and (if it feeds an aggregation) contributes to a tracked `aggregations/<slug>/data/*.parquet` release artifact.

**Starting state**:

- A slug is chosen (`acs`, `svi`, etc. — short kebab-case; see [`docs/GLOSSARY.md`](../GLOSSARY.md#slug)).
- An issue is filed via [`backlog/SKILL.md`](../../.github/skills/backlog/SKILL.md) describing the data source (URL pattern, expected tables, vintage cadence).
- An implementation plan exists on the issue per [`implementation_plan/SKILL.md`](../../.github/skills/implementation_plan/SKILL.md) (Z1) and the issue is labeled `in-progress`.

## Steps

### 1. Scaffold the dataset package

Invoke [`new_dataset/SKILL.md`](../../.github/skills/new_dataset/SKILL.md) with the chosen slug.

The skill calls `source/create_dataset_skeleton.R` which lays down `datasets/<slug>/source/{specs,workflow,functions,skills}/`, `datasets/<slug>/data/` (gitignored), `datasets/<slug>/docs/{metadata,qc-reports,requirements}/`, and `datasets/<slug>/tests/`. The entry script `datasets/<slug>/source/<slug>.R` is created with the standard sourcing pattern.

**Verify**: `ls datasets/<slug>/source/` shows the four subdirectories; `git status` shows the new files unstaged.

### 2. Author the source + table specs

Edit the two spec files the entry script references:

- `source/specs/<slug>-sources.R` — declare each raw data source (URL pattern, cache key, vintage parser).
- `source/specs/<slug>-tables.R` — declare each output table (column types, regression keys, partition cols).

Refer to an existing dataset's specs (`datasets/uspopest/source/specs/uspopest-*.R`) as a template.

**Decision point**: **If the source data is highly nested or has irregular structure**, file a discovery spike via `discovery/SKILL.md` (PR-3b) before continuing — author specs after the spike artifact lands. **Otherwise**: continue with the specs derived from the source documentation.

### 3. Author the workflow steps

Add numbered step files under `datasets/<slug>/source/workflow/<slug>-NN-*.R`. Each step is a function wrapped via `with_step()` from `source/with_step.R`. Typical steps: download, parse, transform, write outputs, regression check, render docs.

The entry script picks step files up by sort order, so number them with leading zeros (`-01-`, `-02-`, etc.).

### 4. First build

Invoke [`invoke_dataset/SKILL.md`](../../.github/skills/invoke_dataset/SKILL.md) with the slug.

Under the hood: `Rscript datasets/<slug>/source/<slug>.R`. This sources `source/`, then the specs, then runs the workflow steps in order via `with_step()`. Output parquets land under `datasets/<slug>/data/` (gitignored).

**Decision point**: **If the build fails on a transient network or upstream-source issue**, retry once after confirming the source URL is current. **If it fails on a structural issue** (column rename, schema drift, parser bug), fix the spec or workflow step and rerun. **Otherwise** (success): proceed to QC.

**Verify**: `ls datasets/<slug>/data/` shows one parquet per declared output table; row counts match expectations from the spec.

### 5. Author and run dataset QC

Invoke [`create_dataset_qc/SKILL.md`](../../.github/skills/create_dataset_qc/SKILL.md) with the slug to scaffold QC checks under `datasets/<slug>/source/skills/<slug>-qc.md` and the corresponding test files under `datasets/<slug>/tests/`.

Then invoke [`run_dataset_qc/SKILL.md`](../../.github/skills/run_dataset_qc/SKILL.md) to execute the QC checks against the just-built outputs.

**Verify**: a QC report appears under `datasets/<slug>/docs/qc-reports/<vintage>.md`; no BLOCKER findings remain unresolved.

### 6. Register the dataset

The first successful release run will append the slug to `data/dataset-registry.json` via the `register_dataset(slug)` helper. For now (pre-release) the slug is unregistered — confirm with `cat data/dataset-registry.json | jq '.datasets[].slug'`.

### 7. Release ceremony

When QC is clean and the operator has UAT-confirmed the outputs, invoke [`release_dataset/SKILL.md`](../../.github/skills/release_dataset/SKILL.md). This is the only sanctioned path for publishing a dataset; it gates on QC freshness, registers the slug, rebuilds aggregations under `aggregations/`, and writes the per-vintage release artifact set.

**Verify**:

- `data/dataset-registry.json` includes the new slug.
- `aggregations/<aggregation-slug>/data/*.parquet` is updated (these ARE tracked in git).
- `datasets/<slug>/docs/metadata/<vintage>.md` is rendered.

## Verification (full)

- [ ] `datasets/<slug>/source/` has the standard skeleton + populated specs + numbered workflow steps.
- [ ] `Rscript datasets/<slug>/source/<slug>.R` runs cleanly end-to-end.
- [ ] `tests/test-skills.R` and `datasets/<slug>/tests/test-<slug>.R` pass.
- [ ] QC report exists with no unresolved BLOCKERs.
- [ ] Slug registered in `data/dataset-registry.json`.
- [ ] Aggregation parquets refreshed (if dataset feeds an aggregation).
- [ ] Issue closed via [`backlog_retrospective/SKILL.md`](../../.github/skills/backlog_retrospective/SKILL.md) and PR opened via [`pull_request/SKILL.md`](../../.github/skills/pull_request/SKILL.md).

## Pointers

- **Spec source of truth**: [`docs/requirements/repo-scaling.md`](../requirements/repo-scaling.md) §6 acceptance criteria.
- **Helper API contract**: see [`docs/ARCHITECTURE.md`](../ARCHITECTURE.md) and [`docs/GLOSSARY.md`](../GLOSSARY.md) entries for `slug`, `family`, `kind`, `dataset_path`, `register_dataset`, `regression_check`.
- **Existing dataset reference**: `datasets/uspopest/` is the canonical example.
- **Lifecycle reference**: [`docs/WORKFLOW.md`](../WORKFLOW.md).

---

**Where this fits**: Tutorials quadrant of the docs hub. Curated by [`playbooks/SKILL.md`](../../.github/skills/playbooks/SKILL.md). The composition shown here mirrors [`docs/ARCHITECTURE.md`](../ARCHITECTURE.md)'s "Producers and consumers" + "Major components" sections in operational form.
