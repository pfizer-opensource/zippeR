# Dataset Requirements Template

Use this template to provide initial instructions for adding a new open
dataset to `evgen-open-data`. This template is the source of truth that
`create_dataset_skeleton()` (and `new_dataset/SKILL.md`) copies into
each new dataset at `datasets/<dataset-slug>/docs/requirements/<dataset-slug>.md`.
Fill it in and hand it to the implementer (human or AI). The goal is to
give enough information that someone can go from raw source → tidy,
documented, reproducible dataset under `datasets/<dataset-slug>/`
without needing to ask follow-up questions.

Anything marked **Required** must be filled in before work starts.
Anything marked *Optional* can be left blank or answered with "TBD".

---

## 1. Overview

- **Dataset name** (Required):
- **Short slug** (Required, kebab-case, used for file/folder names):
- **One-line description** (Required):
- **Proof-of-concept?** (Required, yes/no):
- **Requester / point of contact** *Optional*:

## 2. Purpose & Use Case

- **Why are we publishing this?** (Required)
- **Who are the intended consumers?** (Required — e.g. epidemiologists,
  internal modelers, external researchers)
- **Downstream use cases** *Optional* (e.g. denominators for incidence
  rates, covariate in a forecasting model)

## 3. Source

- **Publisher / authority** (Required):
- **Source URL(s)** (Required — link to landing page *and* direct file
  URL if stable):
- **Source file format** (Required — CSV, XLSX, API, etc.):
- **License / terms of use** (Required — and confirm redistribution is
  permitted):
- **Citation text** (Required):
- **Access method** (Required — direct download, API key, scraping,
  manual):
- **Update cadence** (Required — annual, quarterly, ad-hoc):
- **Vintage / release date of the slice being ingested** (Required):

## 4. Scope

- **Geographic coverage** (Required — e.g. US 50 states + DC, global,
  county-level):
- **Time coverage** (Required — e.g. 2010–2023 annual):
- **Population / universe** (Required — e.g. resident civilian
  population):
- **Explicit exclusions** *Optional* (e.g. territories, institutional
  population):

## 5. Data Model

Define the **tidy output** the implementer should produce. One row per
observation; one column per variable.

### 5.1 Grain

- **Unit of observation** (Required — e.g. "one row per state × year ×
  age group × sex"):
- **Primary key** (Required — list the columns that uniquely identify a
  row):

### 5.2 Columns

Fill in one row per output column. Add/remove rows as needed.

| Column name | Type | Units / Domain | Nullable? | Description | Derivation |
|-------------|------|----------------|-----------|-------------|------------|
|             |      |                |           |             |            |

**Type** = `string` / `integer` / `double` / `date` / `boolean` / `factor`.
**Derivation** = `source` (verbatim), `renamed from X`, `computed: …`,
or `joined from <other dataset>`.

### 5.3 Controlled vocabularies / code lists

List any enumerations used (state codes, age-group bins, sex categories,
race/ethnicity, etc.). Link to an authoritative reference where
possible.

- `…`:
- `…`:

### 5.4 Known joins / foreign keys *Optional*

Other datasets in this repo that this one should be joinable to, and on
what keys.

## 6. Transformations

Describe the steps from raw to tidy. Be explicit about anything that is
*not* a verbatim copy.

- **Reshape**: (e.g. pivot wide age-group columns to long)
- **Filtering**: (e.g. drop "All ages" aggregate rows)
- **Recoding**: (e.g. map FIPS → USPS abbreviation)
- **Derived columns**:
- **Aggregations**: (e.g. sum single-year ages into 5-year bins)
- **Rounding / precision rules**:
- **Unit conversions**:

## 7. Quality Checks

Acceptance criteria the output **must** satisfy. Implementers should
encode these as automated tests where feasible.

- [ ] Primary key is unique and non-null
- [ ] Row count matches expected (state: ~, value: )
- [ ] Totals reconcile to published source totals within tolerance
      (state tolerance, e.g. exact / ±1 / ±0.1%)
- [ ] No unexpected NA values in required columns
- [ ] Categorical columns only contain values from the documented
      vocabulary
- [ ] Numeric ranges are plausible (state bounds)
- [ ] *Add dataset-specific checks here*

## 8. Deliverables

Default expectations — override if different:

- `datasets/<slug>/data/data_raw/` — unmodified source files
  (gitignored via the `datasets/*/data/` pattern; document retrieval
  in the dataset README if files are too large to commit elsewhere)
- `datasets/<slug>/source/<slug>.R` — entry script that runs the full
  workflow (`source/specs/<slug>-sources.R`,
  `source/specs/<slug>-tables.R`, every step in
  `source/workflow/<slug>-NN-*.R`)
- `datasets/<slug>/data/<slug>-*.parquet` (and optional
  `datasets/<slug>/data/csv/<slug>-*.csv` mirror) — tidy outputs
- `datasets/<slug>/docs/metadata/<slug>.md` — generated data
  dictionary + provenance + usage notes + citation
- `datasets/<slug>/tests/<slug>-test.R` — `testthat` tests for the
  quality checks in §7

## 9. Out of Scope *Optional*

Call out anything a reasonable implementer might assume is in scope but
isn't.

## 10. Open Questions *Optional*

List ambiguities for the implementer to confirm before coding.

---

# Worked Example: US Population Estimates (First POC)

This section is a filled-in instance of the template above, for the
first POC dataset. Treat it as both an example of how to use the
template and as the actual spec for the POC.

## 1. Overview

- **Dataset name**: US Resident Population Estimates by State, Age, and
  Sex
- **Short slug**: `us-pop-est-state-age-sex`
- **One-line description**: Annual mid-year resident population
  estimates for the 50 US states + DC, broken down by single-year age
  and sex, from the US Census Bureau's Population Estimates Program
  (PEP).
- **Proof-of-concept?**: Yes — first dataset in the repo; establishes
  the ingest/transform/publish pattern.
- **Requester / point of contact**: `<fill in>`

## 2. Purpose & Use Case

- **Why**: Provide a clean, analysis-ready denominator table so EV-Gen
  analyses can compute rates (incidence, prevalence, utilization)
  without each analyst re-processing Census files.
- **Intended consumers**: Internal epidemiologists and modelers;
  secondarily external collaborators.
- **Downstream use cases**: Denominators for disease-rate calculations;
  age/sex standardization; covariates in state-level models.

## 3. Source

- **Publisher**: US Census Bureau, Population Estimates Program (PEP).
- **Source URL**:
  - Landing:
    https://www.census.gov/programs-surveys/popest/data/tables.html
  - Vintage 2023 state characteristics (direct):
    https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/state/asrh/sc-est2023-alldata6.csv
- **Source file format**: CSV (single file, ~50 MB).
- **License / terms of use**: US Government work, public domain; free
  to redistribute with attribution.
- **Citation**: "U.S. Census Bureau, Population Division. Annual State
  Resident Population Estimates by Single Year of Age and Sex: April 1,
  2020 to July 1, 2023 (SC-EST2023-ALLDATA6)."
- **Access method**: Direct HTTPS download; no API key.
- **Update cadence**: Annual (new vintage each June/July).
- **Vintage ingested for POC**: Vintage 2023 (reference dates
  2020-04-01 through 2023-07-01).

## 4. Scope

- **Geographic coverage**: 50 US states + DC (52 including national
  total row, which will be dropped — see §6).
- **Time coverage**: Reference years 2020–2023 (one estimate per
  July 1, plus the 2020-04-01 base).
- **Population**: Resident population (includes civilian + armed forces
  stationed in the area; excludes US territories).
- **Exclusions**: Puerto Rico and other territories; county-level
  detail (state is the finest geography for the POC).

## 5. Data Model

### 5.1 Grain

- **Unit of observation**: one row per `state × year × age × sex`.
- **Primary key**: (`state_fips`, `year`, `age`, `sex`).

### 5.2 Columns

| Column name   | Type    | Units / Domain                              | Nullable? | Description                                      | Derivation |
|---------------|---------|---------------------------------------------|-----------|--------------------------------------------------|------------|
| `state_fips`  | string  | 2-char FIPS, "01"–"56"                      | No        | State FIPS code, zero-padded                     | renamed from `STATE` |
| `state_usps`  | string  | 2-char USPS, e.g. "AL"                      | No        | State postal abbreviation                        | joined from FIPS→USPS lookup |
| `state_name`  | string  | e.g. "Alabama"                              | No        | State name                                       | source `NAME` |
| `year`        | integer | 2020–2023                                   | No        | Reference year (population as of July 1)         | derived from PEP `POPESTIMATE{YYYY}` columns |
| `age`         | integer | 0–85 (85 = "85 and over")                   | No        | Single year of age; top-coded at 85              | source `AGE`, drop `AGE = 999` aggregate |
| `sex`         | factor  | {"male","female"}                           | No        | Sex                                              | recoded from `SEX` (1→male, 2→female); drop SEX=0 total |
| `population`  | integer | persons                                     | No        | Estimated resident population                    | pivoted from `POPESTIMATE2020`…`POPESTIMATE2023` |
| `vintage`     | integer | e.g. 2023                                   | No        | PEP vintage the estimate came from               | constant per ingest |

### 5.3 Controlled vocabularies

- **`state_fips`**: US Census FIPS state codes
  (https://www.census.gov/library/reference/code-lists/ansi.html),
  restricted to 50 states + DC.
- **`sex`**: `"male"` | `"female"`. The source also provides a
  combined `SEX = 0` ("Total") row — **dropped** in the tidy output
  because it is derivable.
- **`age`**: integer 0–85 where 85 means "85 years and older".

### 5.4 Known joins

- Joinable to any other state-level dataset in this repo on
  `state_fips` (preferred) or `state_usps`.

## 6. Transformations

1. Download `sc-est2023-alldata6.csv` into
   `datasets/us-pop-est-state-age-sex/data/data_raw/` with the vintage
   year in the filename.
2. Read with explicit column types; keep `STATE` as character to
   preserve the leading zero.
3. Drop national aggregate: `SUMLEV == "040"` only (state-level);
   exclude `STATE == "00"`.
4. Drop `SEX == 0` (both-sex total) and `AGE == 999` (all-ages total) —
   these are derivable and would double-count.
5. Pivot `POPESTIMATE2020`, `POPESTIMATE2021`, `POPESTIMATE2022`,
   `POPESTIMATE2023` from wide to long into `year` + `population`.
   Ignore the `ESTIMATESBASE2020` column for the POC (it's an April 1
   base, not July 1 estimate).
6. Recode `SEX`: 1 → `"male"`, 2 → `"female"`.
7. Join FIPS → USPS abbreviation lookup to add `state_usps`.
8. Add constant `vintage = 2023`.
9. Sort by (`state_fips`, `year`, `age`, `sex`) and write to
   `datasets/us-pop-est-state-age-sex/data/us-pop-est-state-age-sex.parquet`
   (with an optional CSV mirror under `data/csv/`).

No unit conversions. No rounding (counts are already integers).

## 7. Quality Checks

- [ ] Primary key (`state_fips`, `year`, `age`, `sex`) is unique and
      non-null.
- [ ] Exactly 51 distinct `state_fips` values (50 states + DC).
- [ ] Exactly 4 distinct `year` values: 2020, 2021, 2022, 2023.
- [ ] Exactly 86 distinct `age` values (0–85).
- [ ] Exactly 2 distinct `sex` values.
- [ ] Expected row count: 51 × 4 × 86 × 2 = **35,088**.
- [ ] `population >= 0` for every row.
- [ ] For each (`state_fips`, `year`), the sum of `population` across
      age × sex matches the published state total from the PEP
      state-level totals file **exactly** (Census aggregates are
      internally consistent).
- [ ] National total (sum over all states) for `year = 2023` matches
      the PEP published US total within ±1 person (rounding tolerance
      for reassurance only; expect exact).
- [ ] No NA values in any required column.

## 8. Deliverables

- `datasets/us-pop-est-state-age-sex/data/data_raw/sc-est2023-alldata6.csv`
  (gitignored under `datasets/*/data/`; document retrieval in the
  dataset README if not redistributable).
- `datasets/us-pop-est-state-age-sex/source/us-pop-est-state-age-sex.R`
  — entry script: download → transform → write → run QC checks.
- `datasets/us-pop-est-state-age-sex/data/us-pop-est-state-age-sex.parquet`
  (and optional `data/csv/` mirror).
- `datasets/us-pop-est-state-age-sex/docs/metadata/us-pop-est-state-age-sex.md`
  — generated data dictionary, provenance, citation, example usage
  snippet.
- QC checks from §7 implemented as `testthat` tests under
  `datasets/us-pop-est-state-age-sex/tests/` and wired into the
  workflow's regression step.

## 9. Out of Scope (POC)

- County- or tract-level geography.
- Race/ethnicity breakdown (the source file has it; we are deferring
  to keep the POC minimal).
- Pre-2020 historical vintages.
- US territories (PR, GU, VI, AS, MP).
- A package/API wrapper — POC is flat files + a script.

## 10. Open Questions

- Confirm whether we want `.parquet` output in addition to `.csv`/`.rds`.
- Confirm the preferred location for the FIPS↔USPS lookup (vendored CSV
  in `data/` vs. pulled from an R package such as `tigris` /
  `tidycensus`).
- Confirm whether raw source files should be committed or fetched on
  demand.
