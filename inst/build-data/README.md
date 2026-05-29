# Data Build Scripts

This directory contains scripts that regenerate the package's internal and
example datasets. A new maintainer should read this file before attempting to
refresh any bundled data.

## Scripts

### `build_vectors.R`

Builds the internal system data (`R/sysdata.rda`) used by `zi_get_geometry()`
for state-level ZCTA lookups. This is the most resource-intensive script and
must be run first.

**What it does:**

1. Downloads US state boundaries from the Census Bureau (via `tigris::states()`).
2. Downloads ZCTA shapefiles for every available year (2010, 2012–2023) from the
   Census Bureau (via `tigris::zctas()`).
3. Intersects ZCTAs with state boundaries using two methods (geometric
   intersection and centroid containment) to produce per-state ZCTA vectors for
   each year.
4. Compares year-over-year changes and builds a reference lookup table.
5. Stores ZCTA3 GeoJSON URLs (sourced from `chris-prener/zcta3` on GitHub).
6. Saves all results to `R/sysdata.rda`.

**Outputs:**

| File | Description |
|------|-------------|
| `R/sysdata.rda` | Internal package data (ZCTA vectors, reference tables, URLs) |
| `inst/data-raw/zcta<YYYY>.rda` | Cached intersect results per year |
| `inst/data-raw/zcta<YYYY>_centroid.rda` | Cached centroid results per year |

### `build_sample.R`

Builds the example datasets shipped with the package (used in documentation and
tests). Depends on functions defined in the package itself, so must be run after
`build_vectors.R`.

**What it does:**

1. Loads the in-development package (`devtools::load_all()`).
2. Calls `zi_get_geometry()` to fetch Missouri ZCTA3 geometry for 2022.
3. Calls `zi_get_demographics()` to fetch population/income data from the Census
   API (ACS 5-year, 2022).
4. Calls `zi_load_crosswalk()` to fetch the HUD ZIP-to-County crosswalk (2023 Q1,
   Missouri).
5. Calls `zi_load_labels()` to fetch USPS 3-digit ZIP labels (vintage 202408,
   filtered to Missouri).
6. Calls `zi_aggregate()` to produce an aggregated result for test validation.
7. Saves all example datasets to `data/` and test fixtures to `inst/testdata/`.

**Outputs:**

| File | Description |
|------|-------------|
| `data/zi_mo_zcta3.rda` | Missouri ZCTA3 geometry (example data) |
| `data/zi_mo_pop.rda` | Missouri population/income by ZCTA (example data) |
| `data/zi_mo_hud.rda` | Missouri HUD ZIP-County crosswalk (example data) |
| `data/zi_mo_usps.rda` | Missouri USPS 3-digit ZIP labels (example data) |
| `inst/testdata/zi_mo_pop_result.rda` | Aggregation result (test fixture) |

## Execution Order

Scripts must be run in this order:

1. **`build_vectors.R`** — builds internal system data
2. **`build_sample.R`** — builds example/test data (requires a working package)

## Prerequisites

### R Packages

| Package | Purpose |
|---------|---------|
| `devtools` | Load in-development package (`build_sample.R`) |
| `usethis` | Interactive prompts (`build_vectors.R`) |
| `dplyr` | Data manipulation |
| `tigris` | Download Census TIGER/Line shapefiles |
| `sf` | Spatial operations (intersection, centroid, transform) |
| `purrr` | Functional iteration |
| `tidycensus` | Census API access (`build_sample.R`) |

### API Keys and Credentials

- **Census API key** — required by `tidycensus` for `zi_get_demographics()` in
  `build_sample.R`. Register at <https://api.census.gov/data/key_signup.html>
  and set via `tidycensus::census_api_key()`.
- **No key required** for `tigris` downloads or HUD crosswalk access.

### External Data Sources

| Source | Used by | URL |
|--------|---------|-----|
| Census TIGER/Line (ZCTAs, states) | `build_vectors.R` | <https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html> |
| Census ACS 5-year | `build_sample.R` | <https://www.census.gov/data/developers/data-sets/acs-5year.html> |
| HUD USPS ZIP Crosswalk | `build_sample.R` | <https://www.huduser.gov/portal/datasets/usps_crosswalk.html> |
| USPS 3-digit ZIP labels | `build_sample.R` | loaded via `zi_load_labels()` |
| ZCTA3 GeoJSON | `build_vectors.R` | <https://github.com/chris-prener/zcta3> |

## Notes

- `build_vectors.R` prompts interactively whether to use local ZCTA data
  (previously cached in `inst/data-raw/`) or to re-download from the Census
  Bureau. Downloading fresh data requires substantial time and bandwidth.
- The `inst/data-raw/` directory contains cached intermediate `.rda` files from
  `build_vectors.R`. These are not shipped to end users but are preserved for
  incremental rebuilds.
- All `.rda` outputs use `version = 2` format with `xz` compression for
  backward compatibility and small file size.
