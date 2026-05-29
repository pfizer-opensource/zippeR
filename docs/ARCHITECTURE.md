---
last_updated: "2026-05-29"
---
# Architecture — zippeR

## Overview

zippeR is an R package that provides a unified interface for working with
United States ZIP Codes, ZIP Code Tabulation Areas (ZCTAs), and 3-digit ZCTAs.
It handles the complexity of mapping between postal delivery zones (ZIP Codes)
and Census Bureau geographic units (ZCTAs), including crosswalking, demographic
retrieval, spatial operations, and aggregation to 3-digit regions.

## Key Abstractions

- **ZIP Code** — A 5-digit USPS postal delivery route identifier. Not a
  geographic area; has no official boundary.
- **ZCTA** — ZIP Code Tabulation Area. A Census Bureau polygon that
  approximates the geographic extent of one or more ZIP Codes.
- **3-digit ZCTA** — An aggregated region formed by dissolving ZCTAs that share
  the same first 3 digits. Used in US healthcare contexts for geographic
  de-identification.
- **Crosswalk** — A lookup table that maps ZIP Codes to ZCTAs (or counties),
  since the mapping is not 1:1. Sources include HUD and UDS.

## Package Structure

```
R/
├── zi_get_geometry.R       # Download & geoprocess ZCTA shapefiles
├── zi_get_demographics.R   # Retrieve ACS demographic data for ZCTAs
├── zi_aggregate.R          # Aggregate ZCTA data to 3-digit ZCTAs
├── zi_load_crosswalk.R     # Load ZIP-to-ZCTA/county crosswalk files
├── zi_crosswalk.R          # Apply a crosswalk to user data
├── zi_convert.R            # Convert 5-digit ZIPs to 3-digit ZIPs
├── zi_load_labels.R        # Load city/area labels for ZIP codes
├── zi_load_labels_list.R   # List available label vintages
├── zi_label.R              # Append label data to a data frame
├── zi_list_zctas.R         # List ZCTA GEOIDs for a state
├── zi_validate.R           # Validate ZIP/ZCTA input vectors
├── zi_prep_hud.R           # Prepare HUD crosswalk data
├── zi_utils.R              # Internal utilities
├── zi_globals.R            # Global variable declarations
├── sysdata.rda             # Internal data (ZCTA vectors, reference tables)
└── zi_mo_*.R               # Example dataset documentation
```

## Data Flow

The typical user workflow follows one of two paths:

### Path 1: ZIP Code → ZCTA → Demographics

```
User ZIP data
  → zi_validate()          validate input
  → zi_crosswalk()         map ZIPs to ZCTAs via HUD/UDS crosswalk
  → zi_get_demographics()  pull ACS data for matched ZCTAs
  → zi_aggregate()         (optional) roll up to 3-digit ZCTAs
```

### Path 2: Spatial Analysis

```
zi_get_geometry()          download ZCTA shapefiles (state-filtered)
  → zi_get_demographics()  join demographic data
  → zi_aggregate()         (optional) aggregate to 3-digit regions
```

### Labeling (auxiliary)

```
zi_load_labels()           load city/area names for ZIP codes
  → zi_label()             append labels to user data
```

## Internal Data (`R/sysdata.rda`)

The package ships pre-computed lookup vectors that map ZCTAs to states for every
Census vintage (2010–2023). These are built by `inst/build-data/build_vectors.R`
using two spatial methods:

- **Intersects** — ZCTAs that geometrically overlap a state boundary
- **Centroids** — ZCTAs whose centroid falls within a state

A reference table tracks which vintage to use for each state×year combination,
accounting for year-over-year ZCTA boundary changes.

## External Dependencies

| Package | Role |
|---------|------|
| `tidycensus` | Census API access (ACS demographics) |
| `tigris` | TIGER/Line shapefiles (ZCTA & state boundaries) |
| `sf` | Spatial operations (intersection, centroid, transform) |
| `dplyr` | Data manipulation |
| `readr` | CSV/TSV parsing (crosswalk files) |
| `rlang` | Tidy evaluation, error signaling |
| `utils` | Base R utilities (download, unzip) |

## Design Decisions

- **No bundled shapefiles** — ZCTA geometries are downloaded on demand via
  `tigris` to keep package size small and data current.
- **Pre-computed state vectors** — Spatial intersection is expensive; results
  are cached in `sysdata.rda` so `zi_get_geometry(state=)` is fast.
- **Multiple crosswalk sources** — HUD and UDS crosswalks serve different use
  cases; the package supports both plus user-supplied custom dictionaries.
- **Vintage-aware** — All functions accept a `year` parameter and route to the
  correct underlying data/API vintage.
