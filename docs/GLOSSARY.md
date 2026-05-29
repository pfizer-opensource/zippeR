---
last_updated: "2026-05-29"
---
# Glossary — zippeR

## Workflow Terms

Terms shared across all EVGen repos. See vendored ADRs for authoritative definitions.

| Term | Definition |
|---|---|
| Skill | A markdown-encoded workflow instruction consumed by AI agents |
| ADR | Architecture Decision Record — immutable once accepted |
| Epic | A multi-issue body of work tracked on the roadmap |

## Domain Terms

### Geographic & Postal Concepts

| Term | Definition |
|---|---|
| ZIP Code | A 5-digit identifier assigned by USPS to mail delivery routes. Not a geographic area — has no official boundary polygon. |
| ZIP3 Code | The first 3 digits of a ZIP Code, corresponding to a Sectional Center Facility (SCF) service area. |
| ZCTA | ZIP Code Tabulation Area — a Census Bureau geographic polygon that approximates the area served by one or more ZIP Codes. Built from census blocks. |
| 3-digit ZCTA | An aggregated geographic region formed by dissolving all ZCTAs sharing the same first 3 digits. Used in healthcare for geographic de-identification. |
| GEOID | Geographic Identifier — the Census Bureau's unique code for a geographic entity (e.g., a 5-digit string for a ZCTA). |
| Crosswalk | A lookup table mapping between two identifier systems (e.g., ZIP Code → ZCTA, or ZIP Code → County FIPS). |
| FIPS Code | Federal Information Processing Standards code — numeric identifiers for states (2-digit) and counties (5-digit). |
| SCF | Sectional Center Facility — a USPS mail processing plant that handles mail for a group of ZIP Codes sharing the same first 3 digits. |

### Data Sources & Agencies

| Term | Definition |
|---|---|
| Census Bureau | U.S. Census Bureau — source of ZCTA boundaries, ACS demographic data, and TIGER/Line shapefiles. |
| USPS | United States Postal Service — assigns and maintains ZIP Codes. |
| HUD | U.S. Department of Housing and Urban Development — publishes quarterly ZIP-to-ZCTA/county crosswalk files. |
| UDS | Uniform Data System — health center crosswalk maintained by HRSA mapping ZIP Codes to ZCTAs. |
| ACS | American Community Survey — an ongoing Census Bureau survey providing demographic, social, economic, and housing data. Published in 1-year and 5-year estimates. |
| TIGER/Line | Topologically Integrated Geographic Encoding and Referencing — the Census Bureau's spatial data product containing boundary shapefiles. |

### R Ecosystem

| Term | Definition |
|---|---|
| tidycensus | R package providing an interface to the Census Bureau's API for retrieving ACS and decennial data. |
| tigris | R package for downloading TIGER/Line shapefiles (states, counties, ZCTAs, etc.) directly into R as `sf` objects. |
| sf | Simple Features — the R package and standard for representing spatial vector data (points, lines, polygons). |
| roxygen2 | R documentation system that generates `.Rd` man pages from inline comments in source files. |

### Package-Specific Terms

| Term | Definition |
|---|---|
| Vintage | A specific year of geographic or demographic data (e.g., 2022 ACS 5-year, 2023 TIGER/Line boundaries). |
| Intersect method | Spatial filtering that selects ZCTAs whose geometry overlaps a state boundary — may include ZCTAs spanning multiple states. |
| Centroid method | Spatial filtering that selects ZCTAs whose geometric center (centroid) falls within a state boundary — assigns each ZCTA to exactly one state. |
| Extensive variable | A variable that scales with population size and is summed during aggregation (e.g., total population). |
| Intensive variable | A variable that does not scale with size and requires weighted averaging during aggregation (e.g., median income). |
| sysdata.rda | Internal package data file containing pre-computed ZCTA-to-state lookup vectors for all supported vintages. |
