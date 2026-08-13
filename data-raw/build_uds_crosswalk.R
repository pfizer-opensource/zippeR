# Build UDS Crosswalk Bundled Data
#
# This script downloads UDS Mapper crosswalk CSVs (2009–2022) from
# chris-prener/uds-mapper, applies the same normalization used by
# zi_load_uds(), adds a `year` column, combines all years into a
# single tibble, and saves the result as inst/extdata/uds_crosswalk.rds
# using xz compression (~400 KB).
#
# Run this script whenever UDS data needs to be refreshed (e.g., when a
# new year becomes available in the upstream repository). A new package
# release is required to ship updated data to users.
#
# Provenance:
#   Source:  https://github.com/chris-prener/uds-mapper/tree/main/data
#   Years:   2009–2022
#   License: Derived from UDS Mapper data (www.udsmapper.org)
#   Retrieved: run Sys.time() below to record date of build

# Dependencies ---------------------------------------------------------------
library(readr)
library(stringr)
library(tibble)

# bind_rows_base() is defined in helpers.R so it can be sourced offline by
# tests/testthat/test_data_raw_helpers.R without triggering this script's
# network-dependent body (see #143).
source("data-raw/helpers.R")

# Configuration ---------------------------------------------------------------
BASE_URL  <- "https://raw.githubusercontent.com/chris-prener/uds-mapper/main/data"
UDS_YEARS <- 2009:2022
OUT_PATH  <- file.path("inst", "extdata", "uds_crosswalk.rds")

cat("Building UDS crosswalk data...\n")
cat("Build date:", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "\n")
cat("Years:", paste(range(UDS_YEARS), collapse = "-"), "\n\n")

# Helper: normalize a single year's CSV --------------------------------------
normalize_uds_year <- function(year) {
  url <- paste0(BASE_URL, "/uds_crosswalk_", year, ".csv")
  cat("  Downloading", year, "...")

  df <- tryCatch(
    readr::read_csv(url, col_types = readr::cols(), show_col_types = FALSE),
    error = function(e) {
      stop("Failed to download UDS crosswalk for year ", year, ": ", conditionMessage(e))
    }
  )

  # normalize non-standard column names (2015 uses a different schema)
  col_renames <- c(zcta_use = "zcta", cityname = "po_name",
                   stateabbr = "state", ziptype = "zip_type")
  for (old_name in names(col_renames)) {
    if (old_name %in% names(df) && !col_renames[[old_name]] %in% names(df)) {
      names(df)[names(df) == old_name] <- col_renames[[old_name]]
    }
  }

  # keep only the standard schema columns
  standard_cols <- c("zip", "po_name", "state", "zip_type", "zcta", "zip_join_type")
  df <- df[, intersect(names(df), standard_cols), drop = FALSE]

  # remove rows with no ZCTA (must happen before zero-padding so "N/A" is caught)
  if ("zcta" %in% names(df)) {
    df <- df[!(df$zcta %in% c("N/A", NA, "No ZCTA", "")), , drop = FALSE]
  }

  # zero-pad ZIP and ZCTA to 5 digits
  df$zip  <- stringr::str_pad(df$zip,  width = 5, side = "left", pad = "0")
  df$zcta <- stringr::str_pad(df$zcta, width = 5, side = "left", pad = "0")

  # remove military ZIPs
  if ("zip_type" %in% names(df)) {
    df <- df[!grepl("^M", df$zip_type), , drop = FALSE]
  }

  # title-case post office names
  if ("po_name" %in% names(df)) {
    df$po_name <- stringr::str_to_title(df$po_name)
  }

  # sort and add year tag
  df <- df[order(df$zip), , drop = FALSE]
  df$year <- as.integer(year)

  cat(" OK (", nrow(df), "rows )\n", sep = "")
  tibble::as_tibble(df)
}

# Build combined dataset -----------------------------------------------------
# bind_rows_base() (row-binds per-year frames by column name, matching
# dplyr::bind_rows()'s contract) now lives in helpers.R, sourced above,
# so it can be tested offline (see #143). See helpers.R for the NOTE on
# its intentional dplyr reference in comments (Epic L, #129 / #141).

all_years <- lapply(UDS_YEARS, normalize_uds_year)
crosswalk  <- Reduce(bind_rows_base, all_years)

cat("\nCombined rows:", nrow(crosswalk), "\n")
cat("Years present:", paste(sort(unique(crosswalk$year)), collapse = ", "), "\n")
cat("Columns:      ", paste(names(crosswalk), collapse = ", "), "\n\n")

# Save -----------------------------------------------------------------------
saveRDS(crosswalk, file = OUT_PATH, compress = "xz")

size_kb <- round(file.size(OUT_PATH) / 1024, 1)
cat("Saved to:     ", OUT_PATH, "\n")
cat("File size:    ", size_kb, "KB\n")

if (size_kb > 600) {
  warning("File is larger than expected (", size_kb, " KB). ",
          "Consider checking for unexpected data or additional columns.")
}

cat("\nDone. Commit inst/extdata/uds_crosswalk.rds with the package.\n")
