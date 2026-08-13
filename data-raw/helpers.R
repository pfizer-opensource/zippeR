# Shared, offline-sourceable helpers for data-raw/ build scripts.
#
# This file contains pure helper functions with no network or file I/O
# side effects, so it can be sourced directly by both build scripts
# (e.g. build_uds_crosswalk.R) and offline unit tests
# (tests/testthat/test_data_raw_helpers.R) without triggering any
# network-dependent script bodies.

# NOTE: comments below intentionally reference dplyr by name to document
# the exact behavior this Base R helper reproduces (Epic L, #129). These
# are explanatory references, not functional dplyr usage, and are out of
# scope for "dplyr reference removal" (see #141).
# bind_rows_base(): row-binds two data frames by column name (matching
# dplyr::bind_rows()'s contract), since not every year's schema includes
# every standard column. Missing columns are filled with NA for the
# frame that lacks them.
bind_rows_base <- function(x, y) {
  x_cols <- names(x)
  y_only_cols <- setdiff(names(y), x_cols)
  all_cols <- c(x_cols, y_only_cols)

  for (col in y_only_cols) {
    x[[col]] <- NA
  }
  for (col in x_cols) {
    if (!(col %in% names(y))) {
      y[[col]] <- NA
    }
  }

  out <- rbind(x[, all_cols, drop = FALSE], y[, all_cols, drop = FALSE])
  rownames(out) <- NULL
  out
}
