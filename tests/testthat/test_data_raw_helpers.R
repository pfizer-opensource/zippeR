# fixture data (no network calls) ---------------------------------------------
# NOTE: this file intentionally sources data-raw/helpers.R (which contains an
# explanatory dplyr reference in comments, not functional usage) to exercise
# bind_rows_base() offline, per #143. data-raw/ scripts remain excluded from
# the package's normal load path; this file sources only the pure-helper
# file, never the network-dependent build_uds_crosswalk.R script body.

source(testthat::test_path("..", "..", "data-raw", "helpers.R"))

x_simple <- data.frame(
  id = c(1, 2),
  a = c("x1", "x2"),
  stringsAsFactors = FALSE
)

y_simple <- data.frame(
  id = c(3, 4),
  a = c("y1", "y2"),
  stringsAsFactors = FALSE
)

x_extra_col <- data.frame(
  id = c(1, 2),
  a = c("x1", "x2"),
  only_in_x = c("ox1", "ox2"),
  stringsAsFactors = FALSE
)

y_extra_col <- data.frame(
  id = c(3, 4),
  a = c("y1", "y2"),
  only_in_y = c("oy1", "oy2"),
  stringsAsFactors = FALSE
)

# tests ------------------------------------------------------------------------
test_that("bind_rows_base() row-binds frames with identical schemas", {
  out <- bind_rows_base(x_simple, y_simple)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4)
  expect_equal(names(out), c("id", "a"))
  expect_equal(out$id, c(1, 2, 3, 4))
  expect_equal(out$a, c("x1", "x2", "y1", "y2"))
})

test_that("bind_rows_base() unions columns and NA-fills the missing side (matching dplyr::bind_rows())", {
  out <- bind_rows_base(x_extra_col, y_simple)

  expect_equal(sort(names(out)), sort(c("id", "a", "only_in_x")))
  # column order: x's columns first, then y-only columns appended
  expect_equal(names(out), c("id", "a", "only_in_x"))
  # rows from x retain their value; rows from y are NA-filled
  expect_equal(out$only_in_x, c("ox1", "ox2", NA, NA))
})

test_that("bind_rows_base() NA-fills columns unique to y for rows from x", {
  out <- bind_rows_base(x_simple, y_extra_col)

  expect_equal(names(out), c("id", "a", "only_in_y"))
  expect_equal(out$only_in_y, c(NA, NA, "oy1", "oy2"))
})

test_that("bind_rows_base() unions columns from both sides simultaneously", {
  out <- bind_rows_base(x_extra_col, y_extra_col)

  expect_equal(names(out), c("id", "a", "only_in_x", "only_in_y"))
  expect_equal(out$only_in_x, c("ox1", "ox2", NA, NA))
  expect_equal(out$only_in_y, c(NA, NA, "oy1", "oy2"))
})

test_that("bind_rows_base() preserves row order: all of x's rows first, then all of y's rows", {
  out <- bind_rows_base(x_simple, y_simple)

  expect_equal(rownames(out), as.character(seq_len(nrow(out))))
  expect_equal(out$id, c(x_simple$id, y_simple$id))
})
