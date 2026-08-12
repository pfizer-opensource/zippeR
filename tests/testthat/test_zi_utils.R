# fixture data (no API calls) ------------------------------------------------

x_simple <- data.frame(
  id = c(1, 2, NA, 3),
  val = c("a", "b", "c", "d"),
  stringsAsFactors = FALSE
)

y_simple <- data.frame(
  id = c(2, NA, 3, 3),
  extra = c("y2", "yna", "y3a", "y3b"),
  stringsAsFactors = FALSE
)

x_named <- data.frame(
  input_id = c(1, 2, 3),
  val = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

y_named <- data.frame(
  source_id = c(2, 3, 1),
  extra = c("y2", "y3", "y1"),
  stringsAsFactors = FALSE
)

x_composite <- data.frame(
  ZCTA3 = c("001", "001", "002", NA),
  GEOID = c("A", "B", "A", "C"),
  variable = c("v1", "v1", "v2", "v3"),
  value = c(10, 20, 30, 40),
  stringsAsFactors = FALSE
)

y_composite <- data.frame(
  ZCTA3 = c("001", "002", NA),
  GEOID = c("A", "A", "C"),
  weight = c(0.5, 0.6, 0.7),
  stringsAsFactors = FALSE
)

# test left_join_base() ------------------------------------------------

test_that("left_join_base preserves x row order and column order", {

  out <- left_join_base(x_simple, y_simple, by = "id")

  expect_equal(names(out), c("id", "val", "extra"))
  expect_equal(nrow(out), 5)
  # row order: x's original row order preserved, with duplicate-key expansion
  expect_equal(out$id, c(1, 2, NA, 3, 3))
  expect_equal(out$val, c("a", "b", "c", "d", "d"))

})

test_that("left_join_base propagates NA keys as a match, matching dplyr semantics", {

  out <- left_join_base(x_simple, y_simple, by = "id")

  na_row <- out[is.na(out$id), ]
  expect_equal(na_row$extra, "yna")

})

test_that("left_join_base expands duplicate keys (one-to-many)", {

  out <- left_join_base(x_simple, y_simple, by = "id")

  dup_rows <- out[!is.na(out$id) & out$id == 3, ]
  expect_equal(nrow(dup_rows), 2)
  expect_setequal(dup_rows$extra, c("y3a", "y3b"))

})

test_that("left_join_base keeps unmatched left rows with NA fill", {

  out <- left_join_base(x_simple, y_simple, by = "id")

  unmatched <- out[out$id == 1 & !is.na(out$id), ]
  expect_true(is.na(unmatched$extra))

})

test_that("left_join_base supports differently-named join keys", {

  join_by <- stats::setNames("source_id", "input_id")
  out <- left_join_base(x_named, y_named, by = join_by)

  expect_equal(names(out), c("input_id", "val", "extra"))
  expect_equal(out$extra, c("y1", "y2", "y3"))

})

test_that("left_join_base supports composite (multi-column) keys with NA and duplicates", {

  out <- left_join_base(x_composite, y_composite, by = c("ZCTA3", "GEOID"))

  expect_equal(names(out), c("ZCTA3", "GEOID", "variable", "value", "weight"))
  expect_equal(nrow(out), 4)
  expect_equal(out$value, c(10, 20, 30, 40))
  expect_equal(out$weight, c(0.5, NA, 0.6, 0.7))

})

test_that("left_join_base disambiguates colliding non-key columns with .x/.y suffixes, matching dplyr", {

  x_collide <- data.frame(
    zip5 = c("001", "002"),
    source_zcta = c("A", "B"),
    stringsAsFactors = FALSE
  )
  y_collide <- data.frame(
    zip5 = c("001", "002"),
    source_zcta = c("X", "Y"),
    stringsAsFactors = FALSE
  )

  out <- left_join_base(x_collide, y_collide, by = stats::setNames("zip5", "zip5"))

  expect_equal(names(out), c("zip5", "source_zcta.x", "source_zcta.y"))
  expect_equal(out$source_zcta.x, c("A", "B"))
  expect_equal(out$source_zcta.y, c("X", "Y"))

})
