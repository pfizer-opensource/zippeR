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

# test group_summarise_base() ------------------------------------------------

test_that("group_summarise_base does not conflate a real NA with the literal string \"NA\"", {

  df <- data.frame(k = c(NA_character_, "NA", "A", "B"), v = c(1, 2, 3, 4), stringsAsFactors = FALSE)

  out <- group_summarise_base(df, "k", function(d) list(total = sum(d$v)))

  # four distinct groups must survive: "A", "B", the literal string "NA", and
  # the real NA - a string-label-based implementation can collapse the last
  # two into one group
  expect_equal(nrow(out), 4)
  expect_equal(out$total[out$k == "NA" & !is.na(out$k)], 2)
  expect_equal(out$total[is.na(out$k)], 1)

})

test_that("group_summarise_base sorts ascending to match dplyr's locale-independent (C-locale) order", {

  df <- data.frame(k = c("b", NA, "a", "Z", "z", "A", NA), v = 1:7, stringsAsFactors = FALSE)

  out <- group_summarise_base(df, "k", function(d) list(n = nrow(d)))

  # dplyr::group_by()+summarise() sorts ascending under C locale regardless
  # of session locale (upper before lower, byte order), with NA last
  expect_equal(out$k, c("A", "Z", "a", "b", "z", NA))
  expect_equal(out$n, c(1, 1, 1, 1, 1, 2))

})

test_that("group_summarise_base returns a typed zero-row data frame for empty input", {

  df <- data.frame(ZCTA3 = character(0), variable = character(0), value = numeric(0),
                    stringsAsFactors = FALSE)

  out <- group_summarise_base(df, c("ZCTA3", "variable"),
                               function(d) list(value = sum(d$value, na.rm = TRUE)))

  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("ZCTA3", "variable", "value"))
  expect_true(is.character(out$ZCTA3))
  expect_true(is.character(out$variable))
  expect_true(is.numeric(out$value))

})

test_that("group_summarise_base does not conflate NA and NaN in a numeric grouping column", {

  df <- data.frame(k = c(NA_real_, NaN, 1, 2), v = c(4, 6, 10, 20))

  out <- group_summarise_base(df, "k", function(d) list(total = sum(d$v)))

  # dplyr::group_by()+summarise() keeps NA and NaN as distinct group keys;
  # is.na() alone treats both as missing, so a naive implementation can
  # silently merge them into one group. dplyr also sorts NaN ahead of NA
  # (both after all non-missing values), so row order is checked exactly.
  expect_equal(nrow(out), 4)
  expect_equal(out$total, c(10, 20, 6, 4))
  expect_true(is.nan(out$k[3]))
  expect_true(is.na(out$k[4]) && !is.nan(out$k[4]))

})

test_that("group_summarise_base coalesces non-contiguous repeated NA/NaN into two groups, matching dplyr", {

  df <- data.frame(k = c(NA_real_, NaN, NA_real_, NaN), v = c(1, 2, 3, 4))

  out <- group_summarise_base(df, "k", function(d) list(total = sum(d$v)))

  # order()'s radix method ties NA and NaN (preserves input order rather than
  # sub-sorting by kind), so interleaved NA/NaN values would stay
  # non-contiguous - and therefore split into 4 singleton groups - unless the
  # sort explicitly disambiguates NaN from NA before run-boundary detection.
  # dplyr sorts NaN ahead of NA, so row order is checked exactly too.
  expect_equal(nrow(out), 2)
  expect_equal(out$total, c(6, 4))
  expect_true(is.nan(out$k[1]))
  expect_true(is.na(out$k[2]) && !is.nan(out$k[2]))

})

test_that("group_summarise_base supports multi-column grouping, matching dplyr", {

  df <- data.frame(
    g1 = c("A", "A", "B", "B", "A", NA),
    g2 = c(1, 1, 2, 2, NA, NA),
    v = 1:6,
    stringsAsFactors = FALSE
  )

  out <- group_summarise_base(df, c("g1", "g2"), function(d) list(total = sum(d$v)))

  expect_equal(out$g1, c("A", "A", "B", NA))
  expect_equal(out$g2, c(1, NA, 2, NA))
  expect_equal(out$total, c(3, 5, 7, 6))

})
