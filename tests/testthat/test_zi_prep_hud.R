# test zi_prep_hud ------------------------------------------------

# create test data ------------------------------------------------

mo_xwalk <- zi_mo_hud

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_prep_hud(mo_xwalk),
               "is required", fixed = TRUE)
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_prep_hud(mo_xwalk, by = "invalid"),
               "must be", fixed = TRUE)
  expect_error(zi_prep_hud(mo_xwalk, by = "residential", return_max = "yes"),
               "must be", fixed = TRUE)
  expect_error(zi_prep_hud(data.frame(a = 1), by = "residential"),
               "missing required columns", fixed = TRUE)
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  skip_on_cran()
  expect_no_error(zi_prep_hud(mo_xwalk, by = "residential"))
  expect_no_error(zi_prep_hud(mo_xwalk, by = "commercial"))
  expect_no_error(zi_prep_hud(mo_xwalk, by = "total"))
  expect_no_error(zi_prep_hud(mo_xwalk, by = "residential", return_max = FALSE))
})

# test outputs ------------------------------------------------

test_that("correctly specified functions produce expected classes", {
  skip_on_cran()
  result <- zi_prep_hud(mo_xwalk, by = "residential")
  expect_s3_class(result, "tbl_df")
  expect_true("zip5" %in% names(result))
  expect_true("geoid" %in% names(result))
  expect_true("state" %in% names(result))
  expect_true("ratio" %in% names(result))
})

test_that("return_max = TRUE returns one row per zip5-state combination", {
  skip_on_cran()
  result <- zi_prep_hud(mo_xwalk, by = "residential", return_max = TRUE)
  counts <- table(paste(result$zip5, result$state))
  expect_true(all(counts == 1))
})

test_that("return_max = FALSE includes max column", {
  skip_on_cran()
  result <- zi_prep_hud(mo_xwalk, by = "residential", return_max = FALSE)
  expect_true("max" %in% names(result))
})

test_that("grouped max is computed correctly when zip5-state keys contain NA", {
  # regression test - a Base R ave()-based grouping must treat NA keys the
  # same way dplyr::group_by() does (grouping NAs together), not as
  # per-row singleton groups (which would silently mark every row as max).
  # Uses wholly synthetic data, so no need to skip on CRAN.

  na_xwalk <- data.frame(
    ZIP = c(NA_character_, NA_character_, "63103"),
    GEOID = c("29001", "29003", "29510"),
    RES_RATIO = c(0.3, 0.7, 1),
    BUS_RATIO = c(0.3, 0.7, 1),
    OTH_RATIO = c(0.3, 0.7, 1),
    TOT_RATIO = c(0.3, 0.7, 1),
    CITY = c("A", "B", "SAINT LOUIS"),
    STATE = c("MO", "MO", "MO")
  )

  result_all <- zi_prep_hud(na_xwalk, by = "residential", return_max = FALSE)
  na_rows <- result_all[is.na(result_all$zip5), ]

  expect_equal(nrow(na_rows), 2)
  # only the row with the higher ratio (0.7) within the shared NA zip5/state
  # group should be flagged as the max - both should NOT be TRUE
  expect_equal(sum(na_rows$max), 1)
  expect_true(na_rows$max[na_rows$ratio == 0.7])

  # return_max = TRUE must collapse the same NA-keyed group to a single row
  # bearing the true max ratio (0.7, GEOID 29003) - not the lowest-GEOID row
  # from an unfixed implementation that treats every NA-keyed row as tied
  result_max <- zi_prep_hud(na_xwalk, by = "residential", return_max = TRUE)
  na_row_max <- result_max[is.na(result_max$zip5), ]

  expect_equal(nrow(na_row_max), 1)
  expect_equal(na_row_max$ratio, 0.7)
  expect_equal(na_row_max$geoid, "29003")
})
